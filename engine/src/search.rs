use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, LazyLock, Mutex, RwLock,
    },
    thread, time,
};

use rayon::iter::{ParallelBridge, ParallelIterator};
use smallvec::SmallVec;

use crate::{
    evaluate::{AtomicScore, Score},
    move_predictor::{self, MovePredictor},
    GameState, Move, ScoreInfo, Table, TableKey,
};

/// Handle to the search thread
pub struct Search {
    result: Arc<RwLock<SearchResult>>,
    stop: Arc<AtomicBool>,
    stats: Arc<RwLock<SearchStatistics>>,
    handle: Option<thread::JoinHandle<()>>,
    start: time::Instant,
    finish: Arc<RwLock<Option<time::Instant>>>,
}

/// Status of the search that is updated by the search thread
#[derive(Clone)]
pub struct SearchStatus {
    /// Indicates if the search thread is still running
    pub thinking: bool,
    /// Depth of the search that gave the current result
    pub depth: u16,
    /// Score assigned to the searched position
    pub score: ScoreInfo,
    /// Best move from the search position. Is None if no moves are available
    /// (because the game is lost) or if it's too early in the search to know
    pub best: Option<Move>,
    /// Search performance metrics
    pub stats: SearchStatistics,
    /// Time elapsed
    pub elapsed: time::Duration,
}

#[derive(Debug, Clone, Default)]
pub struct SearchStatistics {
    /// Number of nodes for which we explored the moves
    pub expanded_nodes: u64,
    /// Number of nodes for which we explored the moves in quiescent search
    pub expanded_nodes_quiescent: u64,
    /// Number of table hits where the cached evaluation matched the desired depth
    pub table_hits: u64,
    /// Number of successful table entry replacements
    pub table_rewrites: u64,
}

/// Value that is updated during the search with the best move found so far
#[derive(Debug, Clone, Copy)]
struct SearchResult {
    depth: u16,
    score: Score,
    best: Option<Move>,
}

/// Values that are stored in the table.
/// Its size should not be excessive to fit within a table entry
#[derive(Clone, Copy)]
struct TableValue {
    depth: u16,
    score: Score,
    best: Option<Move>,
}

const TABLE_NUM_ENTRIES: usize = 2_usize.pow(22);
static TABLE: LazyLock<Table<(), TableValue>> = LazyLock::new(|| Table::new(TABLE_NUM_ENTRIES));

impl Search {
    pub fn start(gs: GameState) -> Self {
        let _ = rayon::ThreadPoolBuilder::new()
            .num_threads(2)
            .build_global();
        let result = Arc::new(RwLock::new(SearchResult {
            depth: 1,
            score: Score::NEG_INF,
            best: None,
        }));
        let stop = Arc::new(AtomicBool::new(false));
        let stats = Arc::new(RwLock::new(SearchStatistics::default()));
        let start = time::Instant::now();
        let finish = Arc::new(RwLock::new(None));
        let handle = {
            let result = Arc::clone(&result);
            let stop = Arc::clone(&stop);
            let stats = Arc::clone(&stats);
            let finish = Arc::clone(&finish);
            thread::spawn(move || {
                // Iterative deepening
                for depth in 1..=20 {
                    let mut new_stats = SearchStatistics::default();
                    let final_score = eval_minmax_pv_split(
                        &gs,
                        Score::MIN,
                        Score::MAX,
                        depth,
                        &stop,
                        &TABLE,
                        &mut new_stats,
                        Some(&result),
                    );
                    stats.write().unwrap().add(&new_stats);
                    let Ok(final_score) = final_score else {
                        break; // Search has been interrupted
                    };
                    if final_score == Score::MIN || final_score == Score::MAX {
                        result.write().unwrap().score = final_score;
                        break; // Stop deepening when a checkmate is found
                    }
                }
                // Signal the end of the search
                *finish.write().unwrap() = Some(time::Instant::now());
            })
        };
        Search {
            result,
            stop,
            stats,
            handle: Some(handle),
            start,
            finish,
        }
    }

    pub fn status(&self) -> SearchStatus {
        let result = self.result.read().unwrap().clone();
        let stats = self.stats.read().unwrap().clone();
        let (elapsed, thinking) = match *self.finish.read().unwrap() {
            Some(finish) => (finish - self.start, false),
            None => (self.start.elapsed(), true),
        };
        SearchStatus {
            thinking,
            depth: result.depth,
            score: match result.score {
                Score::MAX => ScoreInfo::Win(result.depth - 1),
                Score::MIN => ScoreInfo::Loose(result.depth - 1),
                Score(x) => ScoreInfo::Normal(x),
            },
            best: result.best,
            stats,
            elapsed,
        }
    }
}

impl Drop for Search {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        let _ = self.handle.take().unwrap().join();
    }
}

struct SearchInterrupted;

/// Stores a move and a rough evaluation of the move to compare it to other moves
struct MoveWithKey {
    mv: Move,
    key: i16,
}

fn eval_minmax_pv_split(
    gs: &GameState,
    mut alpha: Score,
    beta: Score,
    depth: u16,
    stop: &AtomicBool,
    tt: &Table<(), TableValue>,
    mut stat: &mut SearchStatistics,
    argmax: Option<&RwLock<SearchResult>>,
) -> Result<Score, SearchInterrupted> {
    if depth == 0 {
        return Ok(eval_quiescent(gs, alpha, beta, stop, stat)?);
    }
    if stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    let compare_update_argmax = |new_score, new_best| {
        if let Some(argmax) = argmax {
            let mut argmax = argmax.write().unwrap();
            if depth > argmax.depth || new_score > argmax.score {
                *argmax = SearchResult {
                    depth,
                    score: new_score,
                    best: new_best,
                };
            }
        }
    };

    // Lookup in the table
    let table_key = TableKey::new(gs, ());
    let table_move = match tt.lookup(&table_key) {
        Some(e) if e.depth == depth => {
            stat.table_hits += 1;
            compare_update_argmax(e.score, e.best);
            return Ok(e.score);
        }
        Some(e) => e.best,
        None => None,
    };

    let mut score = Score::NEG_INF;
    let mut best = None;

    stat.expanded_nodes += 1;
    let mut branches = Branches::new(table_move);
    'cutoff: {
        // Search the first branch first, passing the parallel flag
        // This method is called PV-splitting
        while let Some(mv) = branches.next(|| generate_moves(gs)) {
            let Ok(next) = gs.make_move(mv) else {
                continue;
            };
            let branch_score =
                -eval_minmax_pv_split(&next, -beta, -alpha, depth - 1, stop, tt, stat, None)?;
            best = Some(mv);
            compare_update_argmax(branch_score, best);
            score = score.max(branch_score);
            alpha = alpha.max(score);
            if alpha >= beta {
                break 'cutoff;
            }
            break; // First branch exploration complete
        }

        // Search the remaining branches in parallel
        let shared_alpha = AtomicScore::new(alpha);
        let shared_score_best = Mutex::new((score, best));
        let shared_stat = Mutex::new(stat);
        std::iter::from_fn(|| branches.next(|| generate_moves(gs)))
            .par_bridge()
            .try_for_each(|mv| {
                let Ok(next) = gs.make_move(mv) else {
                    return Ok(()); // Illegal move
                };

                // Read the shared alpha value that other threads might have updated
                let alpha = shared_alpha.load();
                if alpha >= beta {
                    return Ok(()); // Cutoff
                }
                let mut stat = SearchStatistics::default();
                let mut mp = MovePredictor::new(depth);
                let branch_score = -eval_minmax(
                    &next,
                    -beta,
                    -alpha,
                    depth - 1,
                    stop,
                    tt,
                    &mut mp,
                    &mut stat,
                )?;
                shared_stat.lock().unwrap().add(&stat);

                // Lock score and best together to keep them in sync
                let mut score_best = shared_score_best.lock().unwrap();
                if branch_score > score_best.0 {
                    score_best.1 = Some(mv);
                    compare_update_argmax(branch_score, score_best.1);
                }
                score_best.0 = score_best.0.max(branch_score);
                shared_alpha.fetch_max(score_best.0);
                Ok(())
            })?;
        (score, best) = shared_score_best.into_inner().unwrap();
        stat = shared_stat.into_inner().unwrap();
    }

    // NEG_INF means that this position is a dead end, so either checkmate or stalemate
    if score == Score::NEG_INF {
        if gs.is_check() {
            score = Score::MIN
        } else {
            score = Score::ZERO
        }
    }

    let table_value = TableValue { depth, score, best };
    if tt.update(table_key, table_value) {
        stat.table_rewrites += 1;
    }
    Ok(score)
}

/// Evaluate a position with a minmax search
fn eval_minmax(
    gs: &GameState,
    mut alpha: Score,
    beta: Score,
    depth: u16,
    stop: &AtomicBool,
    tt: &Table<(), TableValue>,
    mp: &mut MovePredictor,
    stat: &mut SearchStatistics,
) -> Result<Score, SearchInterrupted> {
    if depth == 0 {
        return Ok(eval_quiescent(gs, alpha, beta, stop, stat)?);
    }
    if stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Lookup in the table
    let table_key = TableKey::new(gs, ());
    let table_move = match tt.lookup(&table_key) {
        Some(e) if e.depth == depth => {
            stat.table_hits += 1;
            return Ok(e.score);
        }
        Some(e) => e.best,
        None => None,
    };

    let mut score = Score::NEG_INF;
    let mut best = None;

    // Pop and explore the branches, starting from the most promising
    stat.expanded_nodes += 1;
    let mut branches = Branches::new(table_move);
    while let Some(mv) = branches.next(|| generate_moves_with_predictor(gs, mp, depth)) {
        let Ok(next) = gs.make_move(mv) else {
            continue;
        };
        let branch_score = -eval_minmax(&next, -beta, -alpha, depth - 1, stop, tt, mp, stat)?;
        if branch_score > score {
            best = Some(mv);
        }
        score = score.max(branch_score);
        alpha = alpha.max(score);
        if alpha >= beta {
            mp.apply_cutoff_bonus(gs, mv, depth);
            break;
        }
    }

    // NEG_INF means that this position is a dead end, so either checkmate or stalemate
    if score == Score::NEG_INF {
        if gs.is_check() {
            score = Score::MIN
        } else {
            score = Score::ZERO
        }
    }

    let table_value = TableValue { depth, score, best };
    if tt.update(table_key, table_value) {
        stat.table_rewrites += 1;
    }
    Ok(score)
}

/// Evaluate a position by quiescent search
fn eval_quiescent(
    gs: &GameState,
    mut alpha: Score,
    beta: Score,
    stop: &AtomicBool,
    stats: &mut SearchStatistics,
) -> Result<Score, SearchInterrupted> {
    if stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // In quiescence search, don't forget to consider that we can stop capturing anytime!
    // (Actually not anytime, there might be forced captures, but rarely)
    let mut score = gs.evaluator.eval();
    alpha = alpha.max(score);
    if alpha >= beta {
        return Ok(score);
    }

    // Generate the captures and assign them a score
    stats.expanded_nodes_quiescent += 1;
    let mut moves = generate_captures(gs);

    // Pop and explore the branches, starting from the most promising
    while let Some(mv) = take_highest_move(&mut moves) {
        let Ok(next_gs) = gs.make_move(mv) else {
            continue; // Illegal move
        };
        let branch_score = -eval_quiescent(&next_gs, -beta, -alpha, stop, stats)?;
        score = score.max(branch_score);
        alpha = alpha.max(score);
        if alpha >= beta {
            break;
        }
    }

    Ok(score)
}

fn generate_moves(gs: &GameState) -> SmallVec<[MoveWithKey; 64]> {
    let mut moves = SmallVec::new();
    gs.pseudo_legal_moves(|mv| moves.push(MoveWithKey { mv, key: 0 }));
    for MoveWithKey { mv, key } in moves.iter_mut() {
        *key = move_predictor::eval(gs, *mv);
    }
    moves
}

fn generate_moves_with_predictor(
    gs: &GameState,
    mp: &MovePredictor,
    depth: u16,
) -> SmallVec<[MoveWithKey; 64]> {
    let mut moves = SmallVec::<[_; 64]>::new();
    gs.pseudo_legal_moves(|mv| moves.push(MoveWithKey { mv, key: 0 }));
    for MoveWithKey { mv, key } in moves.iter_mut() {
        *key = mp.eval(gs, *mv, depth);
    }
    moves
}

fn generate_captures(gs: &GameState) -> SmallVec<[MoveWithKey; 16]> {
    let mut moves = SmallVec::new();
    gs.pseudo_legal_captures(|mv| moves.push(MoveWithKey { mv, key: 0 }));
    for MoveWithKey { mv, key } in moves.iter_mut() {
        *key = move_predictor::eval(gs, *mv);
    }
    moves
}

fn take_highest_move<A: smallvec::Array<Item = MoveWithKey>>(
    moves: &mut SmallVec<A>,
) -> Option<Move> {
    let (index, _) = moves.iter().enumerate().max_by_key(|(_, mv)| mv.key)?;
    Some(moves.swap_remove(index).mv)
}

impl SearchStatistics {
    fn add(&mut self, other: &SearchStatistics) {
        *self = SearchStatistics {
            expanded_nodes: self.expanded_nodes + other.expanded_nodes,
            expanded_nodes_quiescent: self.expanded_nodes_quiescent
                + other.expanded_nodes_quiescent,
            table_hits: self.table_hits + other.table_hits,
            table_rewrites: self.table_rewrites + other.table_rewrites,
        };
    }
}

enum Branches<A: smallvec::Array<Item = MoveWithKey>> {
    TableMove(Option<Move>),
    RestUngenerated {
        table_mv: Option<Move>,
    },
    RestGenerated {
        table_mv: Option<Move>,
        rest: SmallVec<A>,
    },
}

impl<A: smallvec::Array<Item = MoveWithKey>> Branches<A> {
    pub fn new(table_move: Option<Move>) -> Self {
        Branches::TableMove(table_move)
    }

    pub fn next<F: FnOnce() -> SmallVec<A>>(&mut self, f: F) -> Option<Move> {
        match self {
            Branches::TableMove(table_move) => match table_move.take() {
                Some(mv) => {
                    *self = Branches::RestUngenerated { table_mv: Some(mv) };
                    Some(mv) // Yield the table move
                }
                None => {
                    *self = Branches::RestUngenerated { table_mv: None };
                    self.next(f)
                }
            },
            Branches::RestUngenerated { table_mv } => {
                let rest = f();
                let table_mv = *table_mv;
                *self = Branches::RestGenerated { table_mv, rest };
                self.next_rest()
            }
            Branches::RestGenerated { .. } => self.next_rest(),
        }
    }

    fn next_rest(&mut self) -> Option<Move> {
        match self {
            Branches::RestGenerated { table_mv, rest } => loop {
                let (index, _) = rest.iter().enumerate().max_by_key(|(_, mv)| mv.key)?;
                let mv = rest.swap_remove(index).mv;
                if Some(mv) != *table_mv {
                    break Some(mv); // Yield a generated move
                }
            },
            _ => unreachable!(),
        }
    }
}
