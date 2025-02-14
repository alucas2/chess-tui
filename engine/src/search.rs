use std::{
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, LazyLock, RwLock,
    },
    thread,
};

use smallvec::SmallVec;

use crate::{evaluate::Score, GameState, Move, ScoreInfo, Table, TableKey};

/// Handle to the search thread
pub struct Search {
    result: Arc<SearchResultCell>,
    stop: Arc<AtomicBool>,
    stats: Arc<RwLock<SearchStatistics>>,
    handle: Option<thread::JoinHandle<()>>,
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

/// Shared container for a search result
#[derive(Debug)]
struct SearchResultCell(AtomicU64);

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
        let result = Arc::new(SearchResultCell::new(SearchResult {
            depth: 1,
            score: Score::ZERO,
            best: None,
        }));
        let stop = Arc::new(AtomicBool::new(false));
        let stats = Arc::new(RwLock::new(SearchStatistics::default()));
        let handle = {
            let result = Arc::clone(&result);
            let stop = Arc::clone(&stop);
            let stats = Arc::clone(&stats);
            thread::spawn(move || {
                // Iterative deepening
                for depth in 1..=20 {
                    let mut new_stats = SearchStatistics::default();
                    let Ok(final_score) = eval_minmax(
                        &gs,
                        Score::MIN,
                        Score::MAX,
                        depth,
                        &stop,
                        &TABLE,
                        &mut new_stats,
                        Some(&result),
                    ) else {
                        break; // Search has been interrupted
                    };
                    *stats.write().unwrap() = new_stats;
                    if final_score == Score::MIN || final_score == Score::MAX {
                        break; // Stop deepening when a checkmate is found
                    }
                }
            })
        };
        Search {
            result,
            stop,
            stats,
            handle: Some(handle),
        }
    }

    pub fn status(&self) -> SearchStatus {
        let result = self.result.load();
        SearchStatus {
            thinking: self.handle.as_ref().is_some_and(|h| !h.is_finished()),
            depth: result.depth,
            score: match result.score {
                Score::MAX => ScoreInfo::Win(result.depth - 1),
                Score::MIN => ScoreInfo::Loose(result.depth - 1),
                Score(x) => ScoreInfo::Normal(x),
            },
            best: result.best,
            stats: self.stats.read().unwrap().clone(),
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

/// An iterator over the banches of a gamestate.
/// - the first call to `next` will return the table move if provided,
/// - the remaining calls to `next` will lazily generate and return the rest of the moves
struct BranchIterator<'a, A: smallvec::Array<Item = MoveWithKey>> {
    gs: &'a GameState,
    table_move: Option<Move>,
    table_move_taken: bool,
    generated_moves: Option<SmallVec<A>>,
}

/// Evaluate a position with a minmax search
fn eval_minmax(
    gs: &GameState,
    mut alpha: Score,
    beta: Score,
    depth: u16,
    stop: &AtomicBool,
    table: &Table<(), TableValue>,
    stats: &mut SearchStatistics,
    argmax: Option<&SearchResultCell>,
) -> Result<Score, SearchInterrupted> {
    if depth == 0 {
        return Ok(eval_quiescent(gs, alpha, beta, stop, stats)?);
    }
    if stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Lookup in the table
    let table_key = TableKey::new(gs, ());
    let table_move = match table.lookup(&table_key) {
        Some(e) if e.depth == depth => {
            stats.table_hits += 1;
            if let Some(argmax) = argmax {
                // Update the search result
                argmax.store(SearchResult {
                    depth,
                    score: e.score,
                    best: e.best,
                });
            }
            return Ok(e.score);
        }
        Some(e) => e.best,
        None => None,
    };

    let mut score = Score::NEG_INF;
    let mut best = None;

    // Pop and explore the branches, starting from the most promising
    stats.expanded_nodes += 1;
    let mut branches = BranchIterator::new(gs, table_move);
    while let Some((mv, next_gs)) = branches.next(generate_moves) {
        let branch_score =
            -eval_minmax(&next_gs, -beta, -alpha, depth - 1, stop, table, stats, None)?;
        if branch_score > score {
            best = Some(mv);
            if let Some(argmax) = argmax {
                // Update the search result
                argmax.store(SearchResult {
                    depth,
                    score: branch_score,
                    best,
                });
            }
        }
        score = score.max(branch_score);
        alpha = alpha.max(score);
        if alpha >= beta {
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
    if table.update(table_key, table_value) {
        stats.table_rewrites += 1;
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
    let mut moves = generate_moves_quiescent(gs);

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
        *key = gs.eval_move(*mv);
    }
    moves
}

fn generate_moves_quiescent(gs: &GameState) -> SmallVec<[MoveWithKey; 16]> {
    let mut moves = SmallVec::new();
    gs.pseudo_legal_captures(|mv| moves.push(MoveWithKey { mv, key: 0 }));
    for MoveWithKey { mv, key } in moves.iter_mut() {
        *key = gs.eval_move(*mv);
    }
    moves
}

fn take_highest_move<A: smallvec::Array<Item = MoveWithKey>>(
    moves: &mut SmallVec<A>,
) -> Option<Move> {
    let (index, _) = moves.iter().enumerate().max_by_key(|(_, mv)| mv.key)?;
    Some(moves.swap_remove(index).mv)
}

impl SearchResultCell {
    fn new(value: SearchResult) -> SearchResultCell {
        let value = unsafe { std::mem::transmute(value) };
        SearchResultCell(AtomicU64::new(value))
    }

    fn store(&self, value: SearchResult) {
        let value = unsafe { std::mem::transmute(value) };
        self.0.store(value, Ordering::Relaxed)
    }

    fn load(&self) -> SearchResult {
        // Safety: in the functions `new` and `store`, we guarantee that
        // the atomic contains a valid SearchResult
        let value = self.0.load(Ordering::Relaxed);
        unsafe { std::mem::transmute(value) }
    }
}

impl<'a, A: smallvec::Array<Item = MoveWithKey>> BranchIterator<'a, A> {
    pub fn new(gs: &'a GameState, table_move: Option<Move>) -> Self {
        BranchIterator {
            gs,
            table_move,
            table_move_taken: false,
            generated_moves: None,
        }
    }

    pub fn next<F: FnOnce(&GameState) -> SmallVec<A>>(
        &mut self,
        f: F,
    ) -> Option<(Move, GameState)> {
        match (self.table_move, self.table_move_taken) {
            (Some(mv), false) => {
                self.table_move_taken = true;
                let next_gs = self.gs.make_move(mv).expect("Table move should be legal");
                Some((mv, next_gs))
            }
            _ => {
                let generated_moves = self.generated_moves.get_or_insert_with(|| f(self.gs));
                loop {
                    let mv = take_highest_move(generated_moves)?;
                    if Some(mv) == self.table_move {
                        continue;
                    }
                    if let Ok(next_gs) = self.gs.make_move(mv) {
                        break Some((mv, next_gs));
                    }
                }
            }
        }
    }
}
