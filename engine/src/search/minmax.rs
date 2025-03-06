use std::sync::{
    atomic::{AtomicBool, Ordering},
    Mutex, RwLock,
};

use rayon::iter::{ParallelBridge, ParallelIterator};
use smallvec::SmallVec;

use crate::{GameState, GameStateKeyWithHash, Move};

use super::{
    evaluate::{self, AtomicScore, Score},
    move_predictor::{self, MovePredictor},
    shared_table::{ScoreKind, Table, TableValue},
    SearchInterrupted,
};

pub struct MinmaxContext<'a> {
    /// Shared flag that is set when the search must stop
    pub stop: &'a AtomicBool,
    /// Shared transposition table
    pub table: &'a Table,
    /// Stateful move predictor
    pub move_predictor: MovePredictor,
    /// Search performance metrics
    pub statistics: MinmaxStatistics,
    /// Stack of previous game states, used to detect draws
    pub history: Vec<GameStateKeyWithHash>,
}

#[derive(Debug, Clone, Default)]
pub struct MinmaxStatistics {
    /// Number of nodes for which we explored the moves
    pub expanded_nodes: u64,
    /// Number of nodes for which we explored the moves in quiescent search
    pub expanded_nodes_quiescent: u64,
    /// Number of table hits where the cached evaluation matched the desired depth
    pub table_hits: u64,
    /// Number of successful table entry replacements
    pub table_rewrites: u64,
}

#[derive(Clone, Copy)]
pub struct MinmaxResult {
    /// Depth of the current evaluation
    pub depth: u16,
    /// Current evaluation
    pub score: Score,
    /// Best move so far. None can mean multiple things:
    /// - it's too early in the search and no move have been examined yet
    /// - the current position is a dead end so there is no possible move
    pub best: Option<Move>,
}

enum LookupResult {
    UpdateBounds { alpha: Score, beta: Score },
    Cutoff { score: Score },
}

struct MoveWithKey {
    mv: Move,
    /// Rough evaluation of the move to compare it to other moves
    key: i16,
}

/// Lookup a position in the table and take the opportunity to restrict the alpha and beta bounds.
/// Returns (alpha, beta, best_move).
fn lookup_table(
    key: &GameStateKeyWithHash,
    mut alpha: Score,
    mut beta: Score,
    desired_depth: u16,
    ctx: &mut MinmaxContext,
) -> (LookupResult, Option<Move>) {
    let table_move = match ctx.table.lookup(key) {
        Some(e) => {
            ctx.statistics.table_hits += 1;
            if e.depth == desired_depth {
                match e.score_kind {
                    ScoreKind::Exact => return (LookupResult::Cutoff { score: e.score }, e.best),
                    ScoreKind::AtLeast => alpha = alpha.max(e.score),
                    ScoreKind::AtMost => beta = beta.min(e.score),
                }
                if alpha >= beta {
                    return (LookupResult::Cutoff { score: e.score }, e.best);
                }
            }
            e.best
        }
        None => None,
    };
    (LookupResult::UpdateBounds { alpha, beta }, table_move)
}

/// Update a position's entry in the table
fn update_table(
    key: GameStateKeyWithHash,
    alpha: Score,
    beta: Score,
    depth: u16,
    score: Score,
    best: Option<Move>,
    ctx: &mut MinmaxContext,
) {
    let score_kind = if score <= alpha {
        ScoreKind::AtMost // Could not reach alpha, the exact score is unknown
    } else if score >= beta {
        ScoreKind::AtLeast // Cutoff occurred, the exact score is unknown
    } else {
        ScoreKind::Exact
    };
    let table_value = TableValue {
        depth,
        score,
        score_kind,
        best,
    };
    if ctx.table.update(key, table_value) {
        ctx.statistics.table_rewrites += 1;
    }
}

/// Probe the game state history for a threefold repeition or fiftymove draw
fn is_draw(
    key: &GameStateKeyWithHash,
    fiftymove_counter: u16,
    history: &[GameStateKeyWithHash],
) -> bool {
    if fiftymove_counter >= 100 {
        return true;
    }
    let mut repetitions = 1;
    let mut i = 2;
    while i <= fiftymove_counter as usize && i <= history.len() {
        if &history[history.len() - i] == key {
            repetitions += 1;
            if repetitions == 3 {
                return true;
            }
        }
        i += 2;
    }
    false
}

/// Evaluate a position with a parallel minmax using "principal variation split"
pub fn eval_minmax_pv_split(
    gs: &GameState,
    alpha: Score,
    beta: Score,
    depth: u16,
    ctx: &mut MinmaxContext,
    argmax: Option<&RwLock<MinmaxResult>>,
) -> Result<Score, SearchInterrupted> {
    if depth == 0 {
        return Ok(eval_quiescent(gs, Score::MIN, Score::MAX, ctx)?);
    }
    if ctx.stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Call this closure to report a potential score and best move.
    // This is only needed for the root node.
    let compare_update_argmax = |new_score, new_best| {
        if let Some(argmax) = argmax {
            let mut argmax = argmax.write().unwrap();
            if depth > argmax.depth || new_score > argmax.score {
                *argmax = MinmaxResult {
                    depth,
                    score: new_score,
                    best: new_best,
                };
            }
        }
    };

    // Test for a draw position
    let key = gs.key().hash();
    if is_draw(&key, gs.fiftymove_count, &ctx.history) {
        compare_update_argmax(Score::ZERO, None);
        return Ok(Score::ZERO);
    }

    // Lookup in the table
    let (alpha, beta, table_move) = match lookup_table(&key, alpha, beta, depth, ctx) {
        (LookupResult::UpdateBounds { alpha, beta }, table_move) => (alpha, beta, table_move),
        (LookupResult::Cutoff { score }, best_move) => {
            compare_update_argmax(score, best_move);
            return Ok(score);
        }
    };

    let mut score = Score::NEG_INF;
    let mut best = None;
    ctx.statistics.expanded_nodes += 1;
    ctx.history.push(key);
    let mut branches = Branches::new(table_move);

    // Search the first branch first, passing the parallel flag
    // This method is called PV-splitting
    while let Some(mv) = branches.next(|| generate_moves(gs)) {
        let Ok(next) = gs.make_move(mv) else {
            continue;
        };
        let branch_score = -eval_minmax_pv_split(&next, -beta, -alpha, depth - 1, ctx, None)?;
        best = Some(mv);
        score = branch_score;
        compare_update_argmax(score, best);
        break; // First branch exploration complete
    }

    // Search the remaining branches in parallel
    let shared_score = AtomicScore::new(score);
    let shared_score_best = Mutex::new((score, best));
    let shared_stat = Mutex::new(&mut ctx.statistics);
    std::iter::from_fn(|| branches.next(|| generate_moves(gs)))
        .par_bridge()
        .try_for_each(|mv| {
            let Ok(next) = gs.make_move(mv) else {
                return Ok(()); // Illegal move
            };

            // Read the shared score value that other threads might have updated
            let score = shared_score.load();
            if score >= beta {
                return Ok(()); // Cutoff
            }
            let mut ctx = MinmaxContext {
                stop: ctx.stop,
                table: ctx.table,
                move_predictor: MovePredictor::new(depth),
                statistics: MinmaxStatistics::default(),
                history: ctx.history.clone(),
            };
            let branch_score = -eval_minmax(&next, -beta, -alpha.max(score), depth - 1, &mut ctx)?;
            shared_score.fetch_max(branch_score);
            shared_stat.lock().unwrap().add(&ctx.statistics);

            // Lock score and best together to keep them in sync
            let mut score_best = shared_score_best.lock().unwrap();
            if branch_score > score_best.0 {
                score_best.1 = Some(mv);
                compare_update_argmax(branch_score, score_best.1);
            }
            score_best.0 = score_best.0.max(branch_score);
            Ok(())
        })?;
    (score, best) = shared_score_best.into_inner().unwrap();
    ctx.history.pop();

    // NEG_INF means that this position is a dead end, so either checkmate or stalemate
    if score == Score::NEG_INF {
        if gs.is_check() {
            score = Score::MIN // Checkmate
        } else {
            score = Score::ZERO // Stalemate
        }
        compare_update_argmax(score, None);
    }

    // Store the result in the table
    update_table(key, alpha, beta, depth, score, best, ctx);
    Ok(score)
}

/// Evaluate a position with a minmax search
fn eval_minmax(
    gs: &GameState,
    alpha: Score,
    beta: Score,
    depth: u16,
    ctx: &mut MinmaxContext,
) -> Result<Score, SearchInterrupted> {
    if depth == 0 {
        return Ok(eval_quiescent(gs, alpha, beta, ctx)?);
    }
    if ctx.stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Test for a draw position
    let key = gs.key().hash();
    if is_draw(&key, gs.fiftymove_count, &ctx.history) {
        return Ok(Score::ZERO);
    }

    // Lookup in the table
    let (alpha, beta, table_move) = match lookup_table(&key, alpha, beta, depth, ctx) {
        (LookupResult::UpdateBounds { alpha, beta }, table_move) => (alpha, beta, table_move),
        (LookupResult::Cutoff { score }, _) => return Ok(score),
    };

    // Pop and explore the branches, starting from the most promising
    let mut score = Score::NEG_INF;
    let mut best = None;
    ctx.statistics.expanded_nodes += 1;
    ctx.history.push(key);
    let mut branches = Branches::new(table_move);
    while let Some(mv) =
        branches.next(|| generate_moves_with_predictor(gs, &ctx.move_predictor, depth))
    {
        let Ok(next) = gs.make_move(mv) else {
            continue;
        };
        let branch_score = -eval_minmax(&next, -beta, -alpha.max(score), depth - 1, ctx)?;
        if branch_score > score {
            best = Some(mv);
            score = branch_score;
            if score >= beta {
                ctx.move_predictor.apply_cutoff_bonus(gs, mv, depth);
                break; // Cutoff
            }
        }
    }
    ctx.history.pop();

    // NEG_INF means that this position is a dead end, so either checkmate or stalemate
    if score == Score::NEG_INF {
        if gs.is_check() {
            score = Score::MIN // Checkmate
        } else {
            score = Score::ZERO // Stalemate
        }
    }

    // Store the result in the table
    update_table(key, alpha, beta, depth, score, best, ctx);
    Ok(score)
}

/// Evaluate a position by quiescent search
fn eval_quiescent(
    gs: &GameState,
    mut alpha: Score,
    beta: Score,
    ctx: &mut MinmaxContext,
) -> Result<Score, SearchInterrupted> {
    if ctx.stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // In quiescence search, don't forget to consider that we can stop capturing anytime!
    // (Actually not anytime, there might be forced captures, but rarely)
    let mut score = evaluate::eval(gs);
    alpha = alpha.max(score);
    if alpha >= beta {
        return Ok(score);
    }

    // Generate the captures and assign them a score
    ctx.statistics.expanded_nodes_quiescent += 1;
    let mut moves = generate_non_quiet_moves(gs);

    // Pop and explore the branches, starting from the most promising
    while let Some(mv) = take_highest_move(&mut moves) {
        let Ok(next_gs) = gs.make_move_quiescent(mv) else {
            continue; // Illegal move
        };
        let branch_score = -eval_quiescent(&next_gs, -beta, -alpha.max(score), ctx)?;
        if branch_score > score {
            score = branch_score;
            if score >= beta {
                break; // Cutoff
            }
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

fn generate_non_quiet_moves(gs: &GameState) -> SmallVec<[MoveWithKey; 16]> {
    let mut moves = SmallVec::new();
    gs.pseudo_legal_non_quiet_moves(|mv| moves.push(MoveWithKey { mv, key: 0 }));
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

impl MinmaxStatistics {
    pub fn add(&mut self, other: &MinmaxStatistics) {
        *self = MinmaxStatistics {
            expanded_nodes: self.expanded_nodes + other.expanded_nodes,
            expanded_nodes_quiescent: self.expanded_nodes_quiescent
                + other.expanded_nodes_quiescent,
            table_hits: self.table_hits + other.table_hits,
            table_rewrites: self.table_rewrites + other.table_rewrites,
        };
    }
}

impl Default for MinmaxResult {
    fn default() -> Self {
        MinmaxResult {
            depth: 1,
            score: Score::NEG_INF,
            best: None,
        }
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
