use std::sync::{
    atomic::{AtomicBool, Ordering},
    Mutex, RwLock,
};

use branch_iterator::Branch;
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

    // Explore the branches
    ctx.statistics.expanded_nodes += 1;
    ctx.history.push(key);
    let (score, best) = if let Some((Branch { mv, next }, rest)) =
        branch_iterator::first_and_rest(gs, table_move, &ctx.move_predictor, depth)
    {
        // Search the first branch first, passing the parallel flag
        // This method is called PV-splitting
        let mut best = mv;
        let mut score = -eval_minmax_pv_split(&next, -beta, -alpha, depth - 1, ctx, None)?;
        compare_update_argmax(score, Some(mv));
        if score >= beta {
            // Cutoff (no need to update the move predictor because we don't use it here)
        } else {
            // The remaining branches are explored in parallel
            let shared_score = AtomicScore::new(score);
            let shared_score_and_best = Mutex::new((score, best));
            let shared_stat = Mutex::new(&mut ctx.statistics);
            rest.get_or_generate(&ctx.move_predictor, depth)
                .par_bridge()
                .try_for_each(|Branch { mv, next }| {
                    let mut score = shared_score.load();
                    if score >= beta {
                        return Ok(()); // Cutoff (first opportunity)
                    }

                    // Explore with minimal bounds to test if it can beat the current score
                    let mut ctx = MinmaxContext {
                        stop: ctx.stop,
                        table: ctx.table,
                        move_predictor: MovePredictor::new(depth),
                        statistics: MinmaxStatistics::default(),
                        history: ctx.history.clone(),
                    };
                    let (lo, hi) = (-alpha.max(score)).minimal_window();
                    let temp = -eval_minmax(&next, lo, hi, depth - 1, &mut ctx)?;

                    // If the branch beats the current score, we might need to re-explore
                    score = shared_score.load();
                    if score >= beta {
                        return Ok(()); // Cutoff (second opportunity)
                    }
                    if temp > score {
                        score = if temp > alpha && temp < beta {
                            -eval_minmax(&next, -beta, -temp, depth - 1, &mut ctx)?
                        } else {
                            temp
                        };
                        shared_score.fetch_max(score);
                        let mut score_and_best = shared_score_and_best.lock().unwrap();
                        if score > score_and_best.0 {
                            score_and_best.0 = score;
                            score_and_best.1 = mv;
                            compare_update_argmax(score, Some(mv));
                        }
                    }
                    shared_stat.lock().unwrap().add(&ctx.statistics);
                    Ok(())
                })?;
            (score, best) = shared_score_and_best.into_inner().unwrap();
        }
        (score, Some(best))
    } else {
        // No branches to explore
        if gs.is_check() {
            compare_update_argmax(Score::MIN, None);
            (Score::MIN, None) // Checkmate
        } else {
            compare_update_argmax(Score::ZERO, None);
            (Score::ZERO, None) // Stalemate
        }
    };
    ctx.history.pop();

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

    // Explore the branches
    ctx.statistics.expanded_nodes += 1;
    ctx.history.push(key);
    let (score, best) = if let Some((Branch { mv, next }, rest)) =
        branch_iterator::first_and_rest(gs, table_move, &ctx.move_predictor, depth)
    {
        // The first branch is explored normally
        let mut best = mv;
        let mut score = -eval_minmax(&next, -beta, -alpha, depth - 1, ctx)?;
        if score >= beta {
            ctx.move_predictor.apply_cutoff_bonus(gs, mv, depth); // Cutoff
        } else {
            // The remaining branches are explored with a "principal variation search"
            for Branch { mv, next } in rest.get_or_generate(&ctx.move_predictor, depth) {
                // Explore with minimal bounds to test if it can beat the current score
                let (lo, hi) = (-alpha.max(score)).minimal_window();
                let temp = -eval_minmax(&next, lo, hi, depth - 1, ctx)?;

                // If the branch beats the current score, we might need to re-explore
                if temp > score {
                    best = mv;
                    score = if temp > alpha && temp < beta {
                        -eval_minmax(&next, -beta, -temp, depth - 1, ctx)?
                    } else {
                        temp
                    };
                    if score >= beta {
                        ctx.move_predictor.apply_cutoff_bonus(gs, mv, depth); // Cutoff
                        break;
                    }
                }
            }
        }
        (score, Some(best))
    } else {
        // No branches to explore
        if gs.is_check() {
            (Score::MIN, None) // Checkmate
        } else {
            (Score::ZERO, None) // Stalemate
        }
    };
    ctx.history.pop();

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

type MoveList = SmallVec<[MoveWithKey; 64]>;

fn generate_moves_with_predictor(gs: &GameState, mp: &MovePredictor, depth: u16) -> MoveList {
    let mut moves = SmallVec::<[_; 64]>::new();
    gs.pseudo_legal_moves(|mv| moves.push(MoveWithKey { mv, key: 0 }));
    for MoveWithKey { mv, key } in moves.iter_mut() {
        *key = mp.eval(gs, *mv, depth);
    }
    moves
}

type NonQuietMoveList = SmallVec<[MoveWithKey; 16]>;

fn generate_non_quiet_moves(gs: &GameState) -> NonQuietMoveList {
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

mod branch_iterator {
    use super::*;

    pub struct Branch {
        pub mv: Move,
        pub next: GameState,
    }

    pub struct RestLazy<'a> {
        gs: &'a GameState,
        exclude: Option<Move>,
        rest: Option<MoveList>,
    }

    pub struct Rest<'a> {
        gs: &'a GameState,
        exclude: Option<Move>,
        rest: MoveList,
    }

    pub fn first_and_rest<'a>(
        gs: &'a GameState,
        table_move: Option<Move>,
        move_predictor: &MovePredictor,
        depth: u16,
    ) -> Option<(Branch, RestLazy<'a>)> {
        match table_move {
            Some(mv) => {
                let next = gs.make_move(mv).expect("Table move should be legal");
                Some((
                    Branch { mv, next },
                    RestLazy {
                        gs,
                        exclude: table_move,
                        rest: None,
                    },
                ))
            }
            None => {
                let mut rest = generate_moves_with_predictor(gs, move_predictor, depth);
                loop {
                    let mv = take_highest_move(&mut rest)?;
                    if let Ok(next) = gs.make_move(mv) {
                        break Some((
                            Branch { mv, next },
                            RestLazy {
                                gs,
                                exclude: None,
                                rest: Some(rest),
                            },
                        ));
                    }
                }
            }
        }
    }

    impl<'a> RestLazy<'a> {
        pub fn get_or_generate(self, move_predictor: &MovePredictor, depth: u16) -> Rest<'a> {
            Rest {
                gs: self.gs,
                exclude: self.exclude,
                rest: match self.rest {
                    Some(rest) => rest,
                    None => generate_moves_with_predictor(self.gs, move_predictor, depth),
                },
            }
        }
    }

    impl<'a> Iterator for Rest<'a> {
        type Item = Branch;

        fn next(&mut self) -> Option<Branch> {
            loop {
                let mv = take_highest_move(&mut self.rest)?;
                if Some(mv) == self.exclude {
                    continue;
                }
                if let Ok(next) = self.gs.make_move(mv) {
                    break Some(Branch { mv, next });
                }
            }
        }
    }
}
