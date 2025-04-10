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
    move_predictor::{self, MovePrediction, MovePredictor, MoveScore},
    shared_table::{ScoreKind, Table, TableValue},
    SearchInterrupted,
};

/// Absolute limit on how far the search can go.
/// Prevent depth extensions from going out of control
pub(crate) const MAX_PLY: u16 = 64;

/// Opaque depth token
/// TODO: allow non-integer values
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Depth(u16);

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

/// Result of the minmax search at the root node
type Argmax = RwLock<Option<MinmaxResult>>;

#[derive(Clone, Copy)]
pub struct MinmaxResult {
    /// Depth of the current evaluation
    pub depth: Depth,
    /// Current evaluation
    pub score: Score,
    /// Best move found
    pub best: Option<Move>,
}

enum LookupResult {
    UpdateBounds { alpha: Score, beta: Score },
    Cutoff { score: Score },
}

/// Lookup a position in the table and take the opportunity to restrict the alpha and beta bounds.
/// Returns (alpha, beta, best_move).
fn lookup_table(
    key: &GameStateKeyWithHash,
    mut alpha: Score,
    mut beta: Score,
    desired_depth: Depth,
    ply: u16,
    ctx: &mut MinmaxContext,
) -> (LookupResult, Option<Move>) {
    // Restrict alpha and beta within the loss/win bounds for the current ply
    alpha = alpha.max(Score::LOSS.add_ply(ply));
    beta = beta.min(Score::WIN.add_ply(ply + 1));

    // Restrict alpha and beta further by probing the transposition table
    let table_move = match ctx.table.lookup(key) {
        Some(e) => {
            ctx.statistics.table_hits += 1;
            if e.depth >= desired_depth {
                let score = e.score.add_ply(ply);
                match e.score_kind {
                    ScoreKind::Exact => return (LookupResult::Cutoff { score }, e.best),
                    ScoreKind::AtLeast => alpha = alpha.max(score),
                    ScoreKind::AtMost => beta = beta.min(score),
                }
                if alpha >= beta {
                    return (LookupResult::Cutoff { score }, e.best);
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
    depth: Depth,
    ply: u16,
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
        score: score.sub_ply(ply),
        score_kind,
        best,
    };
    if ctx.table.update(key, table_value) {
        ctx.statistics.table_rewrites += 1;
    }
}

/// Probe the game state history for a threefold repeition or fiftymove draw.
///
/// NOTE: Forced draws should not be stored in the transposition table
/// or else tranpositions could incorrectly be labelled as draw.
/// This measure does not exactly solve the issue of "draw contamination",
/// e.g. when the search avoids a branch because the enemy threatens to force a draw,
/// then store its score in the table, and later probe this "draw contaminated" score
/// even though the draw threat no longer exists.
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

/// Report a potential score and best move
fn update_argmax(argmax: &Argmax, new_score: Score, new_best: Option<Move>, new_depth: Depth) {
    let mut argmax = argmax.write().unwrap();
    if argmax.is_none_or(|argmax| new_depth > argmax.depth || new_score > argmax.score) {
        *argmax = Some(MinmaxResult {
            depth: new_depth,
            score: new_score,
            best: new_best,
        })
    }
}

/// Evaluate a position with a parallel minmax using "principal variation split"
pub fn eval_minmax_pv_split(
    gs: &GameState,
    alpha: Score,
    beta: Score,
    depth: Depth,
    ply: u16,
    ctx: &mut MinmaxContext,
    argmax: Option<&Argmax>,
) -> Result<Score, SearchInterrupted> {
    if ply >= MAX_PLY {
        return Ok(eval_quiescent(gs, alpha, beta, ctx)?);
    }
    let Some(depth) = depth.minus_one() else {
        return Ok(eval_quiescent(gs, alpha, beta, ctx)?);
    };
    if ctx.stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Test for a draw position
    let key = gs.key().hash();
    if is_draw(&key, gs.fiftymove_count, &ctx.history) {
        argmax.map(|a| update_argmax(a, Score::ZERO, None, depth));
        return Ok(Score::ZERO);
    }

    // Lookup in the table
    let (alpha, beta, table_move) = match lookup_table(&key, alpha, beta, depth, ply, ctx) {
        (LookupResult::UpdateBounds { alpha, beta }, table_move) => (alpha, beta, table_move),
        (LookupResult::Cutoff { score }, best_move) => {
            argmax.map(|a| update_argmax(a, score, best_move, depth));
            return Ok(score);
        }
    };

    // Explore the branches
    ctx.statistics.expanded_nodes += 1;
    ctx.history.push(key);
    let (score, best) = if let Some((Branch { mv, next, pred }, rest)) =
        branch_iterator::first_and_rest(gs, table_move, &ctx.move_predictor, ply)
    {
        let mut best = mv;
        let mut score = {
            // Search the first branch first, passing the parallel flag
            // This method is called PV-splitting
            let depth = depth.extend(pred.extend);
            -eval_minmax_pv_split(&next, -beta, -alpha, depth, ply + 1, ctx, None)?
        };
        argmax.map(|a| update_argmax(a, score, Some(mv), depth));
        if score >= beta {
            // Cutoff (no need to update the move predictor because we don't use it here)
        } else {
            // The remaining branches are explored in parallel
            let shared_score = AtomicScore::new(score);
            let shared_score_and_best = Mutex::new((score, best));
            let shared_stat = Mutex::new(&mut ctx.statistics);
            rest.get_or_generate(&ctx.move_predictor, ply)
                .par_bridge()
                .try_for_each(|Branch { mv, next, pred }| {
                    // Create a new context just for this branch, because it's on its own thread
                    let ctx = &mut MinmaxContext {
                        stop: ctx.stop,
                        table: ctx.table,
                        move_predictor: MovePredictor::new(),
                        statistics: MinmaxStatistics::default(),
                        history: ctx.history.clone(),
                    };
                    'end: {
                        let mut score = shared_score.load();
                        if score >= beta {
                            break 'end; // Cutoff (first opportunity)
                        }
                        let depth = depth.extend(pred.extend);
                        let (lo, hi) = (-alpha.max(score)).minimal_window();

                        // Explore with minimal bounds and reduced depth
                        let reduced_depth = depth.reduce(pred.reduce);
                        if reduced_depth != depth {
                            let temp = -eval_minmax(&next, lo, hi, reduced_depth, ply + 1, ctx)?;
                            score = shared_score.load();
                            if score >= beta {
                                break 'end; // Cutoff (second opportunity)
                            }
                            if temp <= score {
                                break 'end; // Branch (probably) does not beat the current score
                            }
                        }

                        // Explore with minimal bounds
                        let temp = -eval_minmax(&next, lo, hi, depth, ply + 1, ctx)?;
                        score = shared_score.load();
                        if score >= beta {
                            break 'end; // Cutoff (third opportunity)
                        }
                        if temp <= score {
                            break 'end; // Branch does not beat the current score
                        }

                        score = if temp > alpha && temp < beta {
                            // Re-explore with normal bounds to get the true score
                            -eval_minmax(&next, -beta, -temp, depth, ply + 1, ctx)?
                        } else {
                            temp
                        };
                        shared_score.fetch_max(score);
                        let mut score_and_best = shared_score_and_best.lock().unwrap();
                        if score > score_and_best.0 {
                            score_and_best.0 = score;
                            score_and_best.1 = mv;
                            argmax.map(|a| update_argmax(a, score, Some(mv), depth));
                        }
                    }
                    // Add the statistics of this branch to the main statistics
                    shared_stat.lock().unwrap().add(&ctx.statistics);
                    Ok(())
                })?;
            (score, best) = shared_score_and_best.into_inner().unwrap();
        }
        (score, Some(best))
    } else {
        // No branches to explore
        if gs.is_check() {
            argmax.map(|a| update_argmax(a, Score::LOSS.add_ply(ply), None, depth));
            (Score::LOSS.add_ply(ply), None) // Checkmate
        } else {
            argmax.map(|a| update_argmax(a, Score::ZERO, None, depth));
            (Score::ZERO, None) // Stalemate
        }
    };
    ctx.history.pop();

    // Store the result in the table
    update_table(key, alpha, beta, depth, ply, score, best, ctx);
    Ok(score)
}

/// Evaluate a position with a minmax search
fn eval_minmax(
    gs: &GameState,
    alpha: Score,
    beta: Score,
    depth: Depth,
    ply: u16,
    ctx: &mut MinmaxContext,
) -> Result<Score, SearchInterrupted> {
    if ply >= MAX_PLY {
        return Ok(eval_quiescent(gs, alpha, beta, ctx)?);
    }
    let Some(depth) = depth.minus_one() else {
        return Ok(eval_quiescent(gs, alpha, beta, ctx)?);
    };
    if ctx.stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Test for a draw position
    let key = gs.key().hash();
    if is_draw(&key, gs.fiftymove_count, &ctx.history) {
        return Ok(Score::ZERO);
    }

    // Lookup in the table
    let (alpha, beta, table_move) = match lookup_table(&key, alpha, beta, depth, ply, ctx) {
        (LookupResult::UpdateBounds { alpha, beta }, table_move) => (alpha, beta, table_move),
        (LookupResult::Cutoff { score }, _) => return Ok(score),
    };

    // Explore the branches
    ctx.statistics.expanded_nodes += 1;
    ctx.history.push(key);
    let (score, best) = if let Some((Branch { mv, next, pred }, rest)) =
        branch_iterator::first_and_rest(gs, table_move, &ctx.move_predictor, ply)
    {
        let mut best = mv;
        let mut score = {
            // The first branch is explored normally
            let depth = depth.extend(pred.extend);
            -eval_minmax(&next, -beta, -alpha, depth, ply + 1, ctx)?
        };
        if score >= beta {
            ctx.move_predictor.apply_cutoff_bonus(gs, mv, ply); // Cutoff
        } else {
            // The remaining branches are explored with a "principal variation search"
            for Branch { mv, next, pred } in rest.get_or_generate(&ctx.move_predictor, ply) {
                let depth = depth.extend(pred.extend);
                let (lo, hi) = (-alpha.max(score)).minimal_window();

                // Explore with minimal bounds and reduced depth
                let reduced_depth = depth.reduce(pred.reduce);
                if reduced_depth != depth {
                    let temp = -eval_minmax(&next, lo, hi, reduced_depth, ply + 1, ctx)?;
                    if temp <= score {
                        continue; // Branch (probably) does not beat the current score
                    }
                }

                // Explore with minimal bounds
                let temp = -eval_minmax(&next, lo, hi, depth, ply + 1, ctx)?;
                if temp <= score {
                    continue; // Branch does not beat the current score
                }

                best = mv;
                score = if temp > alpha && temp < beta {
                    // Re-explore with normal bounds to get the true score
                    -eval_minmax(&next, -beta, -temp, depth, ply + 1, ctx)?
                } else {
                    temp
                };
                if score >= beta {
                    ctx.move_predictor.apply_cutoff_bonus(gs, mv, ply); // Cutoff
                    break;
                }
            }
        }
        (score, Some(best))
    } else {
        // No branches to explore
        if gs.is_check() {
            (Score::LOSS.add_ply(ply), None) // Checkmate
        } else {
            (Score::ZERO, None) // Stalemate
        }
    };
    ctx.history.pop();

    // Store the result in the table
    update_table(key, alpha, beta, depth, ply, score, best, ctx);
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
    let mut moves = NonQuietMoveList::generate(gs);

    // Pop and explore the branches, starting from the most promising
    while let Some((mv, _)) = moves.take_highest() {
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

/// List of moves used during normal search. The extra data is (reduction, extension)
struct MoveList {
    inner: SmallVec<[(Move, MovePrediction); 64]>,
}

impl MoveList {
    fn generate(gs: &GameState, mp: &MovePredictor, ply: u16) -> Self {
        let mut inner = SmallVec::<[_; 64]>::new();
        gs.pseudo_legal_moves(|mv| inner.push((mv, MovePrediction::default())));
        for (mv, prediction) in inner.iter_mut() {
            *prediction = mp.eval(gs, *mv, ply);
        }
        MoveList { inner }
    }

    fn take_highest(&mut self) -> Option<(Move, MovePrediction)> {
        let (index, _) = self.inner.iter().enumerate().max_by_key(|(_, x)| {
            let (_, MovePrediction { score, .. }) = x;
            score
        })?;
        Some(self.inner.swap_remove(index))
    }
}

/// List of moves used during quiescence search
struct NonQuietMoveList {
    inner: SmallVec<[(Move, MoveScore); 16]>,
}

impl NonQuietMoveList {
    fn generate(gs: &GameState) -> Self {
        let mut inner = SmallVec::new();
        gs.pseudo_legal_non_quiet_moves(|mv| inner.push((mv, MoveScore::default())));
        for (mv, score) in inner.iter_mut() {
            *score = move_predictor::eval(gs, *mv);
        }
        NonQuietMoveList { inner }
    }

    fn take_highest(&mut self) -> Option<(Move, MoveScore)> {
        let (index, _) = self.inner.iter().enumerate().max_by_key(|(_, x)| {
            let (_, score) = x;
            score
        })?;
        Some(self.inner.swap_remove(index))
    }
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

impl From<u16> for Depth {
    fn from(value: u16) -> Self {
        Depth(value)
    }
}

impl Depth {
    fn minus_one(self) -> Option<Depth> {
        if self.0 >= 1 {
            Some(Depth(self.0 - 1))
        } else {
            None
        }
    }

    fn reduce(self, amount: Depth) -> Depth {
        Depth(self.0.saturating_sub(amount.0))
    }

    fn extend(self, amount: Depth) -> Depth {
        Depth(self.0 + amount.0)
    }

    pub fn integer(self) -> u16 {
        self.0
    }
}

mod branch_iterator {
    use super::*;

    pub struct Branch {
        /// Move that led to this branch
        pub mv: Move,
        /// State that results from the move
        pub next: GameState,
        /// Result of the move prediction for this branch
        pub pred: MovePrediction,
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
        ply: u16,
    ) -> Option<(Branch, RestLazy<'a>)> {
        match table_move {
            Some(mv) => {
                let next = gs.make_move(mv).expect("Table move should be legal");
                let pred = move_predictor.eval(gs, mv, ply);
                Some((
                    Branch { mv, next, pred },
                    RestLazy {
                        gs,
                        exclude: table_move,
                        rest: None,
                    },
                ))
            }
            None => {
                let mut rest = MoveList::generate(gs, move_predictor, ply);
                loop {
                    let (mv, pred) = rest.take_highest()?;
                    if let Ok(next) = gs.make_move(mv) {
                        break Some((
                            Branch { mv, next, pred },
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
        pub fn get_or_generate(self, move_predictor: &MovePredictor, ply: u16) -> Rest<'a> {
            Rest {
                gs: self.gs,
                exclude: self.exclude,
                rest: match self.rest {
                    Some(rest) => rest,
                    None => MoveList::generate(self.gs, move_predictor, ply),
                },
            }
        }
    }

    impl<'a> Iterator for Rest<'a> {
        type Item = Branch;

        fn next(&mut self) -> Option<Branch> {
            loop {
                let (mv, pred) = self.rest.take_highest()?;
                if Some(mv) == self.exclude {
                    continue;
                }
                if let Ok(next) = self.gs.make_move(mv) {
                    break Some(Branch { mv, next, pred });
                }
            }
        }
    }
}
