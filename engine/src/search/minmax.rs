use std::sync::{
    atomic::{AtomicBool, Ordering},
    RwLock,
};

use branch_iterator::Branch;
use smallvec::SmallVec;

use crate::{GameState, GameStateKeyWithHash, Move};

use super::{
    evaluate::{self, Score},
    move_predictor::{self, MovePrediction, MovePredictor, MoveScore},
    shared_table::{ScoreKind, TableEntry, TableEntryCell},
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
    pub table: &'a [TableEntryCell],
    /// Stateful move predictor
    pub move_predictor: MovePredictor,
    /// Search performance metrics
    pub statistics: MinmaxStatistics,
    /// Stack of previous game states, used to detect draws
    pub history: Vec<GameStateKeyWithHash>,
    /// Used by the root move to send its results
    pub result_tx: Option<&'a MinmaxResultSender>,
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
pub type MinmaxResultSender = RwLock<Option<MinmaxResult>>;

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
fn lookup_table(
    key: &GameStateKeyWithHash,
    mut alpha: Score,
    mut beta: Score,
    desired_depth: Depth,
    ply: u16,
    ctx: &mut MinmaxContext,
) -> (LookupResult, Option<Move>) {
    // Restrict alpha and beta further by probing the transposition table
    let index = key.hash as usize % ctx.table.len();
    let table_move = match ctx.table[index].try_load() {
        Some(Some(e)) if e.key == key.key => {
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
        _ => None,
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
    let index = key.hash as usize % ctx.table.len();
    let e = TableEntry {
        key: key.key,
        depth,
        score: score.sub_ply(ply),
        score_kind,
        best,
    };
    if ctx.table[index].try_store(Some(e)) {
        ctx.statistics.table_rewrites += 1;
    }
}

/// Probe the game state history for a threefold repeition or fiftymove draw.
///
/// NOTE: We test for twofold repetition instead of threefold so that draw situations
/// are discovered faster during the search. A position being declared
/// a draw does not necessarily means that the game should end.
///
/// NOTE: Forced draws should not be stored in the transposition table
/// or else tranpositions could incorrectly be labelled as draw.
/// This measure does not exactly solve the issue of "draw contamination",
/// e.g. when the search avoids a branch because the enemy threatens to force a draw,
/// then store its score in the table, and later probe this "draw contaminated" score
/// even though the draw threat no longer exists.
fn is_repetition(
    key: &GameStateKeyWithHash,
    fiftymove_counter: u16,
    history: &[GameStateKeyWithHash],
) -> bool {
    if fiftymove_counter >= 100 {
        return true;
    }
    let mut i = 2;
    while i <= fiftymove_counter as usize && i <= history.len() {
        if &history[history.len() - i] == key {
            return true;
        }
        i += 2;
    }
    false
}

/// Report a potential score and best move
fn update_result(
    result_tx: &MinmaxResultSender,
    new_score: Score,
    new_best: Option<Move>,
    new_depth: Depth,
) {
    let mut result = result_tx.write().unwrap();
    if result.is_none_or(|r| new_depth > r.depth || new_score > r.score) {
        *result = Some(MinmaxResult {
            depth: new_depth,
            score: new_score,
            best: new_best,
        })
    }
}

/// Evaluate a position with a minmax search
pub fn eval_minmax(
    gs: &GameState,
    alpha: Score,
    beta: Score,
    depth: Depth,
    ply: u16,
    ctx: &mut MinmaxContext,
) -> Result<Score, SearchInterrupted> {
    if ply >= MAX_PLY {
        return eval_quiescent(gs, alpha, beta, ctx);
    }
    let Some(mut depth) = depth.minus_one() else {
        return eval_quiescent(gs, alpha, beta, ctx);
    };
    if ctx.stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // This is None for all nodes but the root because only the root node should send results
    let result = ctx.result_tx.take();

    // Test for a draw position, but we want to keep searching if we are at the root
    let key = gs.key().hash();
    if (is_repetition(&key, gs.fiftymove_count, &ctx.history)
        || evaluate::insufficient_material(gs))
        && result.is_none()
    {
        return Ok(Score::ZERO);
    }

    // Restrict alpha and beta within the loss/win bounds for the current ply
    let alpha = alpha.max(Score::LOSS.add_ply(ply));
    let beta = beta.min(Score::WIN.add_ply(ply + 1));

    // Lookup in the table
    let (alpha, beta, table_move) = match lookup_table(&key, alpha, beta, depth, ply, ctx) {
        (LookupResult::UpdateBounds { alpha, beta }, table_move) => (alpha, beta, table_move),
        (LookupResult::Cutoff { score }, table_move) => {
            result.map(|a| update_result(a, score, table_move, depth));
            return Ok(score);
        }
    };

    // Push the current gamestate to the history for repetition tests.
    // Do not forget to pop before leaving the function
    ctx.history.push(key);

    // Reduce the search depth if the null move is successful
    if !evaluate::null_move_risky(gs) {
        if let Ok(next) = gs.make_move_null() {
            let (lo, hi) = beta.minimal_window();
            let reduced_depth = depth.reduce(3.into());
            if -eval_minmax(&next, -hi, -lo, reduced_depth, ply + 1, ctx)? >= beta {
                depth = reduced_depth
            }
        }
    }

    // Explore the branches
    ctx.statistics.expanded_nodes += 1;
    let (score, best) = 'cutoff: {
        let Some((Branch { mv, next, pred }, rest)) =
            branch_iterator::first_and_rest(gs, table_move, &ctx.move_predictor, ply)
        else {
            // No branches to explore
            let best = None;
            let score = if gs.is_check() {
                Score::LOSS.add_ply(ply) // Checkmate
            } else {
                Score::ZERO // Stalemate
            };
            result.map(|a| update_result(a, score, best, depth));
            break 'cutoff (score, best); // Cutoff
        };

        // Explore the first branch
        let mut best = mv;
        let mut score = {
            let depth = depth.extend(pred.extend);
            -eval_minmax(&next, -beta, -alpha, depth, ply + 1, ctx)?
        };
        result.map(|a| update_result(a, score, Some(mv), depth));
        if score >= beta {
            ctx.move_predictor.apply_cutoff_bonus(gs, mv, ply);
            break 'cutoff (score, Some(mv)); // Cutoff
        }

        // Explore the remaining branches
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
            let mut temp = -eval_minmax(&next, lo, hi, depth, ply + 1, ctx)?;
            if temp <= score {
                continue; // Branch does not beat the current score
            }

            // Re-explore with normal bounds to get the true score
            if temp > alpha && temp < beta {
                temp = -eval_minmax(&next, -beta, -temp, depth, ply + 1, ctx)?;
                if temp <= score {
                    continue; // Branch does not actually beat the current score
                }
            }

            best = mv;
            score = temp;
            result.map(|a| update_result(a, score, Some(mv), depth));
            if score >= beta {
                ctx.move_predictor.apply_cutoff_bonus(gs, mv, ply);
                break 'cutoff (score, Some(mv)); // Cutoff
            }
        }

        (score, Some(best)) // No cutoff
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
