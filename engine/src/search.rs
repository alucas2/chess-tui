use std::{
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, LazyLock, RwLock,
    },
    thread,
};

use smallvec::SmallVec;

use crate::{GameState, Move, Table, TableKey};

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

#[derive(Clone, Copy)]
pub enum ScoreInfo {
    Normal(i16),
    /// Position is a win in the specified number of moves
    Win(u16),
    /// Position is a loss in the specified number of moves
    Loose(u16),
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

/// Opaque score that can be compared with other scores.
/// Score::MAX represents a winning position. Score::MIN represents a losing position.
/// Score::NEG_INF acts like "negative infinity", which is a placeholder invalid score.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
struct Score(i16);

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
    let move_to_try_first = match table.lookup(&table_key) {
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
    'cutoff: {
        // Try the cached move first to raise the alpha bound
        if let Some(mv) = move_to_try_first {
            let next_gs = gs.make_move(mv).expect("Move to try first should be legal");
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
                break 'cutoff;
            }
        }

        // Generate the moves and assign them a score
        stats.expanded_nodes += 1;
        let mut moves = SmallVec::<[_; 64]>::new();
        gs.pseudo_legal_moves(|mv| moves.push(MoveWithKey { mv, key: 0 }));
        for MoveWithKey { mv, key } in moves.iter_mut() {
            *key = gs.eval_move(*mv);
        }

        // Pop and explore the branches, starting from the most promising
        while let Some((index_max, _)) = moves.iter().enumerate().max_by_key(|(_, x)| x.key) {
            let mv = moves.swap_remove(index_max).mv;
            if Some(mv) == move_to_try_first {
                continue; // Already explored the first move
            }
            let Ok(next_gs) = gs.make_move(mv) else {
                continue; // Illegal move
            };
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
                break 'cutoff;
            }
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
    let mut score = eval_heuristic(gs);
    alpha = alpha.max(score);
    if alpha >= beta {
        return Ok(score);
    }

    // Generate the captures and assign them a score
    stats.expanded_nodes_quiescent += 1;
    let mut moves = SmallVec::<[_; 16]>::new();
    gs.pseudo_legal_captures(|mv| moves.push(MoveWithKey { mv, key: 0 }));
    for MoveWithKey { mv, key } in moves.iter_mut() {
        *key = gs.eval_move(*mv);
    }

    // Pop and explore the branches, starting from the most promising
    while let Some((index_max, _)) = moves.iter().enumerate().max_by_key(|(_, x)| x.key) {
        let mv = moves.swap_remove(index_max).mv;
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

/// Evaluate a position with a fast heuristic
fn eval_heuristic(gs: &GameState) -> Score {
    Score(gs.material_value)
    // let mut score = 0;
    // for kind in PieceKind::iter() {
    //     // Sum the friend material
    //     for sq in SquareIter(gs.friends_bb[kind]) {
    //         score += lut::piece_value_table(kind)[sq as usize];
    //     }
    //     // Subtract the enemy material
    //     for sq in SquareIter(gs.enemies_bb[kind]) {
    //         score -= lut::piece_value_table(kind)[sq.mirror() as usize];
    //     }
    // }
    // Score(score)
}

impl Score {
    const ZERO: Score = Score(0);
    const MAX: Score = Score(i16::MAX);
    const MIN: Score = Score(-i16::MAX);
    const NEG_INF: Score = Score(i16::MIN);
}

impl std::ops::Neg for Score {
    type Output = Score;

    fn neg(self) -> Self::Output {
        Score(-self.0)
    }
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
