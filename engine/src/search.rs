use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, OnceLock, RwLock,
    },
    thread,
};

use smallvec::SmallVec;

use crate::{GameState, Move, Table, TableKey};

/// Handle to the search thread
pub struct Search {
    status: Arc<RwLock<SearchStatus>>,
    stop: Arc<AtomicBool>,
    handle: Option<thread::JoinHandle<()>>,
}

/// Status of the search that is updated by the search thread
#[derive(Clone)]
pub struct SearchStatus {
    /// Indicates if the search thread is still running
    pub thinking: bool,
    /// Depth of the latest search iteration
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

/// Opaque score that can be compared with other scores.
/// Score::MAX represents a winning position. Score::MIN represents a losing position.
/// Score::NEG_INF acts like "negative infinity", which is a placeholder invalid score.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
struct Score(i16);

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

/// Values that are stored in the table.
/// Its size should not exceed 14 bytes to fit within a table entry
#[derive(Clone, Copy)]
struct TableValue {
    /// depth zero means quiescent search
    depth: u16,
    score: Score,
    best_move: Option<Move>,
}

static TABLE: OnceLock<Table<(), TableValue>> = OnceLock::new();
const TABLE_NUM_ENTRIES: usize = 2_usize.pow(22);

impl Search {
    pub fn start(gs: GameState) -> Self {
        let table = TABLE.get_or_init(|| Table::new(TABLE_NUM_ENTRIES));
        let status = Arc::new(RwLock::new(SearchStatus {
            thinking: true,
            depth: 0,
            score: ScoreInfo::Normal(0),
            best: None,
            stats: SearchStatistics::default(),
        }));
        let stop = Arc::new(AtomicBool::new(false));
        let handle = {
            let status = Arc::clone(&status);
            let stop = Arc::clone(&stop);
            thread::spawn(move || {
                // Iterative deepening
                for depth in 1..=20 {
                    let mut stats = SearchStatistics::default();
                    let result = eval_minmax(
                        &gs,
                        Score::MIN,
                        Score::MAX,
                        depth,
                        &stop,
                        &table,
                        &mut stats,
                    );
                    let Ok((score, best)) = result else {
                        break;
                    };
                    let score = match score {
                        Score::MIN => ScoreInfo::Loose(depth - 1),
                        Score::MAX => ScoreInfo::Win(depth - 1),
                        _ => ScoreInfo::Normal(score.0),
                    };
                    *status.write().unwrap() = SearchStatus {
                        thinking: true,
                        depth,
                        score,
                        best,
                        stats,
                    };
                    if matches!(score, ScoreInfo::Loose(_) | ScoreInfo::Win(_)) {
                        break; // Stop deepening when a move is found
                    }
                }
                status.write().unwrap().thinking = false;
            })
        };
        Search {
            status,
            stop,
            handle: Some(handle),
        }
    }

    pub fn status(&self) -> SearchStatus {
        self.status.read().unwrap().clone()
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
) -> Result<(Score, Option<Move>), SearchInterrupted> {
    if depth == 0 {
        return Ok((eval_quiescent(gs, alpha, beta, stop, table, stats)?, None));
    }
    if stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Lookup in the table
    let table_key = TableKey::new(gs, ());
    let move_to_try_first = match table.lookup(&table_key) {
        Some(e) if e.depth == depth => {
            stats.table_hits += 1;
            return Ok((e.score, e.best_move));
        }
        Some(e) => e.best_move,
        None => None,
    };

    let mut score = Score::NEG_INF;
    let mut best_move = None;
    'cutoff: {
        // Try the cached move first to raise the alpha bound
        if let Some(mv) = move_to_try_first {
            let next_gs = gs.make_move(mv).expect("Move to try first should be legal");
            let branch_score =
                -eval_minmax(&next_gs, -beta, -alpha, depth - 1, stop, table, stats)?.0;
            if branch_score > score {
                best_move = Some(mv)
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
                -eval_minmax(&next_gs, -beta, -alpha, depth - 1, stop, table, stats)?.0;
            if branch_score > score {
                best_move = Some(mv)
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

    let table_value = TableValue {
        depth,
        score,
        best_move,
    };
    if table.update(table_key, table_value) {
        stats.table_rewrites += 1;
    }
    Ok((score, best_move))
}

/// Evaluate a position by quiescent search
fn eval_quiescent(
    gs: &GameState,
    mut alpha: Score,
    beta: Score,
    stop: &AtomicBool,
    table: &Table<(), TableValue>,
    stats: &mut SearchStatistics,
) -> Result<Score, SearchInterrupted> {
    if stop.load(Ordering::Relaxed) {
        return Err(SearchInterrupted);
    }

    // Lookup in the table
    let table_key = TableKey::new(gs, ());
    match table.lookup(&table_key) {
        Some(e) if e.depth == 0 => {
            stats.table_hits += 1;
            return Ok(e.score);
        }
        _ => {}
    };

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
        let branch_score = -eval_quiescent(&next_gs, -beta, -alpha, stop, table, stats)?;
        score = score.max(branch_score);
        alpha = alpha.max(score);
        if alpha >= beta {
            break;
        }
    }

    let table_value = TableValue {
        depth: 0,
        score,
        best_move: None,
    };
    if table.update(table_key, table_value) {
        stats.table_rewrites += 1;
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
