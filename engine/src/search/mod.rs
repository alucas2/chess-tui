mod evaluate;
mod minmax;
mod move_predictor;
mod shared_table;
mod thread_pool;

use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
    thread, time,
};

use evaluate::Score;
use minmax::{MinmaxContext, MinmaxResult, MinmaxStatistics};
use move_predictor::MovePredictor;
use shared_table::TableValue;

use crate::{GameState, IllegalMoveError, Move};

pub use evaluate::ScoreInfo;

pub mod settings {
    use super::*;

    pub use shared_table::set_table_size_megabytes;
    pub use shared_table::DEFAULT_TABLE_SIZE_MB;
    pub use thread_pool::set_num_threads;
    pub use thread_pool::DEFAULT_NUM_THREADS;
}

/// Handle to the search thread
pub struct Search {
    gs: GameState,
    result: Arc<RwLock<Option<MinmaxResult>>>,
    stop: Arc<AtomicBool>,
    stats: Arc<RwLock<MinmaxStatistics>>,
    handle: Option<thread::JoinHandle<()>>,
    start: time::Instant,
    finish: Arc<RwLock<Option<time::Instant>>>,
}

/// Status of the search that is updated by the search thread
#[derive(Clone)]
pub struct SearchStatus {
    /// Indicates if the search thread is still running
    pub thinking: bool,
    /// Search performance metrics
    pub stats: MinmaxStatistics,
    /// Time elapsed
    pub elapsed: time::Duration,
}

#[derive(Clone)]
pub struct SearchResult {
    /// Depth of the search that gave the current result
    pub depth: u16,
    /// Score assigned to the searched position
    pub score: ScoreInfo,
    /// Best move and continuation from the search position
    pub pv: Vec<Move>,
}

struct SearchInterrupted;

impl Search {
    /// Start a search from an initial state and a list of moves made from it.
    pub fn start(
        initial: GameState,
        moves_from_initial: impl Iterator<Item = Move>,
    ) -> Result<Self, IllegalMoveError> {
        // Reconstruct the last game state and the history
        let mut gs = initial;
        let mut history = vec![];
        for mv in moves_from_initial {
            history.push(gs.key().hash());
            gs = gs.make_move(mv)?;
        }

        // Spawn the search thread
        let result = Arc::new(RwLock::new(None));
        let stop = Arc::new(AtomicBool::new(false));
        let stats = Arc::new(RwLock::new(MinmaxStatistics::default()));
        let start = time::Instant::now();
        let finish = Arc::new(RwLock::new(None));
        let handle = {
            let result = Arc::clone(&result);
            let stop = Arc::clone(&stop);
            let stats = Arc::clone(&stats);
            let finish = Arc::clone(&finish);
            thread::spawn(move || {
                // Iterative deepening
                for depth in 8..=30 {
                    let mut ctx = MinmaxContext {
                        stop: &stop,
                        table: &shared_table::get(),
                        move_predictor: MovePredictor::new(),
                        statistics: MinmaxStatistics::default(),
                        history: history.clone(),
                    };
                    let final_score = thread_pool::get().install(|| {
                        minmax::eval_minmax_pv_split(
                            &gs,
                            Score::LOSS,
                            Score::WIN,
                            depth.into(),
                            0,
                            &mut ctx,
                            Some(&result),
                        )
                    });
                    stats.write().unwrap().add(&ctx.statistics);
                    let Ok(final_score) = final_score else {
                        break; // Search has been interrupted
                    };
                    match final_score.info() {
                        // Stop deepening when a checkmate in a minimal number of moves is found
                        // Note: we may still miss an end of game due to reductions though
                        ScoreInfo::Win(ply) | ScoreInfo::Loss(ply) if ply <= depth => break,
                        _ => {}
                    };
                    if result.read().unwrap().is_some_and(|r| r.best.is_none()) {
                        break; // Current position is a dead end
                    }
                }
                // Signal the end of the search
                *finish.write().unwrap() = Some(time::Instant::now());
            })
        };
        Ok(Search {
            gs,
            result,
            stop,
            stats,
            handle: Some(handle),
            start,
            finish,
        })
    }

    /// Get the status of the search thread
    pub fn status(&self) -> SearchStatus {
        let stats = self.stats.read().unwrap().clone();
        let (elapsed, thinking) = match *self.finish.read().unwrap() {
            Some(finish) => (finish - self.start, false),
            None => (self.start.elapsed(), true),
        };

        SearchStatus {
            thinking,
            stats,
            elapsed,
        }
    }

    /// Get the result of the search thread. Returns None if it's too early to have a result.
    pub fn result(&self) -> Option<SearchResult> {
        let result = self.result.read().unwrap().clone()?;
        let pv = match result.best {
            Some(mut mv) => {
                let mut gs = self.gs;
                let mut pv = vec![mv];
                let mut seen_positions = vec![];
                loop {
                    gs = gs.make_move(mv).expect("PV move should be legal");
                    let key = gs.key().hash();
                    if seen_positions.contains(&key) {
                        break; // PV forms a loop
                    }
                    mv = match shared_table::get().lookup(&key) {
                        Some(TableValue { best: Some(mv), .. }) => mv,
                        _ => break, // PV stops here
                    };
                    pv.push(mv);
                    seen_positions.push(key);
                }
                pv
            }
            _ => vec![],
        };
        let score = result.score.info();
        let depth = result.depth.integer() + 1;
        Some(SearchResult { depth, score, pv })
    }

    /// Equivalent to `result().is_some()`
    pub fn has_result(&self) -> bool {
        self.result.read().unwrap().is_some()
    }
}

impl Drop for Search {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        let _ = self.handle.take().unwrap().join();
    }
}
