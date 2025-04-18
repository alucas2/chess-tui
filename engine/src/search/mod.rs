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
use minmax::{MinmaxContext, MinmaxResult, MinmaxResultSender, MinmaxStatistics};
use move_predictor::MovePredictor;
use shared_table::TableEntry;

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
    /// Gamestate of the root node of the search
    gs: GameState,
    /// Block of data shared between the supervisor, worker, and current threads
    shared: Arc<SharedData>,
    /// Handle to the search supervisor thread
    supervisor: Option<thread::JoinHandle<()>>,
    /// Timestamp of the search start
    start: time::Instant,
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
    /// Result proposals for each worker
    pub workers: Vec<Option<WorkerStatus>>,
}

#[derive(Clone)]
pub struct WorkerStatus {
    /// Depth that the worker has reached
    pub depth: u16,
    /// Score that the worker proposes
    pub score: ScoreInfo,
    /// Best move that the worker proposes
    pub best: Option<Move>,
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

struct SharedData {
    /// Result of the search, for each worker
    results: Vec<MinmaxResultSender>,
    /// Stats for each worker
    stats: RwLock<MinmaxStatistics>,
    /// Search stop signal
    stop: AtomicBool,
    /// Timestamp of the search end, if it ended
    finish: RwLock<Option<time::Instant>>,
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

        // Create the shared block of data
        let num_threads = thread_pool::get().current_num_threads();
        let shared = Arc::new(SharedData {
            results: (0..num_threads).map(|_| None.into()).collect(),
            stats: MinmaxStatistics::default().into(),
            stop: false.into(),
            finish: None.into(),
        });

        // Spawn the supervisor thread.
        // It actually does nothing apart from spawning worker and waiting for them to finish
        let start = time::Instant::now();
        let supervisor = {
            let shared = Arc::clone(&shared);
            thread::spawn(move || {
                // Spawn the search workers
                thread_pool::get().broadcast(|tctx| {
                    let tid = tctx.index();
                    if tid >= num_threads {
                        return;
                    }
                    let result = &shared.results[tid];

                    // Iterative deepening
                    for depth in 8..=30 {
                        let mut ctx = MinmaxContext {
                            stop: &shared.stop,
                            table: &shared_table::get(),
                            move_predictor: MovePredictor::new(),
                            statistics: MinmaxStatistics::default(),
                            history: history.clone(),
                            result_tx: Some(&result),
                        };
                        let final_score = minmax::eval_minmax(
                            &gs,
                            Score::LOSS,
                            Score::WIN,
                            depth.into(),
                            0,
                            &mut ctx,
                        );
                        shared.stats.write().unwrap().add(&ctx.statistics);
                        if result.read().unwrap().is_some_and(|r| r.best.is_none()) {
                            break; // Current position is a dead end
                        }
                        let Ok(final_score) = final_score else {
                            break; // Search has been interrupted
                        };
                        match final_score.info() {
                            // Stop deepening when a checkmate in a minimal number of moves is found
                            // Note: we may still miss an end of game due to reductions though
                            ScoreInfo::Win(ply) | ScoreInfo::Loss(ply) if ply <= depth => break,
                            _ => {}
                        };
                    }
                });

                // Signal the end of the search
                *shared.finish.write().unwrap() = Some(time::Instant::now());
            })
        };

        Ok(Search {
            gs,
            shared,
            supervisor: Some(supervisor),
            start,
        })
    }

    /// Get the status of the search
    pub fn status(&self) -> SearchStatus {
        let stats = self.shared.stats.read().unwrap().clone();
        let (elapsed, thinking) = match *self.shared.finish.read().unwrap() {
            Some(finish) => (finish - self.start, false),
            None => (self.start.elapsed(), true),
        };
        let mut workers = vec![];
        for proposal in &self.shared.results {
            if let Some(proposal) = *proposal.read().unwrap() {
                workers.push(Some(WorkerStatus {
                    depth: proposal.depth.integer() + 1,
                    score: proposal.score.info(),
                    best: proposal.best,
                }))
            } else {
                workers.push(None)
            }
        }

        SearchStatus {
            thinking,
            stats,
            elapsed,
            workers,
        }
    }

    /// Get the result of the search thread. Returns None if it's too early to have a result.
    pub fn result(&self) -> Option<SearchResult> {
        // Combine the proposals of all search workers into one agreed upon result
        let mut result = None;
        for proposal in &self.shared.results {
            if let Some(proposal) = *proposal.read().unwrap() {
                if result.is_none_or(|r: MinmaxResult| {
                    proposal.depth > r.depth || proposal.score > r.score
                }) {
                    result = Some(proposal);
                }
            }
        }
        let result = result?;

        // Reconstruct the principal variation (roughly) by probing the table
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
                    let table = shared_table::get();
                    mv = match table[key.hash as usize % table.len()].try_load() {
                        Some(Some(e @ TableEntry { best: Some(mv), .. })) if e.key == key.key => mv,
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
        self.result().is_some()
    }
}

impl Drop for Search {
    fn drop(&mut self) {
        self.shared.stop.store(true, Ordering::Relaxed);
        let _ = self.supervisor.take().unwrap().join();
    }
}
