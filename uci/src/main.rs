use anyhow::bail;
use engine::{settings, GameState, Move, PlayerSide, ScoreInfo, Search};
use std::{
    io,
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};
use uci_parser::UciCommand;

fn main() -> anyhow::Result<()> {
    // Retained state for the next "go" command
    let mut initial = GameState::default();
    let mut moves_from_initial = vec![];
    let mut current = GameState::default();

    // Perpetually running search thread
    let search = SearchThread::new();
    let mut settings = Settings::default();

    // Event loop
    loop {
        match recv_command()? {
            UciCommand::Uci => {
                // Engine informations
                println!("id name BlunderCrab");
                println!("id author alucas2");

                // Engine settings
                let defaults = Settings::default();
                println!(
                    "option name Hash type spin default {} min 1 max 4096",
                    defaults.table_size_megabytes
                );
                println!(
                    "option name Threads type spin default {} min 1 max 4",
                    defaults.num_threads
                );
                println!(
                    "option name UseOpponentsTime type check default {}",
                    defaults.use_opponents_time
                );
                println!("option name UCI_Chess960 type check default false");
                println!("uciok");
            }
            UciCommand::Debug(_) => {}
            UciCommand::IsReady => println!("readyok"),
            UciCommand::SetOption { name, value } => {
                let value = value.unwrap_or_default();
                match name.as_str() {
                    "Hash" => settings.table_size_megabytes = value.parse()?,
                    "Threads" => settings.num_threads = value.parse()?,
                    "UseOpponentsTime" => settings.use_opponents_time = value.parse()?,
                    "UCI_Chess960" => {}
                    other => bail!("Unknown option {other}"),
                }
                search.change_settings(settings);
            }
            UciCommand::Register { .. } => {}
            UciCommand::UciNewGame => {}
            UciCommand::Position { fen, moves } => {
                initial = fen
                    .as_deref()
                    .unwrap_or("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                    .parse()?;
                moves_from_initial.clear();
                current = initial;
                for mv in moves {
                    match make_move_with_notation(&current, &mv) {
                        Some((mv, next_gs)) => {
                            current = next_gs;
                            moves_from_initial.push(mv);
                        }
                        None => bail!("Cannot make the move {mv}"),
                    }
                }
            }
            UciCommand::Go(options) => {
                let (time_left, time_increment) = match current.side_to_move() {
                    PlayerSide::White => (options.wtime, options.winc),
                    PlayerSide::Black => (options.btime, options.binc),
                };
                search.change_position(
                    initial,
                    moves_from_initial.clone(),
                    current,
                    time_left.unwrap_or(Duration::MAX),
                    time_increment.unwrap_or(Duration::ZERO),
                );
            }
            UciCommand::Stop => search.stop(),
            UciCommand::PonderHit => {}
            UciCommand::Quit => return Ok(()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Settings {
    table_size_megabytes: usize,
    num_threads: usize,
    use_opponents_time: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            table_size_megabytes: settings::DEFAULT_TABLE_SIZE_MB,
            num_threads: settings::DEFAULT_NUM_THREADS,
            use_opponents_time: false,
        }
    }
}

struct SearchThread {
    message_tx: mpsc::Sender<SearchMessage>,
    handle: Option<thread::JoinHandle<()>>,
}

enum SearchMessage {
    Quit,
    StopSearching,
    ChangePosition {
        initial: GameState,
        moves_from_initial: Vec<Move>,
        current: GameState,
        time_left: Duration,
        time_increment: Duration,
    },
    ChangeSettings(Settings),
}

enum SearchState {
    Waiting,
    Searching {
        initial: GameState,
        moves_from_initial: Vec<Move>,
        current: GameState,
        search: Search,
        deadline: Instant,
        stop: bool,
    },
    Pondering {
        search: Search,
        deadline: Instant,
    },
}

impl SearchThread {
    /// Start the perpetually running search thread.
    /// It continus to search event when the opponent is thinking
    fn new() -> Self {
        let (message_tx, message_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            let mut state = SearchState::Waiting;
            let mut settings = Settings::default();
            loop {
                match message_rx.try_recv() {
                    Ok(SearchMessage::StopSearching) => match state {
                        SearchState::Searching { ref mut stop, .. } => *stop = true,
                        _ => {}
                    },
                    Ok(SearchMessage::ChangePosition {
                        initial,
                        moves_from_initial,
                        current,
                        time_left,
                        time_increment,
                    }) => {
                        // Some botched logic to determine the time to think
                        let remaining_moves = if !time_increment.is_zero() {
                            (40_f64 - current.fullmoves_count() as f64).max(20.0)
                        } else {
                            (80_f64 - current.fullmoves_count() as f64).max(40.0)
                        };
                        let maximum_time_to_think = time_left.mul_f64(0.5);
                        let time_to_think = (time_left.div_f64(remaining_moves) + time_increment)
                            .min(maximum_time_to_think)
                            .min(Duration::from_secs(10));

                        let search =
                            Search::start(initial, moves_from_initial.iter().copied()).unwrap();
                        state = SearchState::Searching {
                            search,
                            initial,
                            moves_from_initial,
                            current,
                            deadline: Instant::now() + time_to_think,
                            stop: false,
                        };
                    }
                    Ok(SearchMessage::ChangeSettings(new_settings)) => {
                        // Pass the new settings to the engine
                        if new_settings.table_size_megabytes != settings.table_size_megabytes {
                            settings::set_table_size_megabytes(new_settings.table_size_megabytes);
                        }
                        if new_settings.num_threads != settings.num_threads {
                            settings::set_num_threads(new_settings.num_threads);
                        }
                        settings = new_settings;
                    }
                    Ok(SearchMessage::Quit) | Err(mpsc::TryRecvError::Disconnected) => break,
                    Err(mpsc::TryRecvError::Empty) => {}
                }

                match state {
                    SearchState::Searching {
                        ref initial,
                        ref moves_from_initial,
                        ref current,
                        ref search,
                        deadline,
                        ref mut stop,
                    } => {
                        // If the search has not find a result yet, we just wait.
                        if let Some(result) = search.result() {
                            let status = search.status();

                            *stop |= Instant::now() > deadline; // We are past the deadline
                            *stop |= !status.thinking; // The search stopped itself

                            // Print some info
                            let score = match result.score {
                                ScoreInfo::Normal(x) => format!("score cp {x}"),
                                ScoreInfo::Win(x) => format!("score mate {}", x / 2 + 1),
                                ScoreInfo::Loss(x) => format!("score mate -{}", x / 2),
                            };
                            let pv = if result.pv.is_empty() {
                                "".to_string()
                            } else {
                                let mut string = String::new();
                                let mut current = *current;
                                for mv in &result.pv {
                                    string = format!("{string} {}", mv.info(&current));
                                    current =
                                        current.make_move(*mv).expect("PV move should be legal");
                                }
                                format!("pv {string}")
                            };
                            println!(
                                "info depth {} nodes {} {pv} {score}",
                                result.depth,
                                status.stats.expanded_nodes + status.stats.expanded_nodes_quiescent
                            );

                            if *stop {
                                match result.pv.first() {
                                    Some(best) => {
                                        println!("bestmove {}", best.info(current));
                                        if settings.use_opponents_time {
                                            // Continue pondering
                                            // (i.e. search while pretending to be the opponent)
                                            let time_to_ponder = Duration::from_secs(10);
                                            let search = Search::start(
                                                *initial,
                                                moves_from_initial
                                                    .iter()
                                                    .copied()
                                                    .chain(std::iter::once(*best)),
                                            )
                                            .expect("Best move should be legal");
                                            state = SearchState::Pondering {
                                                search,
                                                deadline: Instant::now() + time_to_ponder,
                                            };
                                        } else {
                                            state = SearchState::Waiting;
                                        }
                                    }
                                    None => {
                                        println!("bestmove 0000");
                                        state = SearchState::Waiting
                                    }
                                }
                            }
                        }
                    }
                    SearchState::Pondering {
                        search: ref _ponder_token,
                        deadline,
                    } => {
                        if Instant::now() > deadline {
                            state = SearchState::Waiting;
                        }
                    }
                    _ => {}
                };
                thread::sleep(Duration::from_millis(100));
            }
        });
        SearchThread {
            message_tx,
            handle: Some(handle),
        }
    }

    /// Set the position and search from there
    fn change_position(
        &self,
        initial: GameState,
        moves_from_initial: Vec<Move>,
        current: GameState,
        time_left: Duration,
        time_increment: Duration,
    ) {
        self.message_tx
            .send(SearchMessage::ChangePosition {
                initial,
                moves_from_initial,
                current,
                time_left,
                time_increment,
            })
            .unwrap()
    }

    /// Stop the search and print the best move
    fn stop(&self) {
        self.message_tx.send(SearchMessage::StopSearching).unwrap();
    }

    /// Change search settings
    fn change_settings(&self, settings: Settings) {
        self.message_tx
            .send(SearchMessage::ChangeSettings(settings))
            .unwrap();
    }
}

impl Drop for SearchThread {
    fn drop(&mut self) {
        if let Ok(_) = self.message_tx.send(SearchMessage::Quit) {
            self.handle.take().unwrap().join().unwrap();
        }
    }
}

/// Wait for UCI command on stdin
fn recv_command() -> anyhow::Result<UciCommand> {
    let mut buf = String::new();
    loop {
        io::stdin().read_line(&mut buf)?;
        let message = buf.trim();
        if !message.is_empty() {
            return Ok(buf.trim().parse()?);
        }
    }
}

/// Make a move from a gamestate given its UCI notation
fn make_move_with_notation(gs: &GameState, notation: &str) -> Option<(Move, GameState)> {
    let mut available_moves = vec![];
    gs.pseudo_legal_moves(|mv| {
        if gs.make_move(mv).is_ok() {
            available_moves.push(mv)
        }
    });
    let mv = find_move_with_notation(&gs, &available_moves, notation)?;
    Some((mv, gs.make_move(mv).unwrap()))
}

fn find_move_with_notation(
    gs: &GameState,
    available_moves: &[Move],
    notation: &str,
) -> Option<Move> {
    match available_moves
        .iter()
        .find(|mv| mv.info(&gs).to_string() == notation)
    {
        Some(mv) => Some(*mv),
        None => {
            // Translate the classic castling notation
            // to the chess960 notation that the engine uses
            let alternate_notation = match notation {
                "e1g1" => "e1h1",
                "e1c1" => "e1a1",
                "e8g8" => "e8h8",
                "e8c8" => "e8a8",
                _ => return None,
            };
            find_move_with_notation(gs, available_moves, alternate_notation)
        }
    }
}
