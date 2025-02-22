use anyhow::bail;
use engine::{GameState, Move, PlayerSide, ScoreInfo, Search};
use std::{
    io,
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};
use uci_parser::UciCommand;

fn main() -> anyhow::Result<()> {
    let mut gs = GameState::default();
    let search = SearchThread::new();
    loop {
        match recv_command()? {
            UciCommand::Uci => {
                println!("id name BlunderCrab");
                println!("id author alucas2");
                println!("option name Hash type spin default 1 min 1 max 256");
                println!("option name UCI_Chess960 type check default false");
                println!("uciok");
            }
            UciCommand::Debug(_) => {}
            UciCommand::IsReady => println!("readyok"),
            UciCommand::SetOption { name, .. } => match name.as_str() {
                "Hash" => {}
                "UCI_Chess960" => {}
                other => bail!("Unknown option {other}"),
            },
            UciCommand::Register { .. } => {}
            UciCommand::UciNewGame => {}
            UciCommand::Position { fen, moves } => {
                gs = fen
                    .as_deref()
                    .unwrap_or("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                    .parse()?;
                for mv in moves {
                    match make_move_with_notation(&gs, &mv) {
                        Some(next_gs) => gs = next_gs,
                        None => bail!("Cannot make the move {mv}"),
                    }
                }
            }
            UciCommand::Go(options) => {
                let (time_left, time_increment) = match gs.side_to_move() {
                    PlayerSide::White => (options.wtime, options.winc),
                    PlayerSide::Black => (options.btime, options.binc),
                };
                search.start(
                    gs,
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

struct SearchThread {
    message_tx: mpsc::Sender<SearchMessage>,
    handle: Option<thread::JoinHandle<()>>,
}

enum SearchMessage {
    Quit,
    StopSearching,
    ChangePosition {
        gs: GameState,
        time_left: Duration,
        time_increment: Duration,
    },
}

enum SearchState {
    Waiting,
    Searching {
        gs: GameState,
        search: Search,
        deadline: Instant,
        stop: bool,
    },
    Pondering(Search),
}

impl SearchThread {
    /// Start a search
    fn new() -> Self {
        let (message_tx, message_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            let mut state = SearchState::Waiting;
            loop {
                match message_rx.try_recv() {
                    Ok(SearchMessage::StopSearching) => match state {
                        SearchState::Searching { ref mut stop, .. } => *stop = true,
                        _ => {}
                    },
                    Ok(SearchMessage::ChangePosition {
                        gs,
                        time_left,
                        time_increment,
                    }) => {
                        let remaining_moves = (60_f64 - gs.fullmoves() as f64).max(10.0);
                        let maximum_time_to_think = time_left.mul_f64(0.5);
                        let time_to_think = (time_left.div_f64(remaining_moves) + time_increment)
                            .min(maximum_time_to_think)
                            .min(Duration::from_secs(9));
                        state = SearchState::Searching {
                            gs,
                            search: Search::start(gs),
                            deadline: Instant::now() + time_to_think,
                            stop: false,
                        };
                    }
                    Ok(SearchMessage::Quit) | Err(mpsc::TryRecvError::Disconnected) => break,
                    Err(mpsc::TryRecvError::Empty) => {}
                }

                match state {
                    SearchState::Searching {
                        ref gs,
                        ref search,
                        deadline,
                        ref mut stop,
                    } => {
                        let status = search.status();
                        *stop |= Instant::now() > deadline;
                        *stop |= !status.thinking;

                        // Print some info
                        let score = match status.score {
                            ScoreInfo::Normal(x) => format!("score cp {x}"),
                            ScoreInfo::Win(x) => format!("score mate {x}"),
                            ScoreInfo::Loose(x) => format!("score mate -{x}"),
                        };
                        let pv = match status.best {
                            Some(best) => format!("pv {}", best.info(gs)),
                            None => format!(""),
                        };
                        println!(
                            "info depth {} nodes {} {pv} {score}",
                            status.depth,
                            status.stats.expanded_nodes + status.stats.expanded_nodes_quiescent
                        );

                        // Stop the search and continue pondering if possible
                        if *stop {
                            match status.best {
                                Some(best) => {
                                    println!("bestmove {}", best.info(gs));
                                    let next_gs =
                                        gs.make_move(best).expect("Best move should be legal");
                                    state = SearchState::Pondering(Search::start(next_gs));
                                }
                                None if !status.thinking => state = SearchState::Waiting,
                                None => {}
                            }
                        }
                    }
                    SearchState::Pondering(ref _ponder_token) => {}
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
    fn start(&self, gs: GameState, time_left: Duration, time_increment: Duration) {
        self.message_tx
            .send(SearchMessage::ChangePosition {
                gs,
                time_left,
                time_increment,
            })
            .unwrap()
    }

    /// Stop the search and print the best move
    fn stop(&self) {
        self.message_tx.send(SearchMessage::StopSearching).unwrap();
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
fn make_move_with_notation(gs: &GameState, notation: &str) -> Option<GameState> {
    let mut available_moves = vec![];
    gs.pseudo_legal_moves(|mv| {
        if gs.make_move(mv).is_ok() {
            available_moves.push(mv)
        }
    });
    find_move_with_notation(&gs, &available_moves, notation).and_then(|mv| gs.make_move(mv).ok())
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
