use std::fmt::Display;

use engine::{GameState, Move, MoveFlag, PieceKind, PlayerColor, SquareIndex};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveWithNotation {
    /// Opaque move token used by the engine
    pub inner: Move,
    /// Fullmove number
    pub number: u16,
    /// Player who does the move
    pub player: PlayerColor,
    /// What kind of piece is moving
    pub who: PieceKind,
    /// Starting square of the piece that moves
    pub start: SquareIndex,
    /// Ending square of the piece that moves
    pub end: SquareIndex,
    /// If the starting square's file needs to be specified to disambiguate the move
    pub need_start_file: bool,
    /// If the starting square's rank needs to be specified to disambiguare the move
    pub need_start_rank: bool,
    /// If the move is a capture
    pub capture: bool,
    /// Special move flag
    pub flag: MoveFlag,
    /// If the move is a check or checkmate
    pub outcome: Option<MoveOutcome>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveOutcome {
    Check,
    CheckMate,
}

pub fn moves_with_notation(gs: &GameState) -> Vec<MoveWithNotation> {
    let mut moves = vec![];
    gs.pseudo_legal_moves(|mv| moves.push(mv));

    // Generate the complete notation of each legal move
    let mut result = moves
        .iter()
        .filter_map(|&mv| {
            gs.do_move(mv).ok().map(|next_gs| {
                let mv_info = gs.move_info(mv);
                let capture =
                    gs.piece(mv_info.end).is_some() || mv_info.flag == engine::MoveFlag::EnPassant;
                let outcome = if next_gs.is_check() {
                    let mut next_moves = vec![];
                    next_gs.pseudo_legal_moves(|mv| next_moves.push(mv));
                    if next_moves.iter().any(|&mv| next_gs.do_move(mv).is_ok()) {
                        Some(MoveOutcome::Check)
                    } else {
                        Some(MoveOutcome::CheckMate)
                    }
                } else {
                    None
                };
                MoveWithNotation {
                    inner: mv,
                    number: gs.fullmoves(),
                    player: gs.active_color(),
                    who: mv_info.who,
                    start: mv_info.start,
                    end: mv_info.end,
                    need_start_file: false,
                    need_start_rank: false,
                    capture,
                    flag: mv_info.flag,
                    outcome,
                }
            })
        })
        .collect::<Vec<_>>();

    // Add information to disambiguate moves
    for i in 0..result.len() {
        let mv = result[i];
        for j in 0..result.len() {
            let other = result[j];
            if i == j || mv.who != other.who || mv.end != other.end {
                continue;
            }
            if mv.start.coords().0 != other.start.coords().0 {
                result[i].need_start_file = true;
            } else if mv.start.coords().1 != other.start.coords().1 {
                result[i].need_start_rank = true;
            }
        }
    }

    result
}

impl Display for MoveWithNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.flag {
            engine::MoveFlag::CastleEast => write!(f, "O-O")?,
            engine::MoveFlag::CastleWest => write!(f, "O-O-O")?,
            _ => {
                if self.who != PieceKind::Pawn {
                    write!(f, "{}", self.who.label().to_ascii_uppercase())?;
                }
                let (start_file, start_rank) = self.start.coords();
                let (dst_file, dst_rank) = self.end.coords();
                if self.need_start_file || self.capture && self.who == PieceKind::Pawn {
                    write!(f, "{}", start_file.label())?
                }
                if self.need_start_rank {
                    write!(f, "{}", start_rank.label())?
                }
                if self.capture {
                    write!(f, "x")?
                }
                write!(f, "{}{}", dst_file.label(), dst_rank.label())?
            }
        }
        match self.flag {
            engine::MoveFlag::Promotion(piece_kind) => {
                write!(f, "={}", piece_kind.label().to_ascii_uppercase())?
            }
            _ => {}
        }
        match self.outcome {
            Some(MoveOutcome::Check) => write!(f, "+")?,
            Some(MoveOutcome::CheckMate) => write!(f, "#")?,
            None => {}
        }
        Ok(())
    }
}
