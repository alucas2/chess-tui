use crate::{evaluate, GameState, Move, MoveFlag, MoveInfo, PieceKind, PlayerSide, SquareIndex};

const NUM_KILLER_MOVES: usize = 3;

#[derive(Debug)]
pub struct MovePredictor {
    /// History of cutoffs from white moves
    history_white: [[u8; 64]; 6],
    /// History of cutoffs from black moves
    history_black: [[u8; 64]; 6],
    /// Killer moves that caused a cutoff, for each depth
    killer_moves: Vec<[Option<MoveInfo>; NUM_KILLER_MOVES]>,
}

/// Make a fast evaluation of a move.
/// NOTE: The "score" returned by this function has nothing to do with
/// the "score" of a gamestate, the latter being represented by the `Score` type.
pub fn eval(gs: &GameState, mv: Move) -> i16 {
    let mvi = mv.unwrap();
    const CAPTURE_MULT: i16 = 20;
    match gs.pieces.get(mvi.to) {
        Some((_, victim)) if !matches!(mvi.flag, MoveFlag::CastleEast | MoveFlag::CastleWest) => {
            // Static exchange evaluation for captures
            // Ranges from approx. -18000 to +18000
            let next_gs = gs.make_move_exchange_eval(mv);
            CAPTURE_MULT
                * (evaluate::piece_value(victim)
                    - static_exchange_eval(&next_gs, mvi.to.mirror(), mvi.kind))
        }
        _ => 0,
    }
}

/// Compute the material gain that happen if the specified piece at the specified square
/// gets captured, and its capturor is captured again, and so on...
fn static_exchange_eval(gs: &GameState, sq: SquareIndex, victim: PieceKind) -> i16 {
    if let Some(mv) = gs.cheapest_attacker(sq) {
        let next_gs = gs.make_move_exchange_eval(mv);
        let mv = mv.unwrap();
        let capturor = match mv.flag {
            MoveFlag::Promotion(prom) => prom,
            _ => mv.kind,
        };
        let mut score = evaluate::piece_value(victim);
        score -= static_exchange_eval(&next_gs, sq.mirror(), capturor);
        score.max(0)
    } else {
        0
    }
}

impl MovePredictor {
    pub fn new(max_depth: u16) -> Self {
        MovePredictor {
            history_white: [[0; 64]; 6],
            history_black: [[0; 64]; 6],
            killer_moves: vec![[None; NUM_KILLER_MOVES]; max_depth as usize],
        }
    }

    /// Make a fast evaluation of a move, with a stateful evaluator for better accuracy.
    pub fn eval(&self, gs: &GameState, mv: Move, depth: u16) -> i16 {
        let mvi = mv.unwrap();
        const KILLER_BONUS: i16 = 512;
        const CAPTURE_MULT: i16 = 20;
        match gs.pieces.get(mvi.to) {
            Some((_, victim))
                if !matches!(mvi.flag, MoveFlag::CastleEast | MoveFlag::CastleWest) =>
            {
                // Static exchange evaluation for captures
                // Ranges from approx. -18000 to +18000
                let next_gs = gs.make_move_exchange_eval(mv);
                let capture_gain = evaluate::piece_value(victim)
                    - static_exchange_eval(&next_gs, mvi.to.mirror(), mvi.kind);
                match capture_gain {
                    // Place winning and neutral captures above killer moves
                    0.. => CAPTURE_MULT * capture_gain + KILLER_BONUS + 1,
                    // Place loosing captures after everything
                    ..=-1 => CAPTURE_MULT * capture_gain,
                }
            }
            _ => {
                if let Some(i) = self.killer_moves[depth as usize]
                    .iter()
                    .position(|k| *k == Some(mvi))
                {
                    // Bonus for killer moves
                    // Ranges from approx 500 to 512
                    KILLER_BONUS - i as i16
                } else {
                    // Bonus from history
                    // Ranges from 0 to 255
                    (match gs.side_to_move {
                        PlayerSide::White => self.history_white[mvi.kind as usize][mvi.to as usize],
                        PlayerSide::Black => self.history_black[mvi.kind as usize][mvi.to as usize],
                    }) as i16
                }
            }
        }
    }

    pub fn apply_cutoff_bonus(&mut self, gs: &GameState, mv: Move, depth: u16) {
        let mv = mv.unwrap();
        if gs.pieces.get(mv.to).is_none() {
            // Store as a killer move
            let killer_moves = &mut self.killer_moves[depth as usize];
            if !killer_moves[..NUM_KILLER_MOVES - 1].contains(&Some(mv)) {
                killer_moves.rotate_right(1);
                killer_moves[0] = Some(mv);
            }
            // Increase the history value
            let value = match gs.side_to_move {
                PlayerSide::White => &mut self.history_white[mv.kind as usize][mv.to as usize],
                PlayerSide::Black => &mut self.history_black[mv.kind as usize][mv.to as usize],
            };
            *value = (*value).saturating_add(1);
        }
    }
}
