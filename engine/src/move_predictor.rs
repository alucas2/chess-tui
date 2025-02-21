use crate::{evaluate, GameState, Move, PlayerSide};

const NUM_KILLER_MOVES: usize = 3;

#[derive(Debug)]
pub struct MovePredictor {
    /// History of cutoffs from white moves
    history_white: [[u8; 64]; 6],
    /// History of cutoffs from black moves
    history_black: [[u8; 64]; 6],
    /// Killer moves that caused a cutoff, for each depth
    killer_moves: Vec<[Option<Move>; NUM_KILLER_MOVES]>,
}

/// Make a fast evaluation of a move.
/// NOTE: The "score" returned by this function has nothing to do with
/// the "score" of a gamestate, the latter being represented by the `Score` type.
pub fn eval(gs: &GameState, mv: Move) -> i16 {
    const CAPTURE_MULT: i16 = 20;
    if let Some((_, victim)) = gs.pieces.get(mv.to) {
        // Bonus for capturing an enemy with a cheap friend
        CAPTURE_MULT * evaluate::piece_value(victim) - evaluate::piece_value(mv.kind)
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
        const KILLER_BONUS: i16 = 512;
        const CAPTURE_MULT: i16 = 20;
        if let Some((_, victim)) = gs.pieces.get(mv.to) {
            // Bonus for capturing an enemy with a cheap friend
            // Ranges from approx 1000 to 17900
            CAPTURE_MULT * evaluate::piece_value(victim) - evaluate::piece_value(mv.kind)
        } else {
            if let Some(i) = self.killer_moves[depth as usize]
                .iter()
                .position(|k| *k == Some(mv))
            {
                // Bonus for killer moves
                // Ranges from approx 500 to 512
                KILLER_BONUS - i as i16
            } else {
                // Bonus from history
                // Ranges from 0 to 255
                (match gs.side_to_move {
                    PlayerSide::White => self.history_white[mv.kind as usize][mv.to as usize],
                    PlayerSide::Black => self.history_black[mv.kind as usize][mv.to as usize],
                }) as i16
            }
        }
    }

    pub fn apply_cutoff_bonus(&mut self, gs: &GameState, mv: Move, depth: u16) {
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
