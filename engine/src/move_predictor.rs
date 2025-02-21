use crate::{evaluate, GameState, Move, MoveFlag, PlayerSide};

#[derive(Debug)]
pub struct MovePredictor {
    /// History of cutoffs from white moves
    history_white: [[i16; 64]; 6],
    /// History of cutoffs from black moves
    history_black: [[i16; 64]; 6],
}

impl Default for MovePredictor {
    fn default() -> Self {
        Self {
            history_white: [[0; 64]; 6],
            history_black: [[0; 64]; 6],
        }
    }
}

/// Make a fast evaluation of a move.
/// NOTE: The "score" returned by this function has nothing to do with
/// the "score" of a gamestate, the latter being represented by the `Score` type.
pub fn eval(gs: &GameState, mv: Move) -> i16 {
    let mut score = 0;
    if let Some((_, kind)) = gs.pieces.get(mv.to) {
        // Bonus for capturing an enemy with a cheap friend
        score += 10 * evaluate::piece_value(kind) - evaluate::piece_value(mv.kind);
    }
    if let MoveFlag::Promotion(kind) = mv.flag {
        // Bonus for promoting a piece
        score += 2000 + evaluate::piece_value(kind)
    }

    score
}

impl MovePredictor {
    /// Make a fast evaluation of a move, with a stateful evaluator for better accuracy.
    pub fn eval(&self, gs: &GameState, mv: Move) -> i16 {
        let mut score = 0;
        if let Some((_, kind)) = gs.pieces.get(mv.to) {
            // Bonus for capturing an enemy with a cheap friend
            score += 10 * evaluate::piece_value(kind) - evaluate::piece_value(mv.kind);
        } else {
            // Bonus from history
            score += match gs.side_to_move {
                PlayerSide::White => self.history_white[mv.kind as usize][mv.to as usize],
                PlayerSide::Black => self.history_black[mv.kind as usize][mv.to as usize],
            }
        }
        if let MoveFlag::Promotion(kind) = mv.flag {
            // Bonus for promoting a piece
            score += 10 * evaluate::piece_value(kind)
        }
        score
    }

    pub fn apply_cutoff_bonus(&mut self, gs: &GameState, mv: Move) {
        const HISTORY_MAX: i16 = 100;
        if gs.pieces.get(mv.to).is_none() {
            let value = match gs.side_to_move {
                PlayerSide::White => &mut self.history_white[mv.kind as usize][mv.to as usize],
                PlayerSide::Black => &mut self.history_black[mv.kind as usize][mv.to as usize],
            };
            *value = (*value + 1).min(HISTORY_MAX);
        }
    }
}
