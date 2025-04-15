use crate::{
    lookup_tables as lut, GameState, Move, MoveFlag, MoveInfo, PieceKind, PlayerSide, SquareIndex,
    SquareIter,
};

use super::{
    evaluate,
    minmax::{Depth, MAX_PLY},
};

const NUM_KILLER_MOVES: usize = 3;

/// Evaluation of a move, used to order promising moves.
/// Not to be confused with the evaluation of a position!
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MoveScore(i16);

#[derive(Default, Clone, Copy)]
pub struct MovePrediction {
    /// Evaluation of the move
    pub score: MoveScore,
    /// Recommended depth reduction
    pub reduce: Depth,
    /// Recommended depth extension
    pub extend: Depth,
}

pub struct MovePredictor {
    /// History of cutoffs from white moves
    history_white: [[u8; 64]; 6],
    /// History of cutoffs from black moves
    history_black: [[u8; 64]; 6],
    /// Killer moves that caused a cutoff, indexed by ply
    killer_moves: Vec<[Option<MoveInfo>; NUM_KILLER_MOVES]>,
}

/// Make a fast evaluation of a move.
pub fn eval(gs: &GameState, mv: Move) -> MoveScore {
    let mvi = mv.unwrap();
    const CAPTURE_MULT: i16 = 20;
    match gs.pieces.get(mvi.to) {
        Some((_, victim)) if !matches!(mvi.flag, MoveFlag::CastleEast | MoveFlag::CastleWest) => {
            // Static exchange evaluation for captures
            // Ranges from approx. -18000 to +18000
            let next_gs = gs.make_move_exchange_eval(mv);
            MoveScore(
                CAPTURE_MULT
                    * (evaluate::piece_value(victim)
                        - static_exchange_eval(&next_gs, mvi.to.mirror(), mvi.kind)),
            )
        }
        _ => MoveScore(0),
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
    pub fn new() -> Self {
        MovePredictor {
            history_white: [[0; 64]; 6],
            history_black: [[0; 64]; 6],
            killer_moves: vec![[None; NUM_KILLER_MOVES]; MAX_PLY as usize],
        }
    }

    /// Make a fast evaluation of a move, with a stateful evaluator for better accuracy.
    pub fn eval(&self, gs: &GameState, mv: Move, ply: u16) -> MovePrediction {
        let mvi = mv.unwrap();
        const KILLER_BONUS: i16 = 512;
        const CAPTURE_MULT: i16 = 20;
        const PRIORITIZE_BONUS: i16 = 14000;

        let next_gs = gs.make_move_exchange_eval(mv);
        let mut extension = 0;
        let mut reduction = 0;
        let mut score;
        match gs.pieces.get(mvi.to) {
            Some((_, victim))
                if !matches!(mvi.flag, MoveFlag::CastleEast | MoveFlag::CastleWest) =>
            {
                // Static exchange evaluation for captures
                // Ranges from approx. -18000 to +18000
                let capture_gain = evaluate::piece_value(victim)
                    - static_exchange_eval(&next_gs, mvi.to.mirror(), mvi.kind);
                if capture_gain >= 0 {
                    // Place winning and neutral captures above killer moves
                    score = CAPTURE_MULT * capture_gain + KILLER_BONUS + 1;
                } else {
                    // Place loosing captures after everything
                    score = CAPTURE_MULT * capture_gain;
                    reduction = 2;
                }
            }
            _ => {
                if let Some(i) = self.killer_moves[ply as usize]
                    .iter()
                    .position(|k| *k == Some(mvi))
                {
                    // Bonus for killer moves
                    // Ranges from approx 500 to 512
                    score = KILLER_BONUS - i as i16;
                } else {
                    // Bonus from history
                    // Ranges from 0 to 255
                    let history = match gs.side_to_move {
                        PlayerSide::White => &self.history_white,
                        PlayerSide::Black => &self.history_black,
                    };
                    score = history[mvi.kind as usize][mvi.to as usize] as i16;
                    reduction = 1;
                }
            }
        }
        if let Some(sq) = SquareIter(next_gs.friends_bb[PieceKind::King]).next() {
            let blockers = next_gs.friends_bb.union() | next_gs.enemies_bb.union();
            if lut::is_dangerous(sq, &next_gs.enemies_bb, blockers) {
                // Prioritize a move that puts the enemy king in check
                score += PRIORITIZE_BONUS;
                reduction = 0;
                extension = 1;
            }
        }
        match mvi.flag {
            // Order promotions: queen first, then knight, bishop and rook are useless
            MoveFlag::Promotion(PieceKind::Queen) => score += 2,
            MoveFlag::Promotion(PieceKind::Knight) => score += 1,
            MoveFlag::Promotion(_) => score = i16::MIN,
            _ => {}
        }
        MovePrediction {
            score: MoveScore(score),
            reduce: reduction.into(),
            extend: extension.into(),
        }
    }

    pub fn apply_cutoff_bonus(&mut self, gs: &GameState, mv: Move, ply: u16) {
        let mv = mv.unwrap();
        if gs.pieces.get(mv.to).is_none() {
            // Store as a killer move
            let killer_moves = &mut self.killer_moves[ply as usize];
            if !killer_moves[..NUM_KILLER_MOVES - 1].contains(&Some(mv)) {
                killer_moves.rotate_right(1);
                killer_moves[0] = Some(mv);
            }
            // Increase the history value
            let history = match gs.side_to_move {
                PlayerSide::White => &mut self.history_white,
                PlayerSide::Black => &mut self.history_black,
            };
            if history[mv.kind as usize][mv.to as usize] == u8::MAX {
                fade_out_values(history);
            }
            history[mv.kind as usize][mv.to as usize] += 1;
        }
    }
}

/// Divide all values in the history by 2, to gain more room for increase
fn fade_out_values(values: &mut [[u8; 64]; 6]) {
    for i in 0..6 {
        for j in 0..64 {
            values[i][j] >>= 1;
        }
    }
}
