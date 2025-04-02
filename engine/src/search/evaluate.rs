use std::sync::atomic::{AtomicI16, Ordering};

use crate::{
    lookup_tables as lut, GameState,
    PieceKind::{self, *},
    SquareIndex, SquareIter,
};

/// Opaque score that can be compared with other scores.
/// Score::MAX represents a winning position. Score::MIN represents a losing position.
/// Score::NEG_INF acts like "negative infinity", which is a placeholder invalid score.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Score(pub(crate) i16);

/// Atomic version of score
#[derive(Debug)]
#[repr(transparent)]
pub struct AtomicScore(AtomicI16);

#[derive(Debug, Clone, Copy)]
pub enum ScoreInfo {
    Normal(i16),
    /// Position is a win in the specified number of halfmoves
    Win(u16),
    /// Position is a loss in the specified number of halfmoves
    Loss(u16),
}

impl Score {
    pub const ZERO: Score = Score(0);
    pub const WIN: Score = Score(i16::MAX);
    pub const LOSS: Score = Score(-i16::MAX);
    pub const NEG_INF: Score = Score(i16::MIN);

    /// Add the specified number of plies to the score, such that:
    /// - Wins with a lower number of plies are favored
    /// - Losses with a higher number of plies are favored
    /// Other scores are not affected
    pub fn add_ply(&self, ply: u16) -> Score {
        if self.0 > i16::MAX - 200 {
            Score(self.0 - ply as i16)
        } else if self.0 < -i16::MAX + 200 {
            Score(self.0 + ply as i16)
        } else {
            *self
        }
    }

    /// Opposite of add_ply, must be used before storing a score in the transposition
    /// table to remove the score's dependency on the ply number.
    pub fn sub_ply(&self, ply: u16) -> Score {
        debug_assert!(self.0 <= i16::MAX - ply as i16);
        debug_assert!(self.0 >= -i16::MAX + ply as i16);
        if self.0 > i16::MAX - 200 {
            Score(self.0 + ply as i16)
        } else if self.0 < -i16::MAX + 200 {
            Score(self.0 - ply as i16)
        } else {
            *self
        }
    }

    pub fn info(&self) -> ScoreInfo {
        if self.0 > i16::MAX - 200 {
            ScoreInfo::Win((i16::MAX - self.0) as u16)
        } else if self.0 < -i16::MAX + 200 {
            ScoreInfo::Loss((i16::MAX + self.0) as u16)
        } else {
            ScoreInfo::Normal(self.0)
        }
    }

    /// Create a minimal window of (score-1, score)
    pub fn minimal_window(&self) -> (Score, Score) {
        debug_assert!(*self != Self::NEG_INF);
        let alpha = Score(self.0 - 1);
        let beta = *self;
        (alpha, beta)
    }
}

impl std::ops::Neg for Score {
    type Output = Score;

    fn neg(self) -> Self::Output {
        Score(-self.0)
    }
}

impl AtomicScore {
    pub fn new(val: Score) -> Self {
        AtomicScore(AtomicI16::new(val.0))
    }

    pub fn load(&self) -> Score {
        Score(self.0.load(Ordering::Relaxed))
    }

    pub fn fetch_max(&self, val: Score) -> Score {
        Score(self.0.fetch_max(val.0, Ordering::Relaxed))
    }
}

pub fn eval(gs: &GameState) -> Score {
    let mut total_material = 0;
    let mut material_diff_midgame = 0;
    let mut material_diff_endgame = 0;
    let mut bonus = 0;
    let blockers = gs.friends_bb.union() | gs.enemies_bb.union();
    let enemies_pawns = gs.enemies_bb[Pawn];
    let friends_pawns = gs.friends_bb[Pawn];
    for kind in PieceKind::iter() {
        // Add the value of friend pieces
        for sq in SquareIter(gs.friends_bb[kind]) {
            let (midgame_value, endgame_value, extra_value) =
                friend_piece_value(kind, sq, blockers, friends_pawns, enemies_pawns);
            total_material += piece_value(kind);
            material_diff_midgame += midgame_value;
            material_diff_endgame += endgame_value;
            bonus += extra_value;
        }
        // Subtract the value of enemy pieces
        for sq in SquareIter(gs.enemies_bb[kind]) {
            let (midgame_value, endgame_value, extra_value) = friend_piece_value(
                kind,
                sq.mirror(),
                blockers.swap_bytes(),
                enemies_pawns.swap_bytes(),
                friends_pawns.swap_bytes(),
            );
            total_material += piece_value(kind);
            material_diff_midgame -= midgame_value;
            material_diff_endgame -= endgame_value;
            bonus -= extra_value;
        }
    }

    // Blend the midgame and engame material values
    // 10000 is considered the beginning of the game, 5000 the end of the game
    let material_diff_blend = {
        let midgame_factor = (total_material - 5000).max(0).saturating_mul(8);
        let endgame_factor = i16::MAX - midgame_factor;
        let material_diff_midgame = (material_diff_midgame as i32 * midgame_factor as i32) >> 15;
        let material_diff_endgame = (material_diff_endgame as i32 * endgame_factor as i32) >> 15;
        (material_diff_midgame + material_diff_endgame) as i16
    };
    Score(material_diff_blend + bonus)
}

fn friend_piece_value(
    kind: PieceKind,
    sq: SquareIndex,
    blockers: u64,
    friends_pawns: u64,
    enemies_pawns: u64,
) -> (i16, i16, i16) {
    let mobility_mask = !(lut::shift_se(enemies_pawns) | lut::shift_sw(enemies_pawns));
    let midgame_value = piece_value_table_midgame(kind)[sq as usize];
    let endgame_value = piece_value_table_endgame(kind)[sq as usize];
    let extra_value = match kind {
        Pawn => {
            let mut pawn_score = 0;
            if lut::in_front(sq) & friends_pawns != 0 {
                pawn_score -= 20 // Malus for multiple pawns on the same file
            }
            if lut::adjacent_files(sq) & friends_pawns == 0 {
                pawn_score -= 20 // Malus for isolated pawns
            }
            if lut::in_front_and_adjacent_files(sq) & enemies_pawns == 0 {
                pawn_score += 50 // Bonus for passed pawns
            }
            pawn_score
        }
        // Knight, bishop, rook and queen receive a mobility bonus
        Knight => 2 * (lut::knight_reachable(sq) & mobility_mask).count_ones() as i16,
        Bishop => 4 * (lut::bishop_reachable(sq, blockers) & mobility_mask).count_ones() as i16,
        Rook => 4 * (lut::rook_reachable(sq, blockers) & mobility_mask).count_ones() as i16,
        Queen => {
            2 * ((lut::bishop_reachable(sq, blockers) | lut::rook_reachable(sq, blockers))
                & mobility_mask)
                .count_ones() as i16
        }
        // Malus for a king that is too exposed
        King => {
            -1 * (lut::bishop_reachable(sq, blockers) | lut::rook_reachable(sq, blockers))
                .count_ones() as i16
        }
    };
    (midgame_value, endgame_value, extra_value)
}

/// Get the piece-square value table for a kind of piece.
/// The table is indexed from white's point of view.
pub fn piece_value_table_midgame(kind: PieceKind) -> &'static [i16; 64] {
    match kind {
        Pawn => &PAWN_VALUE,
        Knight => &KNIGHT_VALUE,
        Bishop => &BISHOP_VALUE,
        Rook => &ROOK_VALUE,
        Queen => &QUEEN_VALUE,
        King => &KING_VALUE,
    }
}

/// Get the piece-square value table for a kind of piece.
/// The table is indexed from white's point of view.
pub fn piece_value_table_endgame(kind: PieceKind) -> &'static [i16; 64] {
    match kind {
        Pawn => &PAWN_VALUE_ENDGAME,
        Knight => &KNIGHT_VALUE,
        Bishop => &BISHOP_VALUE,
        Rook => &ROOK_VALUE,
        Queen => &QUEEN_VALUE,
        King => &KING_VALUE_ENDGAME,
    }
}

/// Get the flat value of a kind of piece
pub fn piece_value(kind: PieceKind) -> i16 {
    match kind {
        Pawn => 100,
        Knight => 320,
        Bishop => 330,
        Rook => 500,
        Queen => 900,
        King => 1000,
    }
}

/////////////////////////////////////////////////////////////////////////////////////////
// Piece square tables: https://www.chessprogramming.org/Simplified_Evaluation_Function
/////////////////////////////////////////////////////////////////////////////////////////

#[rustfmt::skip]
const PAWN_VALUE: [i16; 64] = [
    0,   0,   0,   0,   0,   0,   0,   0,
    105, 110, 110, 80,  80,  110, 110, 105,
    105, 95,  90,  100, 100, 90,  95,  105,
    100, 100, 100, 120, 120, 100, 100, 100,
    105, 105, 110, 125, 125, 110, 105, 105,
    110, 110, 120, 130, 130, 120, 110, 110,
    150, 150, 150, 150, 150, 150, 150, 150,
    0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_VALUE_ENDGAME: [i16; 64] = [
    0,   0,   0,   0,   0,   0,   0,   0,
    80,  80,  80,  80,  80,  80,  80,  80,
    90,  90,  90,  90,  90,  90,  90,  90,
    120, 120, 120, 120, 120, 120, 120, 120,
    140, 140, 140, 140, 140, 140, 140, 140,
    160, 160, 160, 160, 160, 160, 160, 160,
    200, 200, 200, 200, 200, 200, 200, 200,
    0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_VALUE: [i16; 64] = [
    270, 280, 290, 290, 290, 290, 280, 270,
    280, 300, 320, 325, 325, 320, 300, 280,
    290, 300, 330, 335, 335, 330, 300, 290,
    290, 320, 335, 340, 340, 335, 320, 290,
    290, 325, 335, 340, 340, 335, 325, 290,
    290, 320, 330, 335, 335, 330, 320, 290,
    280, 300, 320, 320, 320, 320, 300, 280,
    270, 280, 290, 290, 290, 290, 280, 270,
];

#[rustfmt::skip]
const BISHOP_VALUE: [i16; 64] = [
    310, 320, 320, 320, 320, 320, 320, 310,
    320, 335, 330, 330, 330, 330, 335, 320,
    320, 340, 340, 340, 340, 340, 340, 320,
    320, 330, 340, 340, 340, 340, 330, 320,
    320, 335, 335, 340, 340, 335, 335, 320,
    320, 330, 335, 340, 340, 335, 330, 320,
    320, 330, 330, 330, 330, 330, 330, 320,
    310, 320, 320, 320, 320, 320, 320, 310,    
];

#[rustfmt::skip]
const ROOK_VALUE: [i16; 64] = [
    500, 500, 500, 505, 505, 500, 500, 500,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    505, 510, 510, 510, 510, 510, 510, 505,
    500, 500, 500, 500, 500, 500, 500, 500,
];

#[rustfmt::skip]
const QUEEN_VALUE: [i16; 64] = [
    880, 890, 890, 895, 895, 890, 890, 880,
    890, 900, 900, 900, 900, 900, 900, 890,
    890, 900, 905, 905, 905, 905, 900, 890,
    890, 900, 905, 905, 905, 905, 900, 890,
    890, 900, 905, 905, 905, 905, 900, 890,
    890, 900, 905, 905, 905, 905, 900, 890,
    890, 900, 900, 900, 900, 900, 900, 890,
    880, 890, 890, 895, 895, 890, 890, 880,
];

#[rustfmt::skip]
const KING_VALUE: [i16; 64] = [
    1010, 1010, 1010, 1000, 1000, 1010, 1010, 1010,
    1010, 1010, 1000, 1000, 1000, 1000, 1010, 1010,
    990,  980,  980,  980,  980,  980,  980,  990,
    980,  970,  970,  960,  960,  970,  970,  980,
    970,  960,  960,  950,  950,  960,  960,  970,
    970,  960,  960,  950,  950,  960,  960,  970,
    970,  960,  960,  950,  950,  960,  960,  970,
    970,  960,  960,  950,  950,  960,  960,  970,
];

#[rustfmt::skip]
const KING_VALUE_ENDGAME: [i16; 64] = [
    950,  970,  970,  970,  970,  970,  970,  950,
    970,  980,  990,  1000, 1000, 990,  980,  970,
    970,  990,  1020, 1030, 1030, 1020, 990,  970,
    970,  990,  1030, 1040, 1040, 1030, 990,  970,
    970,  990,  1030, 1040, 1040, 1030, 990,  970,
    970,  990,  1020, 1030, 1030, 1020, 990,  970,
    970,  980,  990,  1000, 1000, 990,  980,  970,
    950,  970,  970,  970,  970,  970,  970,  950,
];
