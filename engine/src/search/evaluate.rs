use std::sync::atomic::{AtomicI16, Ordering};

use crate::{
    lookup_tables as lut, GameState,
    PieceKind::{self, *},
    SquareIndex, SquareIter,
};

/// Opaque score that can be compared with other scores.
/// Score::MAX represents a winning position. Score::MIN represents a losing position.
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
    /// Score of zero, assigned to stalemate situations
    pub const ZERO: Score = Score(0);
    /// Maximum score, opposite of LOSS
    pub const WIN: Score = Score(i16::MAX);
    /// Minimum score, when the active player is checkmated
    pub const LOSS: Score = Score(-i16::MAX);
    /// Slightly negative score, when the active player is forced to draw
    pub const FORCED_DRAW: Score = Score(-50);

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
        debug_assert!(self.0 != i16::MIN);
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
    let mut material_diff_midgame = 0;
    let mut material_diff_endgame = 0;
    let mut phase = 0;
    let blockers = gs.friends_bb.union() | gs.enemies_bb.union();
    let enemies_pawns = gs.enemies_bb[Pawn];
    let friends_pawns = gs.friends_bb[Pawn];
    for kind in PieceKind::iter() {
        // Add the value of friend pieces
        for sq in SquareIter(gs.friends_bb[kind]) {
            let (midgame_value, endgame_value, phase_value) =
                friend_piece_value(kind, sq, blockers, friends_pawns, enemies_pawns);
            material_diff_midgame += midgame_value;
            material_diff_endgame += endgame_value;
            phase += phase_value;
        }
        // Subtract the value of enemy pieces
        for sq in SquareIter(gs.enemies_bb[kind]) {
            let (midgame_value, endgame_value, phase_value) = friend_piece_value(
                kind,
                sq.mirror(),
                blockers.swap_bytes(),
                enemies_pawns.swap_bytes(),
                friends_pawns.swap_bytes(),
            );
            material_diff_midgame -= midgame_value;
            material_diff_endgame -= endgame_value;
            phase += phase_value;
        }
    }

    // Blend the midgame and engame material values
    // phase=24 is the beginning of the game, phase=0 is when no pieces are left
    let material_diff_blend = {
        let midgame_factor = phase.min(24);
        let endgame_factor = 24 - midgame_factor;
        let material_diff_midgame = material_diff_midgame as i32 * midgame_factor as i32;
        let material_diff_endgame = material_diff_endgame as i32 * endgame_factor as i32;
        ((material_diff_midgame + material_diff_endgame) / 24) as i16
    };
    Score(material_diff_blend)
}

/// Returns (midgame_value, endgame_value, phase_value)
fn friend_piece_value(
    kind: PieceKind,
    sq: SquareIndex,
    blockers: u64,
    friends_pawns: u64,
    enemies_pawns: u64,
) -> (i16, i16, i16) {
    let mobility_mask = !(lut::shift_se(enemies_pawns) | lut::shift_sw(enemies_pawns));
    let midgame_value;
    let endgame_value;
    let phase_value;
    match kind {
        Pawn => {
            let mut extra_value = 0;
            if lut::in_front(sq) & friends_pawns != 0 {
                extra_value -= 20 // Malus for multiple pawns on the same file
            }
            if lut::adjacent_files(sq) & friends_pawns == 0 {
                extra_value -= 20 // Malus for isolated pawns
            }
            if lut::in_front_and_adjacent_files(sq) & enemies_pawns == 0 {
                extra_value += PAWN_VALUE_ENDGAME[sq as usize] // Bonus for passed pawns
            }
            midgame_value = PAWN_VALUE_MIDGAME[sq as usize] + extra_value;
            endgame_value = PAWN_VALUE_ENDGAME[sq as usize] + extra_value;
            phase_value = 0;
        }
        // Knight, bishop, rook and queen receive a mobility bonus
        Knight => {
            let m = (lut::knight_reachable(sq) & mobility_mask).count_ones();
            midgame_value = KNIGHT_VALUE_MIDGAME[sq as usize] + mobility_curve(m, 40, -70);
            endgame_value = KNIGHT_VALUE_ENDGAME[sq as usize] + mobility_curve(m, 40, -70);
            phase_value = 1;
        }
        Bishop => {
            let m = (lut::bishop_reachable(sq, blockers) & mobility_mask).count_ones();
            midgame_value = BISHOP_VALUE_MIDGAME[sq as usize] + mobility_curve(m, 40, -50);
            endgame_value = BISHOP_VALUE_ENDGAME[sq as usize] + mobility_curve(m, 40, -50);
            phase_value = 1;
        }
        Rook => {
            let m = (lut::rook_reachable(sq, blockers) & mobility_mask).count_ones();
            midgame_value = ROOK_VALUE_MIDGAME[sq as usize] + mobility_curve(m, 30, -50);
            endgame_value = ROOK_VALUE_ENDGAME[sq as usize] + mobility_curve(m, 60, -80);
            phase_value = 2;
        }
        Queen => {
            let m = (lut::queen_reachable(sq, blockers) & mobility_mask).count_ones();
            midgame_value = QUEEN_VALUE_MIDGAME[sq as usize] + mobility_curve(m, 30, -40);
            endgame_value = QUEEN_VALUE_ENDGAME[sq as usize] + mobility_curve(m, 40, -40);
            phase_value = 4;
        }
        // Malus for a king that is too exposed
        King => {
            let m = (lut::queen_reachable(sq, blockers) & !blockers).count_ones();
            midgame_value = KING_VALUE_MIDGAME[sq as usize] + mobility_curve(m, -10, 20);
            endgame_value = KING_VALUE_ENDGAME[sq as usize];
            phase_value = 0;
        }
    }
    (midgame_value, endgame_value, phase_value)
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

/// Compute `a * sqrt(x) + b` where x must be in 0..=64
pub fn mobility_curve(x: u32, a: i16, b: i16) -> i16 {
    // Table of sqrt(x) * 256
    const LUT: [i16; 65] = [
        0, 256, 362, 443, 512, 572, 627, 677, 724, 768, 809, 849, 886, 923, 957, 991, 1024, 1055,
        1086, 1115, 1144, 1173, 1200, 1227, 1254, 1280, 1305, 1330, 1354, 1378, 1402, 1425, 1448,
        1470, 1492, 1514, 1536, 1557, 1578, 1598, 1619, 1639, 1659, 1678, 1698, 1717, 1736, 1755,
        1773, 1792, 1810, 1828, 1846, 1863, 1881, 1898, 1915, 1932, 1949, 1966, 1982, 1999, 2015,
        2031, 2048,
    ];
    let a = a as i32;
    let b = (b as i32) << 8;
    let y = LUT[x as usize] as i32 * a + b;
    (y >> 8) as i16
}

/////////////////////////////////////////////////////////////////////////////////////////
// Piece square tables: https://www.chessprogramming.org/Simplified_Evaluation_Function
/////////////////////////////////////////////////////////////////////////////////////////

#[rustfmt::skip]
const PAWN_VALUE_MIDGAME: [i16; 64] = [
    0,   0,   0,   0,   0,   0,   0,   0,
    54,  100, 84,  63,  63,  84,  100, 54,
    63,  96,  82,  79,  79,  82,  96,  63,
    56,  86,  82,  96,  96,  82,  86,  56,
    64,  97,  91,  104, 104, 91,  97,  64,
    69,  98,  123, 130, 130, 123, 98,  69,
    125, 166, 175, 163, 163, 175, 166, 125,
    0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_VALUE_ENDGAME: [i16; 64] = [
    0,   0,   0,   0,   0,   0,   0,   0,
    97,  99,  98,  105, 105, 98,  99,  97,
    92,  97,  89,  94,  94,  89,  97,  92,
    100, 100, 89,  87,  87,  89,  100, 100,
    118, 114, 102, 95,  95,  102, 114, 118,
    183, 185, 163, 155, 155, 163, 185, 183,
    276, 263, 239, 234, 234, 239, 263, 276,
    0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_VALUE_MIDGAME: [i16; 64] = [
    273, 317, 294, 312, 312, 294, 317, 273,
    313, 304, 340, 335, 335, 340, 304, 313,
    318, 345, 351, 351, 351, 351, 345, 318,
    327, 349, 354, 357, 357, 354, 349, 327,
    343, 354, 381, 382, 382, 381, 354, 343,
    336, 403, 420, 411, 411, 420, 403, 336,
    292, 320, 404, 366, 366, 404, 320, 292,
    200, 285, 272, 343, 343, 272, 285, 200,
];

#[rustfmt::skip]
const KNIGHT_VALUE_ENDGAME: [i16; 64] = [
    235, 231, 261, 263, 263, 261, 231, 235,
    238, 260, 266, 278, 278, 266, 260, 238,
    259, 270, 279, 293, 293, 279, 270, 259,
    263, 280, 297, 301, 301, 297, 280, 263,
    264, 286, 297, 303, 303, 297, 286, 264,
    249, 262, 281, 285, 285, 281, 262, 249,
    243, 265, 256, 276, 276, 256, 265, 243,
    203, 231, 261, 252, 252, 261, 231, 203,
];

#[rustfmt::skip]
const BISHOP_VALUE_MIDGAME: [i16; 64] = [
    338, 344, 352, 348, 348, 352, 344, 338,
    367, 389, 383, 368, 368, 383, 389, 367,
    370, 381, 386, 379, 379, 386, 381, 370,
    364, 376, 377, 395, 395, 377, 376, 364,
    362, 371, 393, 408, 408, 393, 371, 362,
    356, 402, 411, 402, 402, 411, 402, 356,
    329, 382, 385, 373, 373, 385, 382, 329,
    347, 370, 303, 334, 334, 303, 370, 347,
];

#[rustfmt::skip]
const BISHOP_VALUE_ENDGAME: [i16; 64] = [
    277, 290, 278, 290, 290, 278, 290, 277,
    277, 281, 289, 298, 298, 289, 281, 277,
    284, 292, 302, 308, 308, 302, 292, 284,
    290, 297, 308, 310, 310, 308, 297, 290,
    297, 303, 308, 308, 308, 308, 303, 297,
    300, 293, 300, 296, 296, 300, 293, 300,
    286, 293, 294, 290, 290, 294, 293, 286,
    278, 278, 287, 290, 290, 287, 278, 278,
];

#[rustfmt::skip]
const ROOK_VALUE_MIDGAME: [i16; 64] = [
    455, 452, 481, 493, 493, 481, 452, 455,
    420, 466, 473, 472, 472, 473, 466, 420,
    438, 462, 469, 470, 470, 469, 462, 438,
    448, 467, 468, 481, 481, 468, 467, 448,
    455, 468, 498, 502, 502, 498, 468, 455,
    482, 517, 512, 503, 503, 512, 517, 482,
    512, 506, 539, 548, 548, 539, 506, 512,
    514, 513, 497, 534, 534, 497, 513, 514,
];

#[rustfmt::skip]
const ROOK_VALUE_ENDGAME: [i16; 64] = [
    498, 515, 507, 509, 509, 507, 515, 498,
    508, 504, 508, 509, 509, 508, 504, 508,
    502, 508, 504, 508, 508, 504, 508, 502,
    508, 511, 513, 512, 512, 513, 511, 508,
    515, 513, 519, 513, 513, 519, 513, 515,
    514, 513, 514, 516, 516, 514, 513, 514,
    519, 522, 520, 516, 516, 520, 522, 519,
    521, 521, 527, 525, 525, 527, 521, 521,
];

#[rustfmt::skip]
const QUEEN_VALUE_MIDGAME: [i16; 64] = [
    1000, 1001, 1008, 1023, 1023, 1008, 1001, 1000,
    1008, 1020, 1038, 1030, 1030, 1038, 1020, 1008,
    1021, 1033, 1021, 1022, 1022, 1021, 1033, 1021,
    1019, 1014, 1019, 1019, 1019, 1019, 1014, 1019,
    1012, 1011, 1025, 1017, 1017, 1025, 1011, 1012,
    1047, 1040, 1056, 1043, 1043, 1056, 1040, 1047,
    1040, 1020, 1051, 1018, 1018, 1051, 1020, 1040,
    1033, 1046, 1061, 1060, 1060, 1061, 1046, 1033,
];

#[rustfmt::skip]
const QUEEN_VALUE_ENDGAME: [i16; 64] = [
    899, 912, 909, 912, 912, 909, 912, 899,
    909, 907, 910, 920, 920, 910, 907, 909,
    931, 928, 952, 943, 943, 952, 928, 931,
    938, 969, 962, 975, 975, 962, 969, 938,
    955, 975, 968, 987, 987, 968, 975, 955,
    931, 948, 958, 984, 984, 958, 948, 931,
    928, 961, 964, 985, 985, 964, 961, 928,
    941, 952, 956, 963, 963, 956, 952, 941,
];

#[rustfmt::skip]
const KING_VALUE_MIDGAME: [i16; 64] = [
     0,   30, -8,  -23, -23, -8,   30,  0,
     4,   8,  -12, -53, -53, -12,  8,   4,
    -20, -14, -26, -45, -45, -26, -14, -20,
    -50, -17, -35, -42, -42, -35, -17, -50,
    -26, -17, -18, -28, -28, -18, -17, -26,
    -15,  23,  4,  -18, -18,  4,   23, -15,
     0,  -19, -12, -7,  -7,  -12, -19,  0,
    -26,  12, -9,  -35, -35, -9,   12, -26,
    
];

#[rustfmt::skip]
const KING_VALUE_ENDGAME: [i16; 64] = [
    -48, -29, -17, -19, -19, -17, -29, -48,
    -22, -8,   4,   13,  13,  4,  -8,  -22,
    -14,  2,   13,  22,  22,  13,  2,  -14,
    -14,  2,   22,  25,  25,  22,  2,  -14,
    -2,   24,  28,  26,  26,  28,  24, -2,
     11,  30,  34,  17,  17,  34,  30,  11,
     0,   20,  26,  17,  17,  26,  20,  0,
    -45, -15, -1,  -14, -14, -1,  -15, -45,
];
