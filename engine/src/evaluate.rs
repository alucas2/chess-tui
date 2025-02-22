use std::sync::atomic::{AtomicI16, Ordering};

use crate::{GameState, PieceKind, SquareIter};

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
    /// Position is a win in the specified number of moves
    Win(u16),
    /// Position is a loss in the specified number of moves
    Loose(u16),
}

impl Score {
    pub const ZERO: Score = Score(0);
    pub const MAX: Score = Score(i16::MAX);
    pub const MIN: Score = Score(-i16::MAX);
    pub const NEG_INF: Score = Score(i16::MIN);
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
    let mut diff_midgame = 0;
    let mut diff_endgame = 0;
    let mut total_material = 0;
    for kind in PieceKind::iter() {
        let table_midgame = piece_value_table_midgame(kind);
        let table_endgame = piece_value_table_endgame(kind);
        let flat_value = piece_value(kind);
        for sq in SquareIter(gs.friends_bb[kind]) {
            diff_midgame += table_midgame[sq as usize];
            diff_endgame += table_endgame[sq as usize];
            total_material += flat_value;
        }
        for sq in SquareIter(gs.enemies_bb[kind]) {
            diff_midgame -= table_midgame[sq.mirror() as usize];
            diff_endgame -= table_endgame[sq.mirror() as usize];
            total_material += flat_value;
        }
    }

    // Blend the midgame and engame material values
    // 10000 is considered the beginning of the game, 6000 the end of the game
    let midgame_factor = ((total_material as f32 - 6000.0) / 4000.0).clamp(0.0, 1.0);
    let diff_midgame = diff_midgame as f32 * midgame_factor;
    let diff_endgame = diff_endgame as f32 * (1.0 - midgame_factor);
    Score((diff_midgame + diff_endgame).round() as i16)
}

/// Get the piece-square value table for a kind of piece.
/// The table is indexed from white's point of view.
pub fn piece_value_table_midgame(kind: PieceKind) -> &'static [i16; 64] {
    match kind {
        PieceKind::Pawn => &PAWN_VALUE,
        PieceKind::Knight => &KNIGHT_VALUE,
        PieceKind::Bishop => &BISHOP_VALUE,
        PieceKind::Rook => &ROOK_VALUE,
        PieceKind::Queen => &QUEEN_VALUE,
        PieceKind::King => &KING_VALUE,
    }
}

/// Get the piece-square value table for a kind of piece.
/// The table is indexed from white's point of view.
pub fn piece_value_table_endgame(kind: PieceKind) -> &'static [i16; 64] {
    match kind {
        PieceKind::Pawn => &PAWN_VALUE_ENDGAME,
        PieceKind::Knight => &KNIGHT_VALUE,
        PieceKind::Bishop => &BISHOP_VALUE,
        PieceKind::Rook => &ROOK_VALUE,
        PieceKind::Queen => &QUEEN_VALUE,
        PieceKind::King => &KING_VALUE_ENDGAME,
    }
}

/// Get the flat value of a kind of piece
pub fn piece_value(kind: PieceKind) -> i16 {
    match kind {
        PieceKind::Pawn => 100,
        PieceKind::Knight => 320,
        PieceKind::Bishop => 330,
        PieceKind::Rook => 500,
        PieceKind::Queen => 900,
        PieceKind::King => 1000,
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
