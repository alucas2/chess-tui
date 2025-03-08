use std::sync::atomic::{AtomicI16, Ordering};

use crate::{
    lookup_tables as lut, FileIndex, GameState,
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

impl Score {
    pub const ZERO: Score = Score(0);
    pub const MAX: Score = Score(i16::MAX);
    pub const MIN: Score = Score(-i16::MAX);
    pub const NEG_INF: Score = Score(i16::MIN);

    /// Create a minimal window of (score-1, score)
    pub fn minimal_window(&self) -> (Score, Score) {
        assert!(*self != Self::NEG_INF);
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
    for kind in PieceKind::iter() {
        // Add the value of friend pieces
        for sq in SquareIter(gs.friends_bb[kind]) {
            let (midgame_value, endgame_value, extra_value) =
                friend_piece_value(kind, sq, blockers, gs.enemies_bb[Pawn]);
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
                gs.friends_bb[Pawn].swap_bytes(),
            );
            total_material += piece_value(kind);
            material_diff_midgame -= midgame_value;
            material_diff_endgame -= endgame_value;
            bonus -= extra_value;
        }
    }

    // Malus for multiple pawns on the same file
    for file in FileIndex::iter() {
        let bb = file.bb().get();
        if (gs.friends_bb[Pawn] & bb).count_ones() > 1 {
            bonus -= piece_value(Pawn) / 2;
        }
        if (gs.enemies_bb[Pawn] & bb).count_ones() > 1 {
            bonus += piece_value(Pawn) / 2;
        }
    }

    // Blend the midgame and engame material values
    // 10000 is considered the beginning of the game, 5000 the end of the game
    let material_diff_blend = {
        let midgame_factor = (total_material - 5000).max(0).saturating_mul(8);
        let endgame_factor = i16::MAX - midgame_factor;
        let material_diff_midgame = (material_diff_midgame as i32 * midgame_factor as i32) >> 16;
        let material_diff_endgame = (material_diff_endgame as i32 * endgame_factor as i32) >> 16;
        (material_diff_midgame + material_diff_endgame) as i16
    };
    Score(material_diff_blend + bonus)
}

fn friend_piece_value(
    kind: PieceKind,
    sq: SquareIndex,
    blockers: u64,
    enemy_pawns: u64,
) -> (i16, i16, i16) {
    let enemy_pawn_safe = !(lut::shift_se(enemy_pawns) | lut::shift_sw(enemy_pawns));
    let midgame_value = piece_value_table_midgame(kind)[sq as usize];
    let endgame_value = piece_value_table_endgame(kind)[sq as usize];
    let extra_value = match kind {
        Knight => 2 * (lut::knight_reachable(sq) & enemy_pawn_safe).count_ones() as i16,
        Bishop => 4 * (lut::bishop_reachable(sq, blockers) & enemy_pawn_safe).count_ones() as i16,
        Rook => 4 * (lut::rook_reachable(sq, blockers) & enemy_pawn_safe).count_ones() as i16,
        Queen => {
            2 * ((lut::bishop_reachable(sq, blockers) | lut::rook_reachable(sq, blockers))
                & enemy_pawn_safe)
                .count_ones() as i16
        }
        _ => 0,
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
