use crate::{GameState, Move, MoveFlag, PieceKind};

/// Opaque score that can be compared with other scores.
/// Score::MAX represents a winning position. Score::MIN represents a losing position.
/// Score::NEG_INF acts like "negative infinity", which is a placeholder invalid score.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct Score(i16);

impl Score {
    pub(crate) const ZERO: Score = Score(0);
    pub(crate) const MAX: Score = Score(i16::MAX);
    pub(crate) const MIN: Score = Score(-i16::MAX);
    pub(crate) const NEG_INF: Score = Score(i16::MIN);

    pub(crate) fn get(&self) -> i16 {
        self.0
    }
}

impl std::ops::Neg for Score {
    type Output = Score;

    fn neg(self) -> Self::Output {
        Score(-self.0)
    }
}

fn piece_value(kind: PieceKind) -> i16 {
    match kind {
        PieceKind::Pawn => 100,
        PieceKind::Knight => 300,
        PieceKind::Bishop => 330,
        PieceKind::Rook => 500,
        PieceKind::Queen => 900,
        PieceKind::King => 1000,
    }
}

/// Make a fast evaluation of a gamestate
pub fn eval_heuristic(gs: &GameState) -> Score {
    let mut score = 0;

    // Sum the friend material
    for kind in PieceKind::iter() {
        score += (gs.friends_bb[kind]).count_ones() as i16 * piece_value(kind);
    }

    // Subtract the enemy material
    for kind in PieceKind::iter() {
        score -= (gs.enemies_bb[kind]).count_ones() as i16 * piece_value(kind);
    }

    Score(score)
}

/// Make a fast evaluation of a move.
/// NOTE: The "score" returned by this function has nothing to do with
/// the "score" of a gamestate, the latter being represented by the `Score` type.
pub(crate) fn eval_move(gs: &GameState, mv: Move) -> i16 {
    let mut score = 0;

    // Bonus for capturing an enemy with a cheap friend
    if let Some((_, kind)) = gs.pieces.get(mv.dst) {
        score += 1000 + piece_value(kind) - piece_value(mv.who);
    }

    // Bonus for promoting a piece
    if let MoveFlag::Promotion(kind) = mv.flag {
        score += 2000 + piece_value(kind)
    }

    score
}
