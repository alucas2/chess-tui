use engine::{CastleSide, FileIndex, GameState, PieceKind, PlayerSide, RankIndex, SquareIndex};

pub fn initial_position() -> GameState {
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        .parse()
        .unwrap()
}

pub fn random_chess960_position() -> GameState {
    let mut gs: GameState = "8/pppppppp/8/8/8/8/PPPPPPPP/8 w - - 0 1".parse().unwrap();

    // Place the bishops
    for i in 0..2 {
        let file = FileIndex::from_index(rand::random_range(0..4) * 2 + i).unwrap();
        gs.set_piece(
            SquareIndex::from_coords(file, RankIndex::_1),
            Some((PlayerSide::White, PieceKind::Bishop)),
        );
        gs.set_piece(
            SquareIndex::from_coords(file, RankIndex::_8),
            Some((PlayerSide::Black, PieceKind::Bishop)),
        );
    }

    // Place the knights and queens
    for kind in [PieceKind::Knight, PieceKind::Knight, PieceKind::Queen] {
        let file = std::iter::repeat_with(|| rand::random_range(0..8))
            .map(|x| FileIndex::from_index(x).unwrap())
            .find(|x| {
                gs.piece(SquareIndex::from_coords(*x, RankIndex::_1))
                    .is_none()
            })
            .unwrap();
        gs.set_piece(
            SquareIndex::from_coords(file, RankIndex::_1),
            Some((PlayerSide::White, kind)),
        );
        gs.set_piece(
            SquareIndex::from_coords(file, RankIndex::_8),
            Some((PlayerSide::Black, kind)),
        );
    }

    // Place the rooks and kings
    for kind in [PieceKind::Rook, PieceKind::King, PieceKind::Rook] {
        let file = FileIndex::iter()
            .find(|x| {
                gs.piece(SquareIndex::from_coords(*x, RankIndex::_1))
                    .is_none()
            })
            .unwrap();
        gs.set_piece(
            SquareIndex::from_coords(file, RankIndex::_1),
            Some((PlayerSide::White, kind)),
        );
        gs.set_piece(
            SquareIndex::from_coords(file, RankIndex::_8),
            Some((PlayerSide::Black, kind)),
        );
    }

    // Set the castle rights
    let west_rook = FileIndex::iter().find(|x| {
        matches!(
            gs.piece(SquareIndex::from_coords(*x, RankIndex::_1)),
            Some((_, PieceKind::Rook)),
        )
    });
    gs.castle_mut(PlayerSide::White)
        .set(CastleSide::West, west_rook);
    gs.castle_mut(PlayerSide::Black)
        .set(CastleSide::West, west_rook);
    let east_rook = FileIndex::iter().rev().find(|x| {
        matches!(
            gs.piece(SquareIndex::from_coords(*x, RankIndex::_1)),
            Some((_, PieceKind::Rook)),
        )
    });
    gs.castle_mut(PlayerSide::White)
        .set(CastleSide::East, east_rook);
    gs.castle_mut(PlayerSide::Black)
        .set(CastleSide::East, east_rook);

    gs
}
