use crate::{CastleSide, FileIndex, PieceKind, PlayerSide, SquareIndex};

/// State of the game.
///
/// The internal representation uses the terms "friend" and "enemy".
/// It is laid out in memory as if "friend" was the white player, but if "friend" is actually the
/// black player, then the internal representation is the mirror of the reality.
#[derive(Debug, Default, Clone, Copy)]
pub struct GameState {
    /// Representation of the board as an array
    pub(crate) pieces: CompactPieceArray,
    /// Bitboard mask for each kind of friendly piece
    pub(crate) friends_bb: PieceBitboards,
    /// Bitboard mask for each kind of enemy piece
    pub(crate) enemies_bb: PieceBitboards,
    /// Color of the player to move
    pub(crate) side_to_move: PlayerSide,
    /// Castle rights of the friends
    pub(crate) friends_castle: CastleRights,
    /// Castle rights of the enemies
    pub(crate) enemies_castle: CastleRights,
    /// File of the potential en passant capture
    pub(crate) en_passant: Option<FileIndex>,
    /// Number of full moves, incremented after black has played
    pub(crate) fullmoves_count: u16,
    // Number of moves since the last capture or pawn push
    pub(crate) fiftymove_count: u16,
}

/// Minimal amount of data that can uniqely identify a GameState.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GameStateKey {
    pieces: CompactPieceArray,
    side_to_move: PlayerSide,
    friends_castle: CastleRights,
    enemies_castle: CastleRights,
    en_passant: Option<FileIndex>,
}

/// Array of 6 bitboards, one for each piece kind
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct PieceBitboards([u64; 6]);

/// Array of 64 squares containing a piece kind and a player side.
/// Uses a compact representation of 4 bits per square
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct CompactPieceArray([u64; 4]);

/// Compact representation of various gamestate flags
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CastleRights(u8);

impl GameState {
    pub fn key(&self) -> GameStateKey {
        GameStateKey {
            pieces: self.pieces,
            side_to_move: self.side_to_move,
            friends_castle: self.friends_castle,
            enemies_castle: self.enemies_castle,
            en_passant: self.en_passant,
        }
    }

    pub fn piece(&self, square: SquareIndex) -> Option<(PlayerSide, PieceKind)> {
        let square = match self.side_to_move {
            PlayerSide::White => square,
            PlayerSide::Black => square.mirror(),
        };
        self.pieces.get(square)
    }

    pub fn set_piece(&mut self, square: SquareIndex, piece: Option<(PlayerSide, PieceKind)>) {
        let square = match self.side_to_move {
            PlayerSide::White => square,
            PlayerSide::Black => square.mirror(),
        };
        if let Some((side, kind)) = self.pieces.get(square) {
            self.remove_piece(square, side, kind);
        }
        if let Some((side, kind)) = piece {
            self.put_piece(square, side, kind);
        }
    }

    pub fn side_to_move(&self) -> PlayerSide {
        self.side_to_move
    }

    pub fn set_side_to_move(&mut self, side: PlayerSide) {
        if self.side_to_move != side {
            *self = GameState {
                pieces: self.pieces.mirror(),
                friends_bb: self.enemies_bb.mirror(),
                enemies_bb: self.friends_bb.mirror(),
                side_to_move: self.side_to_move.opposite(),
                friends_castle: self.enemies_castle,
                enemies_castle: self.friends_castle,
                en_passant: self.en_passant,
                fullmoves_count: self.fullmoves_count,
                fiftymove_count: self.fiftymove_count,
            }
        }
    }

    pub fn castle(&self, side: PlayerSide) -> &CastleRights {
        if side == self.side_to_move {
            &self.friends_castle
        } else {
            &self.enemies_castle
        }
    }

    pub fn castle_mut(&mut self, side: PlayerSide) -> &mut CastleRights {
        if side == self.side_to_move {
            &mut self.friends_castle
        } else {
            &mut self.enemies_castle
        }
    }

    pub fn en_passant_target(&self) -> Option<FileIndex> {
        self.en_passant
    }

    pub fn set_en_passant_target(&mut self, value: Option<FileIndex>) {
        self.en_passant = value
    }

    pub fn fullmoves_count(&self) -> u16 {
        self.fullmoves_count
    }

    pub fn set_fullmoves_count(&mut self, value: u16) {
        self.fullmoves_count = value;
    }

    pub fn fiftymove_count(&self) -> u16 {
        self.fiftymove_count
    }

    pub fn set_fiftymove_count(&mut self, value: u16) {
        self.fiftymove_count = value
    }

    /// Remove a piece from the board while keeping the internal data consistent.
    /// Ensure that the expected piece is on specified square before calling this function,
    /// or else corruption will occur.
    pub(crate) fn remove_piece(&mut self, at: SquareIndex, side: PlayerSide, kind: PieceKind) {
        self.pieces.set(at, None);
        if side == self.side_to_move {
            self.friends_bb[kind] ^= at.bb().get();
        } else {
            self.enemies_bb[kind] ^= at.bb().get();
        }
    }

    /// Put a piece on the board while keepking the internal data consistent.
    /// Ensure that no piece occupies the specified square before calling this function,
    /// or else corruption will occur.
    pub(crate) fn put_piece(&mut self, at: SquareIndex, side: PlayerSide, kind: PieceKind) {
        self.pieces.set(at, Some((side, kind)));
        if side == self.side_to_move {
            self.friends_bb[kind] ^= at.bb().get();
        } else {
            self.enemies_bb[kind] ^= at.bb().get();
        }
    }
}

impl PieceBitboards {
    pub(crate) fn mirror(self) -> PieceBitboards {
        PieceBitboards(self.0.map(u64::swap_bytes))
    }

    pub(crate) fn union(self) -> u64 {
        self.0.iter().fold(0, |a, b| a | b)
    }
}

impl std::ops::Index<PieceKind> for PieceBitboards {
    type Output = u64;

    fn index(&self, kind: PieceKind) -> &u64 {
        &self.0[kind as usize]
    }
}

impl std::ops::IndexMut<PieceKind> for PieceBitboards {
    fn index_mut(&mut self, kind: PieceKind) -> &mut u64 {
        &mut self.0[kind as usize]
    }
}

impl Default for CompactPieceArray {
    fn default() -> Self {
        CompactPieceArray([0xffffffffffffffff; 4])
    }
}

impl CompactPieceArray {
    pub(crate) fn get(&self, sq: SquareIndex) -> Option<(PlayerSide, PieceKind)> {
        // Get the 4-bit integer representing the content of the square
        // b3 represents the side of the piece
        // b2-b0 represent the kind of piece, or the absence of piece
        let i = sq as usize;
        let nibble = self.0[i / 16] >> (i % 16 * 4);
        let kind = PieceKind::from_index(nibble as u8 & 0b0111)?;
        let side = if nibble & 0b1000 == 0 {
            PlayerSide::White
        } else {
            PlayerSide::Black
        };
        Some((side, kind))
    }

    pub(crate) fn set(&mut self, sq: SquareIndex, value: Option<(PlayerSide, PieceKind)>) {
        let i = sq as usize;
        let mask = 0b1111;
        self.0[i / 16] |= mask << (i % 16 * 4);
        if let Some((side, kind)) = value {
            let nibble = kind as u64 | (side as u64) << 3;
            self.0[i / 16] ^= (mask ^ nibble) << (i % 16 * 4);
        }
    }

    pub(crate) fn mirror(&self) -> CompactPieceArray {
        CompactPieceArray([
            self.0[3].rotate_left(32),
            self.0[2].rotate_left(32),
            self.0[1].rotate_left(32),
            self.0[0].rotate_left(32),
        ])
    }
}

impl Default for CastleRights {
    fn default() -> Self {
        CastleRights(0b11111111)
    }
}

impl CastleRights {
    pub fn get(&self, castle_side: CastleSide) -> Option<FileIndex> {
        match castle_side {
            CastleSide::East => FileIndex::from_index(self.0 & 0b00001111),
            CastleSide::West => FileIndex::from_index(self.0 >> 4),
        }
    }

    pub fn set(&mut self, castle_side: CastleSide, value: Option<FileIndex>) {
        match castle_side {
            CastleSide::East => match value {
                Some(file) => self.0 = (self.0 & 0b11110000) | file as u8,
                None => self.0 = (self.0 & 0b11110000) | 0b00001111,
            },
            CastleSide::West => match value {
                Some(file) => self.0 = (self.0 & 0b00001111) | (file as u8) << 4,
                None => self.0 = (self.0 & 0b00001111) | 0b11110000,
            },
        }
    }
}
