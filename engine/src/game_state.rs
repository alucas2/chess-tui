use crate::{FileIndex, PieceKind, PlayerSide, SquareIndex};

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
    /// Castle rights and en passant
    pub(crate) flags: CompactGameStateFlags,
    /// Number of full moves, incremented after black has played
    pub(crate) fullmoves: u16,
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
pub(crate) struct CompactGameStateFlags(u8);

impl GameState {
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
                flags: self.flags,
                fullmoves: self.fullmoves,
            }
        }
    }

    pub fn castle_east(&self, side: PlayerSide) -> bool {
        self.flags.castle_east(side)
    }

    pub fn castle_west(&self, side: PlayerSide) -> bool {
        self.flags.castle_west(side)
    }

    pub fn set_castle_east(&mut self, side: PlayerSide, value: bool) {
        self.flags.set_castle_east(side, value)
    }

    pub fn set_castle_west(&mut self, side: PlayerSide, value: bool) {
        self.flags.set_castle_west(side, value)
    }

    pub fn en_passant_target(&self) -> Option<FileIndex> {
        self.flags.en_passant()
    }

    pub fn set_en_passant_target(&mut self, value: Option<FileIndex>) {
        self.flags.set_en_passant(value);
    }

    pub fn fullmoves(&self) -> u16 {
        self.fullmoves
    }

    pub fn set_fullmoves(&mut self, value: u16) {
        self.fullmoves = value;
    }

    /// Remove a piece from the board while keeping the internal data consistent.
    /// Ensure that the expected piece is on specified square before calling this function,
    /// or else corruption will occur.
    #[inline]
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
    #[inline]
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
    #[inline]
    pub(crate) fn mirror(self) -> PieceBitboards {
        PieceBitboards(self.0.map(u64::swap_bytes))
    }

    #[inline]
    pub(crate) fn union(self) -> u64 {
        self.0.iter().fold(0, |a, b| a | b)
    }
}

impl std::ops::Index<PieceKind> for PieceBitboards {
    type Output = u64;

    #[inline]
    fn index(&self, kind: PieceKind) -> &u64 {
        &self.0[kind as usize]
    }
}

impl std::ops::IndexMut<PieceKind> for PieceBitboards {
    #[inline]
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
    #[inline]
    pub(crate) fn get(&self, sq: SquareIndex) -> Option<(PlayerSide, PieceKind)> {
        // Get the 4-bit integer representing the content of the square
        // b3 represents the side of the piece
        // b2-b0 represent the kind of piece, or the absence of piece
        let i = sq as usize;
        let nibble = self.0[i / 16] >> (i % 16 * 4);
        let kind = match nibble & 0b0111 {
            0 => PieceKind::Pawn,
            1 => PieceKind::Knight,
            2 => PieceKind::Bishop,
            3 => PieceKind::Rook,
            4 => PieceKind::Queen,
            5 => PieceKind::King,
            _ => return None,
        };
        let side = if nibble & 0b1000 == 0 {
            PlayerSide::White
        } else {
            PlayerSide::Black
        };
        Some((side, kind))
    }

    #[inline]
    pub(crate) fn set(&mut self, sq: SquareIndex, value: Option<(PlayerSide, PieceKind)>) {
        let i = sq as usize;
        let mask = 0b1111;
        self.0[i / 16] |= mask << (i % 16 * 4);
        if let Some((side, kind)) = value {
            let nibble = kind as u64 | (side as u64) << 3;
            self.0[i / 16] ^= (mask ^ nibble) << (i % 16 * 4);
        }
    }

    #[inline]
    pub(crate) fn mirror(&self) -> CompactPieceArray {
        CompactPieceArray([
            self.0[3].rotate_left(32),
            self.0[2].rotate_left(32),
            self.0[1].rotate_left(32),
            self.0[0].rotate_left(32),
        ])
    }
}

impl Default for CompactGameStateFlags {
    fn default() -> Self {
        // En passant is all 1 to represent the absence of it
        CompactGameStateFlags(0b11110000)
    }
}

impl CompactGameStateFlags {
    const WHITE_CASTLE_WEST: u8 = 1 << 0;
    const WHITE_CASTLE_EAST: u8 = 1 << 1;
    const BLACK_CASTLE_WEST: u8 = 1 << 2;
    const BLACK_CASTLE_EAST: u8 = 1 << 3;
    const EN_PASSANT_SHIFT: u32 = 4;

    pub(crate) fn castle_east(&self, side: PlayerSide) -> bool {
        match side {
            PlayerSide::White => self.0 & Self::WHITE_CASTLE_EAST != 0,
            PlayerSide::Black => self.0 & Self::BLACK_CASTLE_EAST != 0,
        }
    }

    pub(crate) fn castle_west(&self, side: PlayerSide) -> bool {
        match side {
            PlayerSide::White => self.0 & Self::WHITE_CASTLE_WEST != 0,
            PlayerSide::Black => self.0 & Self::BLACK_CASTLE_WEST != 0,
        }
    }

    pub(crate) fn set_castle_east(&mut self, side: PlayerSide, value: bool) {
        match (side, value) {
            (PlayerSide::White, true) => self.0 |= Self::WHITE_CASTLE_EAST,
            (PlayerSide::Black, true) => self.0 |= Self::BLACK_CASTLE_EAST,
            (PlayerSide::White, false) => self.0 &= !Self::WHITE_CASTLE_EAST,
            (PlayerSide::Black, false) => self.0 &= !Self::BLACK_CASTLE_EAST,
        }
    }

    pub(crate) fn set_castle_west(&mut self, side: PlayerSide, value: bool) {
        match (side, value) {
            (PlayerSide::White, true) => self.0 |= Self::WHITE_CASTLE_WEST,
            (PlayerSide::Black, true) => self.0 |= Self::BLACK_CASTLE_WEST,
            (PlayerSide::White, false) => self.0 &= !Self::WHITE_CASTLE_WEST,
            (PlayerSide::Black, false) => self.0 &= !Self::BLACK_CASTLE_WEST,
        }
    }

    pub(crate) fn en_passant(&self) -> Option<FileIndex> {
        match self.0 >> Self::EN_PASSANT_SHIFT {
            0 => Some(FileIndex::A),
            1 => Some(FileIndex::B),
            2 => Some(FileIndex::C),
            3 => Some(FileIndex::D),
            4 => Some(FileIndex::E),
            5 => Some(FileIndex::F),
            6 => Some(FileIndex::G),
            7 => Some(FileIndex::H),
            _ => None,
        }
    }

    pub(crate) fn set_en_passant(&mut self, value: Option<FileIndex>) {
        let mask = 0b1111;
        self.0 |= mask << Self::EN_PASSANT_SHIFT;
        if let Some(file) = value {
            self.0 ^= (mask ^ file as u8) << Self::EN_PASSANT_SHIFT
        }
    }
}
