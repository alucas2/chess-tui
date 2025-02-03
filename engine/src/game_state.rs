use std::num::NonZeroU64;

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
    pub(crate) friends_color: PlayerColor,
    /// Castling rights of the player to move
    pub(crate) friends_castle: CastleAvailability,
    /// Castling rights of the opponent
    pub(crate) enemies_castle: CastleAvailability,
    /// File of the last pawn double push for en passant captures
    pub(crate) en_passant_target: Option<FileIndex>,
    /// Number of full moves, incremented after black has played
    pub(crate) fullmoves: u16,
}

/// Array of 6 bitboards, one for each piece kind
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct PieceBitboards([u64; 6]);

/// Array of 64 squares containing a piece kind and a color.
/// Uses a compact representation of 4 bits per square
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct CompactPieceArray([u64; 4]);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PieceKind {
    Pawn = 0,
    Knight,
    Bishop,
    Rook,
    Queen,
    King = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum PlayerColor {
    #[default]
    White,
    Black,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CastleAvailability {
    /// Castle east, a.k.a. on the king's side
    pub east: bool,
    /// Castle west, a.k.a. on the queen's side
    pub west: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum FileIndex {
    A = 0, B, C, D, E, F, G, H = 7,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum RankIndex {
    _1 = 0, _2, _3, _4, _5, _6, _7, _8 = 7,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum SquareIndex {
    A1 = 0, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8 = 63,
}

/// An iterator over the set squares if a bitboard.
pub(crate) struct SquareIter(pub u64);

impl Iterator for SquareIter {
    type Item = SquareIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let sq = NonZeroU64::new(self.0).map(SquareIndex::from_bb)?;
        self.0 &= self.0 - 1;
        Some(sq)
    }
}

impl GameState {
    pub fn piece(&self, square: SquareIndex) -> Option<(PlayerColor, PieceKind)> {
        let square = match self.friends_color {
            PlayerColor::White => square,
            PlayerColor::Black => square.mirror(),
        };
        self.pieces.get(square)
    }

    pub fn set_piece(&mut self, square: SquareIndex, piece: Option<(PlayerColor, PieceKind)>) {
        let square = match self.friends_color {
            PlayerColor::White => square,
            PlayerColor::Black => square.mirror(),
        };
        // Update bitboards
        let sq_bb = square.bb().get();
        if let Some((color, kind)) = self.pieces.get(square) {
            if color == self.friends_color {
                self.friends_bb[kind] &= !sq_bb;
            } else {
                self.enemies_bb[kind] &= !sq_bb;
            }
        }
        if let Some((color, kind)) = piece {
            if color == self.friends_color {
                self.friends_bb[kind] |= sq_bb;
            } else {
                self.enemies_bb[kind] |= sq_bb;
            };
        }
        // Place piece
        self.pieces.set(square, piece);
    }

    pub fn active_color(&self) -> PlayerColor {
        self.friends_color
    }

    pub fn set_active_color(&mut self, color: PlayerColor) {
        if self.friends_color != color {
            *self = GameState {
                pieces: self.pieces.mirror(),
                friends_bb: self.enemies_bb.mirror(),
                enemies_bb: self.friends_bb.mirror(),
                friends_color: self.friends_color.opposite(),
                friends_castle: self.enemies_castle,
                enemies_castle: self.friends_castle,
                ..*self
            }
        }
    }

    pub fn castle_availability(&self, color: PlayerColor) -> CastleAvailability {
        if self.friends_color == color {
            self.friends_castle
        } else {
            self.enemies_castle
        }
    }

    pub fn set_castle_availability(&mut self, color: PlayerColor, value: CastleAvailability) {
        if self.friends_color == color {
            self.friends_castle = value;
        } else {
            self.enemies_castle = value;
        }
    }

    pub fn en_passant_target(&self) -> Option<FileIndex> {
        self.en_passant_target
    }

    pub fn set_en_passant_target(&mut self, value: Option<FileIndex>) {
        self.en_passant_target = value;
    }

    pub fn fullmoves(&self) -> u16 {
        self.fullmoves
    }

    pub fn set_fullmoves(&mut self, value: u16) {
        self.fullmoves = value;
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
    pub(crate) fn get(&self, sq: SquareIndex) -> Option<(PlayerColor, PieceKind)> {
        // Get the 4-bit integer representing the content of the square
        // b3 represents the color of the piece
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
        let color = if nibble & 0b1000 == 0 {
            PlayerColor::White
        } else {
            PlayerColor::Black
        };
        Some((color, kind))
    }

    #[inline]
    pub(crate) fn set(&mut self, sq: SquareIndex, value: Option<(PlayerColor, PieceKind)>) {
        let i = sq as usize;
        let mask = 0b1111;
        self.0[i / 16] |= mask << (i % 16 * 4);
        if let Some((color, kind)) = value {
            let nibble = kind as u64 | (color as u64) << 3;
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

impl PieceKind {
    pub fn iter() -> impl DoubleEndedIterator<Item = PieceKind> {
        // Safety: index is in 0..=5
        (0..6_u8).map(|index| unsafe { std::mem::transmute(index) })
    }

    pub fn label(&self) -> char {
        match self {
            PieceKind::Pawn => 'p',
            PieceKind::Knight => 'n',
            PieceKind::Bishop => 'b',
            PieceKind::Rook => 'r',
            PieceKind::Queen => 'q',
            PieceKind::King => 'k',
        }
    }

    pub fn parse(c: char) -> Option<PieceKind> {
        Some(match c.to_ascii_lowercase() {
            'p' => PieceKind::Pawn,
            'n' => PieceKind::Knight,
            'b' => PieceKind::Bishop,
            'r' => PieceKind::Rook,
            'q' => PieceKind::Queen,
            'k' => PieceKind::King,
            _ => return None,
        })
    }
}

impl PlayerColor {
    pub fn opposite(self) -> PlayerColor {
        match self {
            PlayerColor::White => PlayerColor::Black,
            PlayerColor::Black => PlayerColor::White,
        }
    }

    pub fn label(&self) -> char {
        match self {
            PlayerColor::White => 'w',
            PlayerColor::Black => 'b',
        }
    }

    pub fn parse(c: char) -> Option<PlayerColor> {
        Some(match c {
            'w' => PlayerColor::White,
            'b' => PlayerColor::Black,
            _ => return None,
        })
    }
}

impl FileIndex {
    #[allow(unused)] // Apparently this function is not used? Might be useful one day...
    pub(crate) const fn bb(self) -> NonZeroU64 {
        // Safety: self is in 0..=7 so 'bb' is non zero
        let bb = 0x0101010101010101 << (self as u8);
        unsafe { NonZeroU64::new_unchecked(bb) }
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = FileIndex> {
        // Safety: index is in 0..=7
        (0..8_u8).map(|index| unsafe { std::mem::transmute(index) })
    }

    pub fn label(&self) -> char {
        match self {
            FileIndex::A => 'a',
            FileIndex::B => 'b',
            FileIndex::C => 'c',
            FileIndex::D => 'd',
            FileIndex::E => 'e',
            FileIndex::F => 'f',
            FileIndex::G => 'g',
            FileIndex::H => 'h',
        }
    }

    pub fn parse(c: char) -> Option<FileIndex> {
        Some(match c.to_ascii_lowercase() {
            'a' => FileIndex::A,
            'b' => FileIndex::B,
            'c' => FileIndex::C,
            'd' => FileIndex::D,
            'e' => FileIndex::E,
            'f' => FileIndex::F,
            'g' => FileIndex::G,
            'h' => FileIndex::H,
            _ => return None,
        })
    }
}

impl RankIndex {
    pub(crate) const fn bb(self) -> NonZeroU64 {
        // Safety: self is in 0..=7 so bb is non zero
        let bb = 0xff << (8 * self as u8);
        unsafe { NonZeroU64::new_unchecked(bb) }
    }

    pub(crate) const fn mirror(self) -> RankIndex {
        // Safety: self is in 0..=7 so index is too
        let index = 7 - self as u8;
        unsafe { std::mem::transmute(index) }
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = RankIndex> {
        // Safety: index is in 0..=7
        (0..8_u8).map(|index| unsafe { std::mem::transmute(index) })
    }

    pub fn label(&self) -> char {
        match self {
            RankIndex::_1 => '1',
            RankIndex::_2 => '2',
            RankIndex::_3 => '3',
            RankIndex::_4 => '4',
            RankIndex::_5 => '5',
            RankIndex::_6 => '6',
            RankIndex::_7 => '7',
            RankIndex::_8 => '8',
        }
    }

    pub fn parse(c: char) -> Option<RankIndex> {
        Some(match c {
            '1' => RankIndex::_1,
            '2' => RankIndex::_2,
            '3' => RankIndex::_3,
            '4' => RankIndex::_4,
            '5' => RankIndex::_5,
            '6' => RankIndex::_6,
            '7' => RankIndex::_7,
            '8' => RankIndex::_8,
            _ => return None,
        })
    }
}

impl SquareIndex {
    pub const fn from_coords(file: FileIndex, rank: RankIndex) -> SquareIndex {
        // Safety: rank is in 0..=7 and file is in 0..=7, thus index is in 0..=63
        let index = 8 * rank as u8 + file as u8;
        unsafe { std::mem::transmute(index) }
    }

    pub const fn coords(self) -> (FileIndex, RankIndex) {
        // Safety: division and modulo results are in 0..=7
        let file = unsafe { std::mem::transmute(self as u8 % 8) };
        let rank = unsafe { std::mem::transmute(self as u8 / 8) };
        (file, rank)
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = SquareIndex> {
        // Safety: index is in 0..=64
        (0..64_u8).map(|index| unsafe { std::mem::transmute(index) })
    }

    pub(crate) const fn from_bb(bb: NonZeroU64) -> SquareIndex {
        // Safety: bb in nonzero so n is in range 0..=63
        let index = bb.trailing_zeros() as u8;
        unsafe { std::mem::transmute(index) }
    }

    pub(crate) const fn bb(self) -> NonZeroU64 {
        // Safety: self is in 0..=63 so bb is non zero
        let bb = 1 << (self as i32);
        unsafe { NonZeroU64::new_unchecked(bb) }
    }

    pub(crate) const fn mirror(self) -> SquareIndex {
        let (file, rank) = self.coords();
        SquareIndex::from_coords(file, rank.mirror())
    }
}
