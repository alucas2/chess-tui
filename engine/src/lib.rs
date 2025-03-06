mod atomic_cell;
mod fen;
mod game_state;
mod game_state_key;
mod lookup_tables;
mod moves;
mod search;
mod transposition_table;

use std::num::NonZeroU64;

pub use game_state::GameState;
pub use game_state_key::{GameStateKey, GameStateKeyExtra, GameStateKeyWithHash};
pub use moves::{IllegalMoveError, Move, MoveFlag, MoveInfo};
pub use search::{settings, ScoreInfo, Search, SearchStatus};
pub use transposition_table::Table;

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
pub enum PlayerSide {
    #[default]
    White,
    Black,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CastleSide {
    East,
    West,
}

/// An iterator over the set squares if a bitboard.
pub struct SquareIter(pub u64);

impl Iterator for SquareIter {
    type Item = SquareIndex;

    #[inline]
    fn next(&mut self) -> Option<SquareIndex> {
        let sq = NonZeroU64::new(self.0).map(SquareIndex::from_bb)?;
        self.0 &= self.0 - 1;
        Some(sq)
    }
}

/// Array of 64 squares containing a piece kind and a player side.
/// Uses a compact representation of 4 bits per square
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompactPieceArray([u64; 4]);

/// Compact representation of various castle rights
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CastleRights(u8);

impl PieceKind {
    #[inline]
    pub const fn from_index(index: u8) -> Option<PieceKind> {
        match index {
            0..=5 => Some(unsafe { std::mem::transmute(index) }),
            _ => None,
        }
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = PieceKind> {
        (0..=5).map(|index| PieceKind::from_index(index).unwrap())
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

impl PlayerSide {
    pub fn opposite(self) -> PlayerSide {
        match self {
            PlayerSide::White => PlayerSide::Black,
            PlayerSide::Black => PlayerSide::White,
        }
    }

    pub fn label(&self) -> char {
        match self {
            PlayerSide::White => 'w',
            PlayerSide::Black => 'b',
        }
    }

    pub fn parse(c: char) -> Option<PlayerSide> {
        Some(match c {
            'w' => PlayerSide::White,
            'b' => PlayerSide::Black,
            _ => return None,
        })
    }
}

impl FileIndex {
    #[inline]
    pub const fn from_index(index: u8) -> Option<FileIndex> {
        match index {
            0..=7 => Some(unsafe { std::mem::transmute(index) }),
            _ => None,
        }
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = FileIndex> {
        (0..=7).map(|index| FileIndex::from_index(index).unwrap())
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

    #[inline]
    pub const fn bb(self) -> NonZeroU64 {
        NonZeroU64::new(0x0101010101010101 << (self as u8)).unwrap()
    }
}

impl RankIndex {
    #[inline]
    pub const fn from_index(index: u8) -> Option<RankIndex> {
        match index {
            0..=7 => Some(unsafe { std::mem::transmute(index) }),
            _ => None,
        }
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = RankIndex> {
        (0..=7).map(|index| RankIndex::from_index(index).unwrap())
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

    #[inline]
    pub const fn bb(self) -> NonZeroU64 {
        NonZeroU64::new(0xff << (8 * self as u8)).unwrap()
    }

    #[inline]
    pub const fn mirror(self) -> RankIndex {
        RankIndex::from_index(7 - self as u8).unwrap()
    }
}

impl SquareIndex {
    #[inline]
    pub const fn from_index(index: u8) -> Option<SquareIndex> {
        match index {
            0..=63 => Some(unsafe { std::mem::transmute(index) }),
            _ => None,
        }
    }

    #[inline]
    pub const fn from_coords(file: FileIndex, rank: RankIndex) -> SquareIndex {
        SquareIndex::from_index(8 * rank as u8 + file as u8).unwrap()
    }

    #[inline]
    pub const fn coords(self) -> (FileIndex, RankIndex) {
        let file = FileIndex::from_index(self as u8 % 8).unwrap();
        let rank = RankIndex::from_index(self as u8 / 8).unwrap();
        (file, rank)
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = SquareIndex> {
        (0..=63).map(|index| SquareIndex::from_index(index).unwrap())
    }

    #[inline]
    pub const fn from_bb(bb: NonZeroU64) -> SquareIndex {
        SquareIndex::from_index(bb.trailing_zeros() as u8).unwrap()
    }

    #[inline]
    pub const fn bb(self) -> NonZeroU64 {
        NonZeroU64::new(1 << (self as i32)).unwrap()
    }

    #[inline]
    pub const fn mirror(self) -> SquareIndex {
        let (file, rank) = self.coords();
        SquareIndex::from_coords(file, rank.mirror())
    }
}

impl Default for CompactPieceArray {
    fn default() -> Self {
        CompactPieceArray([0xffffffffffffffff; 4])
    }
}

impl CompactPieceArray {
    #[inline]
    pub fn get(&self, sq: SquareIndex) -> Option<(PlayerSide, PieceKind)> {
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

    #[inline]
    pub fn set(&mut self, sq: SquareIndex, value: Option<(PlayerSide, PieceKind)>) {
        let i = sq as usize;
        let mask = 0b1111;
        self.0[i / 16] |= mask << (i % 16 * 4);
        if let Some((side, kind)) = value {
            let nibble = kind as u64 | (side as u64) << 3;
            self.0[i / 16] ^= (mask ^ nibble) << (i % 16 * 4);
        }
    }

    #[inline]
    pub fn mirror(&self) -> CompactPieceArray {
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
