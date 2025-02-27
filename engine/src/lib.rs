mod atomic_cell;
mod evaluate;
mod fen;
mod game_state;
mod lookup_tables;
mod move_predictor;
mod moves;
mod search;
mod transposition_table;

use std::num::NonZeroU64;

pub use evaluate::ScoreInfo;
pub use game_state::GameState;
pub use moves::{IllegalMoveError, Move, MoveFlag, MoveInfo};
pub use search::{Search, SearchStatus};
pub use transposition_table::{Table, TableKey};

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
