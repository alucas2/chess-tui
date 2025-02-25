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
pub(crate) struct SquareIter(pub u64);

impl Iterator for SquareIter {
    type Item = SquareIndex;

    fn next(&mut self) -> Option<SquareIndex> {
        let sq = NonZeroU64::new(self.0).map(SquareIndex::from_bb)?;
        self.0 &= self.0 - 1;
        Some(sq)
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

    pub fn from_u8(x: u8) -> Option<FileIndex> {
        Some(match x {
            0 => FileIndex::A,
            1 => FileIndex::B,
            2 => FileIndex::C,
            3 => FileIndex::D,
            4 => FileIndex::E,
            5 => FileIndex::F,
            6 => FileIndex::G,
            7 => FileIndex::H,
            _ => return None,
        })
    }

    pub(crate) const fn bb(self) -> NonZeroU64 {
        // Safety: self is in 0..=7 so 'bb' is non zero
        let bb = 0x0101010101010101 << (self as u8);
        unsafe { NonZeroU64::new_unchecked(bb) }
    }
}

impl RankIndex {
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
