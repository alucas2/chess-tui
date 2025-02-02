use std::num::NonZeroU64;

/// State of the game.
///
/// The internal representation uses the terms "friend" and "enemy".
/// It is laid out in memory as if "friend" was the white player, but if "friend" is actually the
/// black player, then the internal representation is the mirror of the reality.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct GameState {
    pub(crate) friends: HalfPiecesPositions,
    pub(crate) enemies: HalfPiecesPositions,
    pub(crate) friends_color: PlayerColor,
    pub(crate) friends_castle: CastleAvailability,
    pub(crate) enemies_castle: CastleAvailability,
    pub(crate) en_passant_target: Option<FileIndex>,
    pub(crate) fullmoves: u16,
}

/// Internal structure that contains the bitboards for each piece type of the same color,
/// which is half of the information to get the positions of all pieces.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct HalfPiecesPositions {
    /// Array of bitboards indexed by `Piece`
    pub(crate) piece_bb: [u64; 6],
}

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

impl GameState {
    pub fn piece(&self, square: SquareIndex) -> Option<(PlayerColor, PieceKind)> {
        let square = match self.friends_color {
            PlayerColor::White => square,
            PlayerColor::Black => square.mirror(),
        };
        let sq_bb = square.bb().get();
        let found_friend_piece = {
            let mut iter =
                PieceKind::iter().filter(|&p| self.friends.piece_bb[p as usize] & sq_bb != 0);
            let found = iter.next();
            assert_eq!(iter.next(), None, "Multiple pieces on the same square");
            found
        };
        let found_enemy_piece = {
            let mut iter =
                PieceKind::iter().filter(|&p| self.enemies.piece_bb[p as usize] & sq_bb != 0);
            let found = iter.next();
            assert_eq!(iter.next(), None, "Multiple pieces on the same square");
            found
        };
        match (found_friend_piece, found_enemy_piece) {
            (None, None) => None,
            (Some(p), None) => Some((self.friends_color, p)),
            (None, Some(p)) => Some((self.friends_color.opposite(), p)),
            (Some(_), Some(_)) => panic!("Multiple pieces on the same square"),
        }
    }

    pub fn set_piece(&mut self, square: SquareIndex, piece: Option<(PlayerColor, PieceKind)>) {
        let square = match self.friends_color {
            PlayerColor::White => square,
            PlayerColor::Black => square.mirror(),
        };
        let sq_bb = square.bb().get();
        for kind in PieceKind::iter() {
            self.friends.piece_bb[kind as usize] &= !sq_bb;
            self.enemies.piece_bb[kind as usize] &= !sq_bb;
        }
        if let Some((color, kind)) = piece {
            if color == self.friends_color {
                self.friends.piece_bb[kind as usize] |= sq_bb
            } else {
                self.enemies.piece_bb[kind as usize] |= sq_bb
            };
        }
    }

    pub fn active_color(&self) -> PlayerColor {
        self.friends_color
    }

    pub fn set_active_color(&mut self, color: PlayerColor) {
        if self.friends_color != color {
            *self = GameState {
                friends: self.enemies.mirror(),
                enemies: self.friends.mirror(),
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

impl HalfPiecesPositions {
    pub(crate) fn bb(&self, piece: PieceKind) -> u64 {
        self.piece_bb[piece as usize]
    }

    pub(crate) fn bb_mut(&mut self, piece: PieceKind) -> &mut u64 {
        &mut self.piece_bb[piece as usize]
    }

    pub(crate) fn any(&self) -> u64 {
        self.piece_bb.iter().fold(0, std::ops::BitOr::bitor)
    }

    pub(crate) fn mirror(&self) -> HalfPiecesPositions {
        HalfPiecesPositions {
            piece_bb: self.piece_bb.map(u64::swap_bytes),
        }
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
