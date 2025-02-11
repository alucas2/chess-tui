use engine::{FileIndex, GameState, PieceKind, PlayerSide, RankIndex, SquareIndex};

/// An error raised when a FEN string could not be parsed.
#[derive(Debug, Clone, Copy)]
pub struct ParseError;

pub use parse::gamestate as parse;
pub use unparse::gamestate as unparse;

pub fn initial_position() -> GameState {
    parse("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
}

mod unparse {
    use super::*;

    pub fn gamestate(gs: &GameState) -> String {
        let mut result = String::new();
        unparse::pieces(gs, &mut result);
        result.push(' ');
        unparse::side_to_move(gs, &mut result);
        result.push(' ');
        unparse::castle_availability(gs, &mut result);
        result.push(' ');
        unparse::en_passant_target(gs, &mut result);
        result.push(' ');
        unparse::move_counters(gs, &mut result);
        result
    }

    fn pieces(gs: &GameState, result: &mut String) {
        for rank in RankIndex::iter().rev() {
            let mut skip = 0;
            for file in FileIndex::iter() {
                match gs.piece(SquareIndex::from_coords(file, rank)) {
                    None => skip += 1,
                    Some((side, kind)) => {
                        if skip != 0 {
                            result.push(char::from_digit(skip, 10).unwrap());
                        }
                        skip = 0;
                        match side {
                            PlayerSide::White => result.push(kind.label().to_ascii_uppercase()),
                            PlayerSide::Black => result.push(kind.label().to_ascii_lowercase()),
                        }
                    }
                }
            }
            if skip != 0 {
                result.push(char::from_digit(skip, 10).unwrap());
            }
            if rank != RankIndex::_1 {
                result.push('/');
            }
        }
    }

    fn side_to_move(gs: &GameState, result: &mut String) {
        result.push(gs.side_to_move().label());
    }

    fn castle_availability(gs: &GameState, result: &mut String) {
        if gs.castle(PlayerSide::White).east().is_none()
            && gs.castle(PlayerSide::White).west().is_none()
            && gs.castle(PlayerSide::Black).east().is_none()
            && gs.castle(PlayerSide::Black).west().is_none()
        {
            result.push('-');
        } else {
            if let Some(x) = gs.castle(PlayerSide::White).east() {
                result.push(x.label().to_ascii_uppercase());
            }
            if let Some(x) = gs.castle(PlayerSide::White).west() {
                result.push(x.label().to_ascii_uppercase());
            }
            if let Some(x) = gs.castle(PlayerSide::Black).east() {
                result.push(x.label().to_ascii_lowercase());
            }
            if let Some(x) = gs.castle(PlayerSide::Black).west() {
                result.push(x.label().to_ascii_lowercase());
            }
        }
    }

    fn en_passant_target(gs: &GameState, result: &mut String) {
        match gs.en_passant_target() {
            Some(file) => {
                result.push(file.label());
                match gs.side_to_move() {
                    PlayerSide::White => result.push(RankIndex::_6.label()),
                    PlayerSide::Black => result.push(RankIndex::_3.label()),
                }
            }
            None => result.push('-'),
        }
    }

    fn move_counters(gs: &GameState, result: &mut String) {
        result.push('0');
        result.push(' ');
        result.push_str(&gs.fullmoves().to_string());
    }
}

mod parse {
    use super::*;

    pub fn gamestate(fen: &str) -> Result<GameState, ParseError> {
        let mut split = fen.split_whitespace();
        let mut gs = GameState::default();

        if let Some(s) = split.next() {
            pieces(s, &mut gs)?;
        }
        if let Some(s) = split.next() {
            side_to_move(s, &mut gs)?;
        }
        if let Some(s) = split.next() {
            castle_availability(s, &mut gs)?;
        }
        if let Some(s) = split.next() {
            en_passant_target(s, &mut gs)?;
        }
        if let Some(s) = split.next() {
            let _: u16 = s.parse().map_err(|_| ParseError)?;
        }
        if let Some(s) = split.next() {
            gs.set_fullmoves(s.parse().map_err(|_| ParseError)?);
        }
        Ok(gs)
    }

    fn pieces(s: &str, gs: &mut GameState) -> Result<(), ParseError> {
        let mut s = s.chars();
        for rank in RankIndex::iter().rev() {
            let mut skip = 0;
            for file in FileIndex::iter() {
                if skip > 0 {
                    skip -= 1;
                    continue;
                }
                let Some(c) = s.next() else {
                    return Ok(()); // Allow for incomplete position
                };
                if let Some(n @ 1..=8) = c.to_digit(10) {
                    skip = n - 1;
                    continue;
                }
                let kind = PieceKind::parse(c).ok_or(ParseError)?;
                let side = if c.is_uppercase() {
                    PlayerSide::White
                } else {
                    PlayerSide::Black
                };
                gs.set_piece(SquareIndex::from_coords(file, rank), Some((side, kind)));
            }
            if skip != 0 {
                return Err(ParseError);
            }
            if rank != RankIndex::_1 {
                match s.next() {
                    None => return Ok(()), // Allow for incomplete position,
                    Some('/') => {}
                    _ => return Err(ParseError),
                }
            }
        }
        if s.next().is_some() {
            return Err(ParseError);
        }
        Ok(())
    }

    fn side_to_move(s: &str, gs: &mut GameState) -> Result<(), ParseError> {
        let mut s = s.chars();
        gs.set_side_to_move(s.next().and_then(PlayerSide::parse).ok_or(ParseError)?);
        if s.next().is_some() {
            return Err(ParseError);
        }
        Ok(())
    }

    fn castle_availability(s: &str, gs: &mut GameState) -> Result<(), ParseError> {
        if s != "-" {
            for char in s.chars() {
                match char {
                    'K' => gs
                        .castle_mut(PlayerSide::White)
                        .set_east(Some(FileIndex::H)),
                    'Q' => gs
                        .castle_mut(PlayerSide::White)
                        .set_west(Some(FileIndex::A)),
                    'k' => gs
                        .castle_mut(PlayerSide::Black)
                        .set_east(Some(FileIndex::H)),
                    'q' => gs
                        .castle_mut(PlayerSide::Black)
                        .set_west(Some(FileIndex::A)),
                    c => match FileIndex::parse(c.to_ascii_lowercase()) {
                        Some(file) => {
                            let side = if c.is_ascii_uppercase() {
                                PlayerSide::White
                            } else {
                                PlayerSide::Black
                            };
                            if let Some((king_file, _)) = SquareIndex::iter()
                                .find(|sq| gs.piece(*sq) == Some((side, PieceKind::King)))
                                .map(|sq| sq.coords())
                            {
                                if (file as u8) < (king_file as u8) {
                                    gs.castle_mut(side).set_west(Some(file));
                                } else if (file as u8) > (king_file as u8) {
                                    gs.castle_mut(side).set_east(Some(file));
                                }
                            } else {
                                return Err(ParseError);
                            }
                        }
                        None => return Err(ParseError),
                    },
                }
            }
        }
        Ok(())
    }

    fn en_passant_target(s: &str, gs: &mut GameState) -> Result<(), ParseError> {
        if s != "-" {
            let mut s = s.chars();
            let file = s.next().and_then(FileIndex::parse).ok_or(ParseError)?;
            let _rank = s.next().and_then(RankIndex::parse).ok_or(ParseError)?;
            if s.next().is_some() {
                return Err(ParseError);
            };
            gs.set_en_passant_target(Some(file));
        }
        Ok(())
    }
}
