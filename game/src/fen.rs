use engine::{
    CastleAvailability, FileIndex, GameState, PieceKind, PlayerColor, RankIndex, SquareIndex,
};

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
        unparse::active_color(gs, &mut result);
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
                    Some((color, kind)) => {
                        if skip != 0 {
                            result.push(char::from_digit(skip, 10).unwrap());
                        }
                        skip = 0;
                        match color {
                            PlayerColor::White => result.push(kind.label().to_ascii_uppercase()),
                            PlayerColor::Black => result.push(kind.label().to_ascii_lowercase()),
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

    fn active_color(gs: &GameState, result: &mut String) {
        result.push(gs.active_color().label());
    }

    fn castle_availability(gs: &GameState, result: &mut String) {
        let white = gs.castle_availability(PlayerColor::White);
        let black = gs.castle_availability(PlayerColor::Black);
        if !(white.west || white.east || black.west || black.east) {
            result.push('-');
        } else {
            if white.east {
                result.push('K');
            }
            if white.west {
                result.push('Q');
            }
            if black.east {
                result.push('k');
            }
            if black.west {
                result.push('q');
            }
        }
    }

    fn en_passant_target(gs: &GameState, result: &mut String) {
        match gs.en_passant_target() {
            Some(file) => {
                result.push(file.label());
                match gs.active_color() {
                    PlayerColor::White => result.push(RankIndex::_6.label()),
                    PlayerColor::Black => result.push(RankIndex::_3.label()),
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
            active_color(s, &mut gs)?;
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
                let color = if c.is_uppercase() {
                    PlayerColor::White
                } else {
                    PlayerColor::Black
                };
                gs.set_piece(SquareIndex::from_coords(file, rank), Some((color, kind)));
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

    fn active_color(s: &str, gs: &mut GameState) -> Result<(), ParseError> {
        let mut s = s.chars();
        gs.set_active_color(s.next().and_then(PlayerColor::parse).ok_or(ParseError)?);
        if s.next().is_some() {
            return Err(ParseError);
        }
        Ok(())
    }

    fn castle_availability(s: &str, gs: &mut GameState) -> Result<(), ParseError> {
        let mut white = CastleAvailability::default();
        let mut black = CastleAvailability::default();
        if s != "-" {
            for char in s.chars() {
                match char {
                    'K' => white.east = true,
                    'Q' => white.west = true,
                    'k' => black.east = true,
                    'q' => black.west = true,
                    _ => return Err(ParseError),
                }
            }
        }
        gs.set_castle_availability(PlayerColor::White, white);
        gs.set_castle_availability(PlayerColor::Black, black);
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
