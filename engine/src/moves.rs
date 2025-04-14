use crate::{
    game_state::GameState,
    lookup_tables as lut, CastleSide,
    PieceKind::{self, *},
    PlayerSide, RankIndex, SquareIndex, SquareIter,
};

/// Opaque move token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move(MoveInfo);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveInfo {
    /// Kind of piece that is moving
    pub kind: PieceKind,
    /// Starting square of the move
    pub from: SquareIndex,
    /// Landing square of the move. For castling, it is the rook's starting square.
    pub to: SquareIndex,
    /// Flag for special moves
    pub flag: MoveFlag,
}

/// Flag for a special move.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveFlag {
    Normal,
    Promotion(PieceKind),
    EnPassant,
    DoublePush,
    // Note: we have two separate CastleEast and CastleWest variants instead of
    // a single Castle(CastleSide) variants or else MoveFlag would occupy one extra byte
    CastleEast,
    CastleWest,
}

impl Move {
    /// Unwraps the info about this move.
    /// **The square coordinates assume that the white side is moving**
    #[inline]
    pub(crate) fn unwrap(self) -> MoveInfo {
        self.0
    }

    /// Get info about a move.
    /// This function must be called with the state that this move originate from!
    pub fn info(&self, gs: &GameState) -> MoveInfo {
        let Move(mut info) = *self;
        if gs.side_to_move == PlayerSide::Black {
            info.from = info.from.mirror();
            info.to = info.to.mirror();
        }
        info
    }
}

impl std::fmt::Display for MoveInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (from_file, from_rank) = self.from.coords();
        let (to_file, to_rank) = self.to.coords();
        write!(
            f,
            "{}{}{}{}",
            from_file.label(),
            from_rank.label(),
            to_file.label(),
            to_rank.label(),
        )?;
        if let MoveFlag::Promotion(prom) = self.flag {
            write!(f, "{}", prom.label())?
        }
        Ok(())
    }
}

/// An error raised when a move puts the king in check.
#[derive(Debug, Clone, Copy)]
pub struct IllegalMoveError;

impl GameState {
    /// Collect all the pseudo-legal moves from the current
    pub fn pseudo_legal_moves<F: FnMut(Move)>(&self, f: F) {
        self.pseudo_legal_moves_inner::<false, F>(f)
    }

    /// Collect the pseudo-legal moves from the current, but only the non-quiet moves.
    pub(crate) fn pseudo_legal_non_quiet_moves<F: FnMut(Move)>(&self, f: F) {
        self.pseudo_legal_moves_inner::<true, F>(f)
    }

    /// Call the provided closure on all pseudo-legal moves from the current state
    fn pseudo_legal_moves_inner<const ONLY_NON_QUIET: bool, F: FnMut(Move)>(&self, mut f: F) {
        let friends_bb_union = self.friends_bb.union();
        let enemies_bb_union = self.enemies_bb.union();
        let blockers = friends_bb_union | enemies_bb_union;

        let dst_mask = if ONLY_NON_QUIET {
            enemies_bb_union
        } else {
            !friends_bb_union
        };

        // Pawn regular moves
        for from in SquareIter(self.friends_bb[Pawn] & !RankIndex::_7.bb().get()) {
            let from_bb = from.bb().get();
            let captures = (lut::shift_ne(from_bb) | lut::shift_nw(from_bb)) & enemies_bb_union;
            if ONLY_NON_QUIET {
                for to in SquareIter(captures) {
                    f(Move::normal(Pawn, from, to))
                }
            } else {
                let single_push = lut::shift_n(from_bb) & !blockers;
                for to in SquareIter(captures | single_push) {
                    f(Move::normal(Pawn, from, to))
                }
                for to in
                    SquareIter(lut::shift_n(single_push) & !blockers & RankIndex::_4.bb().get())
                {
                    f(Move::with_flag(Pawn, from, to, MoveFlag::DoublePush))
                }
            }
        }

        // Pawn promotions
        for from in SquareIter(self.friends_bb[Pawn] & RankIndex::_7.bb().get()) {
            const PROMOTIONS: [PieceKind; 4] = [Queen, Knight, Bishop, Rook];
            let from_bb = from.bb().get();
            let captures = (lut::shift_ne(from_bb) | lut::shift_nw(from_bb)) & enemies_bb_union;
            let single_push = lut::shift_n(from_bb) & !blockers;
            for to in SquareIter(captures | single_push) {
                for prom in PROMOTIONS {
                    f(Move::with_flag(Pawn, from, to, MoveFlag::Promotion(prom)))
                }
            }
        }

        // Knight, Bishop, Rook, Queen and King moves
        for from in SquareIter(self.friends_bb[Knight]) {
            for to in SquareIter(lut::knight_reachable(from) & dst_mask) {
                f(Move::normal(Knight, from, to))
            }
        }
        for from in SquareIter(self.friends_bb[Bishop]) {
            for to in SquareIter(lut::bishop_reachable(from, blockers) & dst_mask) {
                f(Move::normal(Bishop, from, to))
            }
        }
        for from in SquareIter(self.friends_bb[Rook]) {
            for to in SquareIter(lut::rook_reachable(from, blockers) & dst_mask) {
                f(Move::normal(Rook, from, to))
            }
        }
        for from in SquareIter(self.friends_bb[Queen]) {
            for to in SquareIter(lut::queen_reachable(from, blockers) & dst_mask) {
                f(Move::normal(Queen, from, to))
            }
        }
        for from in SquareIter(self.friends_bb[King]) {
            for to in SquareIter(lut::king_reachable(from) & dst_mask) {
                f(Move::normal(King, from, to))
            }
        }

        // En passant
        if let Some(file) = self.en_passant {
            let to = SquareIndex::from_coords(file, RankIndex::_6);
            let to_bb = to.bb().get();
            let capturers = (lut::shift_se(to_bb) | lut::shift_sw(to_bb)) & self.friends_bb[Pawn];
            for from in SquareIter(capturers) {
                f(Move::with_flag(Pawn, from, to, MoveFlag::EnPassant))
            }
        }

        // Castle
        if !ONLY_NON_QUIET {
            for castle_side in [CastleSide::East, CastleSide::West] {
                if let Some(file) = self.friends_castle.get(castle_side) {
                    if let Some(king_from) = SquareIter(self.friends_bb[King]).next() {
                        let rook_from = SquareIndex::from_coords(file, RankIndex::_1);
                        let (king_to, rook_to) = castle_destinations(castle_side);
                        let blockers = blockers & !king_from.bb().get() & !rook_from.bb().get();
                        if lut::castle_ray(king_from, king_to) & blockers == 0
                            && lut::castle_ray(rook_from, rook_to) & blockers == 0
                        {
                            f(Move::castle(castle_side, king_from, rook_from))
                        }
                    }
                }
            }
        }
    }

    /// Perform a move, then flip the position of enemies and friends.
    /// This function must be called with a move that has been generated from this state!
    /// If not, then probably no error will be raised but the returned state will be corrupted.
    pub fn make_move(self, mv: Move) -> Result<GameState, IllegalMoveError> {
        self.make_move_inner::<true, true>(mv)
    }

    /// Perform a move, accelerated for quiescent search
    pub(crate) fn make_move_quiescent(self, mv: Move) -> Result<GameState, IllegalMoveError> {
        self.make_move_inner::<false, true>(mv)
    }

    /// Perform a move, accelerated for static exchange evaluation
    pub(crate) fn make_move_exchange_eval(self, mv: Move) -> GameState {
        self.make_move_inner::<false, false>(mv).unwrap()
    }

    /// Perform a move.
    ///
    /// When the UPDATE_STATE flag is not set, this function will take some computational shortcuts,
    /// for instance by not updating the castling rights flags.
    fn make_move_inner<const UPDATE_STATE: bool, const CHECK_LEGAL: bool>(
        mut self,
        mv: Move,
    ) -> Result<GameState, IllegalMoveError> {
        let Move(mv) = mv;
        let victim = self.pieces.get(mv.to);

        // Do the move
        let mut new_en_passant_target = None;
        match mv.flag {
            MoveFlag::Normal => {
                if let Some((_, kind)) = victim {
                    self.remove_piece(mv.to, self.side_to_move.opposite(), kind);
                }
                self.remove_piece(mv.from, self.side_to_move, mv.kind);
                self.put_piece(mv.to, self.side_to_move, mv.kind);
            }
            MoveFlag::Promotion(prom) => {
                if let Some((_, kind)) = victim {
                    self.remove_piece(mv.to, self.side_to_move.opposite(), kind);
                }
                self.remove_piece(mv.from, self.side_to_move, Pawn);
                self.put_piece(mv.to, self.side_to_move, prom);
            }
            MoveFlag::EnPassant => {
                let target = SquareIndex::from_coords(
                    self.en_passant.expect("En passant has no target"),
                    RankIndex::_5,
                );
                self.remove_piece(target, self.side_to_move.opposite(), Pawn);
                self.remove_piece(mv.from, self.side_to_move, Pawn);
                self.put_piece(mv.to, self.side_to_move, Pawn);
            }
            MoveFlag::DoublePush => {
                let (file, _) = mv.to.coords();
                new_en_passant_target = Some(file);
                self.remove_piece(mv.from, self.side_to_move, Pawn);
                self.put_piece(mv.to, self.side_to_move, Pawn);
            }
            f @ (MoveFlag::CastleEast | MoveFlag::CastleWest) => {
                let castle_side = match f {
                    MoveFlag::CastleEast => CastleSide::East,
                    MoveFlag::CastleWest => CastleSide::West,
                    _ => unreachable!(),
                };
                let (king_to, rook_to) = castle_destinations(castle_side);
                if CHECK_LEGAL {
                    let blockers = self.friends_bb.union() | self.enemies_bb.union();
                    for sq in SquareIter(lut::castle_ray(mv.from, king_to)) {
                        if lut::is_dangerous(sq, &self.enemies_bb, blockers) {
                            return Err(IllegalMoveError); // Cannot castle through danger
                        }
                    }
                }
                self.remove_piece(mv.from, self.side_to_move, King);
                self.remove_piece(mv.to, self.side_to_move, Rook);
                self.put_piece(rook_to, self.side_to_move, Rook);
                self.put_piece(king_to, self.side_to_move, King);
            }
        }

        if CHECK_LEGAL {
            // Check that the move does not put the king in danger
            let blockers = self.friends_bb.union() | self.enemies_bb.union();
            for sq in SquareIter(self.friends_bb[King]) {
                if lut::is_dangerous(sq, &self.enemies_bb, blockers) {
                    return Err(IllegalMoveError);
                }
            }
        }

        if UPDATE_STATE {
            // Revoke own castle rights
            if mv.kind == King {
                self.friends_castle.set(CastleSide::East, None);
                self.friends_castle.set(CastleSide::West, None);
            } else if mv.kind == Rook {
                let (file, rank) = mv.from.coords();
                if rank == RankIndex::_1 {
                    for castle_side in [CastleSide::East, CastleSide::West] {
                        if Some(file) == self.friends_castle.get(castle_side) {
                            self.friends_castle.set(castle_side, None)
                        }
                    }
                }
            }

            // Revoke enemy's castle rights
            if let Some((_, Rook)) = victim {
                let (file, rank) = mv.to.coords();
                if rank == RankIndex::_8 {
                    for castle_side in [CastleSide::East, CastleSide::West] {
                        if Some(file) == self.enemies_castle.get(castle_side) {
                            self.enemies_castle.set(castle_side, None);
                        }
                    }
                }
            }

            // Flip sides
            Ok(GameState {
                pieces: self.pieces.mirror(),
                friends_bb: self.enemies_bb.mirror(),
                enemies_bb: self.friends_bb.mirror(),
                side_to_move: self.side_to_move.opposite(),
                friends_castle: self.enemies_castle,
                enemies_castle: self.friends_castle,
                en_passant: new_en_passant_target,
                fullmoves_count: match self.side_to_move {
                    PlayerSide::White => self.fullmoves_count,
                    PlayerSide::Black => self.fullmoves_count + 1,
                },
                fiftymove_count: if mv.kind == Pawn || victim.is_some() {
                    0
                } else {
                    self.fiftymove_count + 1
                },
            })
        } else {
            // Flip sides
            Ok(GameState {
                pieces: self.pieces.mirror(),
                friends_bb: self.enemies_bb.mirror(),
                enemies_bb: self.friends_bb.mirror(),
                side_to_move: self.side_to_move.opposite(),
                en_passant: None,
                ..self
            })
        }
    }

    /// Make a "null move", which means that the active player skips its turn
    pub fn make_move_null(self) -> Result<GameState, IllegalMoveError> {
        // Check that the move does not put the king in danger
        let blockers = self.friends_bb.union() | self.enemies_bb.union();
        for sq in SquareIter(self.friends_bb[King]) {
            if lut::is_dangerous(sq, &self.enemies_bb, blockers) {
                return Err(IllegalMoveError);
            }
        }

        // Flip sides
        Ok(GameState {
            pieces: self.pieces.mirror(),
            friends_bb: self.enemies_bb.mirror(),
            enemies_bb: self.friends_bb.mirror(),
            side_to_move: self.side_to_move.opposite(),
            friends_castle: self.enemies_castle,
            enemies_castle: self.friends_castle,
            en_passant: None,
            ..self
        })
    }

    /// Find the cheapest friendly piece in the from_mask that can attack the given square
    pub(crate) fn cheapest_attacker(&self, sq: SquareIndex) -> Option<Move> {
        // Pawn regular move
        let sq_bb = sq.bb().get();
        let attackers = (lut::shift_se(sq_bb) | lut::shift_sw(sq_bb))
            & self.friends_bb[Pawn]
            & !RankIndex::_7.bb().get();
        if let Some(from) = SquareIter(attackers).next() {
            return Some(Move::normal(Pawn, from, sq));
        }
        // Knight moves
        let attackers = lut::knight_reachable(sq) & self.friends_bb[Knight];
        if let Some(from) = SquareIter(attackers).next() {
            return Some(Move::normal(Knight, from, sq));
        }
        // Bishop, Rook and Queen moves, in that order
        let blockers = self.friends_bb.union() | self.enemies_bb.union();
        if let Some(from) = lut::get_attacked_by_bishop(sq, blockers, self.friends_bb[Bishop]) {
            return Some(Move::normal(Bishop, from, sq));
        }
        if let Some(from) = lut::get_attacked_by_rook(sq, blockers, self.friends_bb[Rook]) {
            return Some(Move::normal(Rook, from, sq));
        }
        if let Some(from) = lut::get_attacked_by_bishop(sq, blockers, self.friends_bb[Queen]) {
            return Some(Move::normal(Queen, from, sq));
        }
        if let Some(from) = lut::get_attacked_by_rook(sq, blockers, self.friends_bb[Queen]) {
            return Some(Move::normal(Queen, from, sq));
        }
        // Pawn promotion
        let attackers = (lut::shift_se(sq_bb) | lut::shift_sw(sq_bb))
            & self.friends_bb[Pawn]
            & RankIndex::_7.bb().get();
        if let Some(from) = SquareIter(attackers).next() {
            return Some(Move::with_flag(Pawn, from, sq, MoveFlag::Promotion(Queen)));
        }
        // King move
        let attackers = lut::king_reachable(sq) & self.friends_bb[King];
        if let Some(from) = SquareIter(attackers).next() {
            return Some(Move::normal(King, from, sq));
        }
        None
    }

    pub fn is_check(&self) -> bool {
        let blockers = self.friends_bb.union() | self.enemies_bb.union();
        SquareIter(self.friends_bb[King])
            .any(|sq| lut::is_dangerous(sq, &self.enemies_bb, blockers))
    }
}

/// Get the destination squares for the king and rook during castling
fn castle_destinations(castle_side: CastleSide) -> (SquareIndex, SquareIndex) {
    match castle_side {
        CastleSide::East => (SquareIndex::G1, SquareIndex::F1),
        CastleSide::West => (SquareIndex::C1, SquareIndex::D1),
    }
}

impl Move {
    fn with_flag(kind: PieceKind, from: SquareIndex, to: SquareIndex, flag: MoveFlag) -> Move {
        Move(MoveInfo {
            kind,
            from,
            to,
            flag,
        })
    }

    fn normal(kind: PieceKind, from: SquareIndex, to: SquareIndex) -> Move {
        Move::with_flag(kind, from, to, MoveFlag::Normal)
    }

    fn castle(castle_side: CastleSide, king_from: SquareIndex, rook_from: SquareIndex) -> Move {
        Move::with_flag(
            King,
            king_from,
            rook_from,
            match castle_side {
                CastleSide::East => MoveFlag::CastleEast,
                CastleSide::West => MoveFlag::CastleWest,
            },
        )
    }
}
