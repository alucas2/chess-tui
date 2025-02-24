use crate::{
    game_state::{GameState, PieceBitboards},
    lookup_tables as lut, PieceKind, PlayerSide, RankIndex, SquareIndex, SquareIter,
};

/// Opaque move token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub(crate) kind: PieceKind,
    /// Note: square is mirrored when it's a black move
    pub(crate) from: SquareIndex,
    /// Note: square is mirrored when it's a black move.
    /// For castling, it is the rook's starting square instead the king's landing square
    pub(crate) to: SquareIndex,
    pub(crate) flag: MoveFlag,
}

/// Information about a move
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveInfo {
    pub kind: PieceKind,
    pub from: SquareIndex,
    pub to: SquareIndex,
    pub flag: MoveFlag,
}

/// Flag for a special move.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveFlag {
    Normal,
    Promotion(PieceKind),
    EnPassant,
    DoublePush,
    CastleEast,
    CastleWest,
}

impl Move {
    /// Get info about a move.
    /// This function must be called with the state that this move originate from!
    pub fn info(&self, gs: &GameState) -> MoveInfo {
        let (start, end) = match gs.side_to_move {
            PlayerSide::White => (self.from, self.to),
            PlayerSide::Black => (self.from.mirror(), self.to.mirror()),
        };
        MoveInfo {
            kind: self.kind,
            from: start,
            to: end,
            flag: self.flag,
        }
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
    /// Collect all the pseudo-legal moves from a given state.
    pub fn pseudo_legal_moves<F: FnMut(Move)>(&self, f: F) {
        gen_moves::<false, F>(self, f)
    }

    /// Collect the pseudo-legal moves from a given state, but only the captures.
    pub fn pseudo_legal_captures<F: FnMut(Move)>(&self, f: F) {
        gen_moves::<true, F>(self, f)
    }

    /// Perform a move, then flip the position of enemies and friends.
    /// This function must be called with a move that has been generated from this state!
    /// If not, then probably no error will be raised but the returned state will be corrupted.
    pub fn make_move(mut self, mv: Move) -> Result<GameState, IllegalMoveError> {
        // Revoke enemy's castle rights
        if let Some((_, PieceKind::Rook)) = self.pieces.get(mv.to) {
            let (file, rank) = mv.to.coords();
            if rank == RankIndex::_8 {
                if Some(file) == self.enemies_castle.east() {
                    self.enemies_castle.set_east(None);
                } else if Some(file) == self.enemies_castle.west() {
                    self.enemies_castle.set_west(None);
                }
            }
        }

        // Do the move
        let mut new_en_passant_target = None;
        match mv.flag {
            MoveFlag::Normal => {
                if let Some((_, kind)) = self.pieces.get(mv.to) {
                    self.remove_piece(mv.to, self.side_to_move.opposite(), kind);
                }
                self.remove_piece(mv.from, self.side_to_move, mv.kind);
                self.put_piece(mv.to, self.side_to_move, mv.kind);
            }
            MoveFlag::Promotion(prom) => {
                if let Some((_, kind)) = self.pieces.get(mv.to) {
                    self.remove_piece(mv.to, self.side_to_move.opposite(), kind);
                }
                self.remove_piece(mv.from, self.side_to_move, PieceKind::Pawn);
                self.put_piece(mv.to, self.side_to_move, prom);
            }
            MoveFlag::EnPassant => {
                let target = SquareIndex::from_coords(
                    self.en_passant.expect("En passant has no target"),
                    RankIndex::_5,
                );
                self.remove_piece(target, self.side_to_move.opposite(), PieceKind::Pawn);
                self.remove_piece(mv.from, self.side_to_move, PieceKind::Pawn);
                self.put_piece(mv.to, self.side_to_move, PieceKind::Pawn);
            }
            MoveFlag::DoublePush => {
                let (file, _) = mv.to.coords();
                new_en_passant_target = Some(file);
                self.remove_piece(mv.from, self.side_to_move, PieceKind::Pawn);
                self.put_piece(mv.to, self.side_to_move, PieceKind::Pawn);
            }
            MoveFlag::CastleEast => {
                let blockers = self.friends_bb.union() | self.enemies_bb.union();
                for sq in SquareIter(lut::castle_ray(mv.from, SquareIndex::G1)) {
                    if is_dangerous(sq, self.enemies_bb, blockers) {
                        return Err(IllegalMoveError); // Cannot castle through danger
                    }
                }
                self.remove_piece(mv.from, self.side_to_move, PieceKind::King);
                self.remove_piece(mv.to, self.side_to_move, PieceKind::Rook);
                self.put_piece(SquareIndex::F1, self.side_to_move, PieceKind::Rook);
                self.put_piece(SquareIndex::G1, self.side_to_move, PieceKind::King);
            }
            MoveFlag::CastleWest => {
                let blockers = self.friends_bb.union() | self.enemies_bb.union();
                for sq in SquareIter(lut::castle_ray(mv.from, SquareIndex::C1)) {
                    if is_dangerous(sq, self.enemies_bb, blockers) {
                        return Err(IllegalMoveError); // Cannot castle through danger
                    }
                }
                self.remove_piece(mv.from, self.side_to_move, PieceKind::King);
                self.remove_piece(mv.to, self.side_to_move, PieceKind::Rook);
                self.put_piece(SquareIndex::D1, self.side_to_move, PieceKind::Rook);
                self.put_piece(SquareIndex::C1, self.side_to_move, PieceKind::King);
            }
        }

        // Check that the move does not put the king in danger
        let blockers = self.friends_bb.union() | self.enemies_bb.union();
        for sq in SquareIter(self.friends_bb[PieceKind::King]) {
            if is_dangerous(sq, self.enemies_bb, blockers) {
                return Err(IllegalMoveError);
            }
        }

        // Revoke own castle rights
        if mv.kind == PieceKind::King {
            self.friends_castle.set_east(None);
            self.friends_castle.set_west(None);
        } else if mv.kind == PieceKind::Rook {
            let (file, rank) = mv.from.coords();
            if rank == RankIndex::_1 {
                if Some(file) == self.friends_castle.east() {
                    self.friends_castle.set_east(None);
                } else if Some(file) == self.friends_castle.west() {
                    self.friends_castle.set_west(None);
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
            fullmoves: match self.side_to_move {
                PlayerSide::White => self.fullmoves,
                PlayerSide::Black => self.fullmoves + 1,
            },
        })
    }

    pub fn is_check(&self) -> bool {
        let blockers = self.friends_bb.union() | self.enemies_bb.union();
        SquareIter(self.friends_bb[PieceKind::King])
            .any(|sq| is_dangerous(sq, self.enemies_bb, blockers))
    }
}

/// Push pseudo-legal moves into a Vec.
///
/// We use a const parameter here to force the existence of two monomorphized versions of this function.
fn gen_moves<const JUST_CAPTURES: bool, F: FnMut(Move)>(gs: &GameState, mut f: F) {
    let friends_bb_union = gs.friends_bb.union();
    let enemies_bb_union = gs.enemies_bb.union();
    let blockers = friends_bb_union | enemies_bb_union;

    let dst_mask = if JUST_CAPTURES {
        enemies_bb_union
    } else {
        !friends_bb_union
    };

    // Pawn regular moves
    for from in SquareIter(gs.friends_bb[PieceKind::Pawn] & !RankIndex::_7.bb().get()) {
        let from_bb = from.bb().get();
        let captures = (lut::shift_ne(from_bb) | lut::shift_nw(from_bb)) & enemies_bb_union;
        if JUST_CAPTURES {
            for to in SquareIter(captures) {
                f(Move {
                    kind: PieceKind::Pawn,
                    from,
                    to,
                    flag: MoveFlag::Normal,
                })
            }
        } else {
            let single_push = lut::shift_n(from_bb) & !blockers;
            for to in SquareIter(captures | single_push) {
                f(Move {
                    kind: PieceKind::Pawn,
                    from,
                    to,
                    flag: MoveFlag::Normal,
                })
            }
            for to in SquareIter(lut::shift_n(single_push) & !blockers & RankIndex::_4.bb().get()) {
                f(Move {
                    kind: PieceKind::Pawn,
                    from,
                    to,
                    flag: MoveFlag::DoublePush,
                })
            }
        }
    }

    // Pawn promotions
    for from in SquareIter(gs.friends_bb[PieceKind::Pawn] & RankIndex::_7.bb().get()) {
        const PROMOTIONS: [PieceKind; 4] = [
            PieceKind::Queen,
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
        ];
        let from_bb = from.bb().get();
        let captures = (lut::shift_ne(from_bb) | lut::shift_nw(from_bb)) & enemies_bb_union;
        if JUST_CAPTURES {
            for to in SquareIter(captures) {
                for prom in PROMOTIONS {
                    f(Move {
                        kind: PieceKind::Pawn,
                        from,
                        to,
                        flag: MoveFlag::Promotion(prom),
                    })
                }
            }
        } else {
            let single_push = lut::shift_n(from_bb) & !blockers;
            for to in SquareIter(captures | single_push) {
                for prom in PROMOTIONS {
                    f(Move {
                        kind: PieceKind::Pawn,
                        from,
                        to,
                        flag: MoveFlag::Promotion(prom),
                    })
                }
            }
        }
    }

    // Knight moves
    for from in SquareIter(gs.friends_bb[PieceKind::Knight]) {
        for to in SquareIter(lut::knight_reachable(from) & dst_mask) {
            f(Move {
                kind: PieceKind::Knight,
                from,
                to,
                flag: MoveFlag::Normal,
            })
        }
    }

    // Bishop moves
    for from in SquareIter(gs.friends_bb[PieceKind::Bishop]) {
        for to in SquareIter(lut::bishop_reachable(from, blockers) & dst_mask) {
            f(Move {
                kind: PieceKind::Bishop,
                from,
                to,
                flag: MoveFlag::Normal,
            })
        }
    }

    // Rook moves
    for from in SquareIter(gs.friends_bb[PieceKind::Rook]) {
        for to in SquareIter(lut::rook_reachable(from, blockers) & dst_mask) {
            f(Move {
                kind: PieceKind::Rook,
                from,
                to,
                flag: MoveFlag::Normal,
            })
        }
    }

    // Queen moves
    for from in SquareIter(gs.friends_bb[PieceKind::Queen]) {
        for to in SquareIter(
            (lut::bishop_reachable(from, blockers) | lut::rook_reachable(from, blockers))
                & dst_mask,
        ) {
            f(Move {
                kind: PieceKind::Queen,
                from,
                to,
                flag: MoveFlag::Normal,
            })
        }
    }

    // King moves
    for from in SquareIter(gs.friends_bb[PieceKind::King]) {
        for to in SquareIter(lut::king_reachable(from) & dst_mask) {
            f(Move {
                kind: PieceKind::King,
                from,
                to,
                flag: MoveFlag::Normal,
            })
        }
    }

    // En passant
    if let Some(file) = gs.en_passant {
        let to = SquareIndex::from_coords(file, RankIndex::_6);
        let to_bb = to.bb().get();
        let capturers =
            (lut::shift_se(to_bb) | lut::shift_sw(to_bb)) & gs.friends_bb[PieceKind::Pawn];
        for from in SquareIter(capturers) {
            f(Move {
                kind: PieceKind::Pawn,
                from,
                to,
                flag: MoveFlag::EnPassant,
            })
        }
    }

    // Castle
    if !JUST_CAPTURES {
        if let Some(file) = gs.friends_castle.east() {
            if let Some(king_sq) = SquareIter(gs.friends_bb[PieceKind::King]).next() {
                let rook_sq = SquareIndex::from_coords(file, RankIndex::_1);
                let blockers = blockers & !king_sq.bb().get() & !rook_sq.bb().get();
                if lut::castle_ray(king_sq, SquareIndex::G1) & blockers == 0
                    && lut::castle_ray(rook_sq, SquareIndex::F1) & blockers == 0
                {
                    f(Move {
                        kind: PieceKind::King,
                        from: king_sq,
                        to: rook_sq,
                        flag: MoveFlag::CastleEast,
                    })
                }
            }
        }
        if let Some(file) = gs.friends_castle.west() {
            if let Some(king_sq) = SquareIter(gs.friends_bb[PieceKind::King]).next() {
                let rook_sq = SquareIndex::from_coords(file, RankIndex::_1);
                let blockers = blockers & !king_sq.bb().get() & !rook_sq.bb().get();
                if lut::castle_ray(king_sq, SquareIndex::C1) & blockers == 0
                    && lut::castle_ray(rook_sq, SquareIndex::D1) & blockers == 0
                {
                    f(Move {
                        kind: PieceKind::King,
                        from: king_sq,
                        to: rook_sq,
                        flag: MoveFlag::CastleWest,
                    })
                }
            }
        }
    }
}

/// Check whether a square is attackable by an enemy
fn is_dangerous(sq: SquareIndex, enemies_bb: PieceBitboards, obstacles: u64) -> bool {
    let sq_bb = sq.bb().get();
    let mut attackers = 0;
    attackers |= (lut::shift_ne(sq_bb) | lut::shift_nw(sq_bb)) & enemies_bb[PieceKind::Pawn];
    attackers |= lut::knight_reachable(sq) & enemies_bb[PieceKind::Knight];
    attackers |= lut::bishop_reachable(sq, obstacles)
        & (enemies_bb[PieceKind::Bishop] | enemies_bb[PieceKind::Queen]);
    attackers |= lut::rook_reachable(sq, obstacles)
        & (enemies_bb[PieceKind::Rook] | enemies_bb[PieceKind::Queen]);
    attackers |= lut::king_reachable(sq) & enemies_bb[PieceKind::King];
    attackers != 0
}
