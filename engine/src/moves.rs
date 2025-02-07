use crate::{
    game_state::{GameState, PieceBitboards},
    lookup_tables as lut, PieceKind, PlayerSide, RankIndex, SquareIndex, SquareIter,
};

/// Opaque move token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    kind: PieceKind,
    /// Note: square is mirrored when it's a black move
    from: SquareIndex,
    /// Note: square is mirrored when it's a black move
    to: SquareIndex,
    flag: MoveFlag,
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

/// An error raised when a move puts the king in check.
#[derive(Debug, Clone, Copy)]
pub struct IllegalMoveError;

impl GameState {
    /// Get info about a move.
    /// This function must be called with a move that has been generated from this state!
    pub fn move_info(&self, mv: Move) -> MoveInfo {
        let (start, end) = match self.side_to_move {
            PlayerSide::White => (mv.from, mv.to),
            PlayerSide::Black => (mv.from.mirror(), mv.to.mirror()),
        };
        MoveInfo {
            kind: mv.kind,
            from: start,
            to: end,
            flag: mv.flag,
        }
    }

    /// Collect all the pseudo-legal moves from a given state.
    pub fn pseudo_legal_moves<F: FnMut(Move)>(&self, f: F) {
        gen_moves::<false, F>(self, f)
    }

    /// Collect the pseudo-legal moves from a given state, but only the captures.
    pub fn pseudo_legal_captures<F: FnMut(Move)>(&self, f: F) {
        gen_moves::<true, F>(self, f)
    }

    /// Make a fast evaluation of a move.
    /// NOTE: The "score" returned by this function has nothing to do with
    /// the "score" of a gamestate, the latter being represented by the `Score` type.
    pub fn eval_move(&self, mv: Move) -> i16 {
        let mut score = 0;

        // Bonus for capturing an enemy with a cheap friend
        if let Some((_, kind)) = self.pieces.get(mv.to) {
            score += 10 * lut::piece_value(kind) - lut::piece_value(mv.kind);
        }

        // Bonus for promoting a piece
        if let MoveFlag::Promotion(kind) = mv.flag {
            score += 2000 + lut::piece_value(kind)
        }

        score
    }

    /// Perform a move, then flip the position of enemies and friends.
    /// This function must be called with a move that has been generated from this state!
    /// If not, then probably no error will be raised but the returned state will be corrupted.
    pub fn make_move(mut self, mv: Move) -> Result<GameState, IllegalMoveError> {
        // Capture and displace
        if let Some((_, kind)) = self.pieces.get(mv.to) {
            self.remove_piece(mv.to, self.side_to_move.opposite(), kind);
        }
        self.remove_piece(mv.from, self.side_to_move, mv.kind);
        self.put_piece(mv.to, self.side_to_move, mv.kind);

        // Handle special moves
        let mut new_en_passant_target = None;
        match mv.flag {
            MoveFlag::Normal => {}
            MoveFlag::Promotion(prom) => {
                self.remove_piece(mv.to, self.side_to_move, PieceKind::Pawn);
                self.put_piece(mv.to, self.side_to_move, prom);
            }
            MoveFlag::EnPassant => {
                let target = SquareIndex::from_coords(
                    self.flags.en_passant().expect("En passant has no target"),
                    RankIndex::_5,
                );
                self.remove_piece(target, self.side_to_move.opposite(), PieceKind::Pawn);
            }
            MoveFlag::DoublePush => {
                let (file, _) = mv.to.coords();
                new_en_passant_target = Some(file);
            }
            MoveFlag::CastleEast => {
                let blockers = self.friends_bb.union() | self.enemies_bb.union();
                if is_dangerous(SquareIndex::E1, self.enemies_bb, blockers)
                    || is_dangerous(SquareIndex::F1, self.enemies_bb, blockers)
                {
                    return Err(IllegalMoveError); // Cannot castle through danger
                }
                self.remove_piece(SquareIndex::H1, self.side_to_move, PieceKind::Rook);
                self.put_piece(SquareIndex::F1, self.side_to_move, PieceKind::Rook);
            }
            MoveFlag::CastleWest => {
                let blockers = self.friends_bb.union() | self.enemies_bb.union();
                if is_dangerous(SquareIndex::E1, self.enemies_bb, blockers)
                    || is_dangerous(SquareIndex::D1, self.enemies_bb, blockers)
                {
                    return Err(IllegalMoveError); // Cannot castle through danger
                }
                self.remove_piece(SquareIndex::A1, self.side_to_move, PieceKind::Rook);
                self.put_piece(SquareIndex::D1, self.side_to_move, PieceKind::Rook);
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
            self.flags.set_castle_east(self.side_to_move, false);
            self.flags.set_castle_west(self.side_to_move, false);
        } else if mv.kind == PieceKind::Rook {
            if mv.from == SquareIndex::H1 {
                self.flags.set_castle_east(self.side_to_move, false);
            } else if mv.from == SquareIndex::A1 {
                self.flags.set_castle_west(self.side_to_move, false);
            }
        }

        // Revoke enemy's castle rights
        if mv.to == SquareIndex::H8 {
            self.flags
                .set_castle_east(self.side_to_move.opposite(), false);
        } else if mv.to == SquareIndex::A8 {
            self.flags
                .set_castle_west(self.side_to_move.opposite(), false);
        }

        // Flip sides
        self.flags.set_en_passant(new_en_passant_target);
        Ok(GameState {
            pieces: self.pieces.mirror(),
            friends_bb: self.enemies_bb.mirror(),
            enemies_bb: self.friends_bb.mirror(),
            side_to_move: self.side_to_move.opposite(),
            flags: self.flags,
            fullmoves: match self.side_to_move {
                PlayerSide::White => self.fullmoves,
                PlayerSide::Black => self.fullmoves + 1,
            },
            material_value: -self.material_value,
        })
    }

    pub fn is_check(&self) -> bool {
        let blockers = self.friends_bb.union() | self.enemies_bb.union();
        SquareIter(self.friends_bb[PieceKind::King])
            .any(|sq| is_dangerous(sq, self.enemies_bb, blockers))
    }
}

fn knight_reachable(pos: SquareIndex) -> u64 {
    lut::KNIGHT_REACHABLE[pos as usize]
}

fn bishop_reachable(pos: SquareIndex, blockers: u64) -> u64 {
    let lut::Rays { ne, nw, se, sw, .. } = lut::RAYS[pos as usize];
    let mut result = ne | nw | se | sw;

    let ne_collision = (ne & blockers) | SquareIndex::H8.bb();
    result ^= lut::RAYS[ne_collision.trailing_zeros() as usize].ne;

    let nw_collision = (nw & blockers) | SquareIndex::H8.bb();
    result ^= lut::RAYS[nw_collision.trailing_zeros() as usize].nw;

    let se_collision = (se & blockers) | SquareIndex::A1.bb();
    result ^= lut::RAYS[63 - se_collision.leading_zeros() as usize].se;

    let sw_collision = (sw & blockers) | SquareIndex::A1.bb();
    result ^= lut::RAYS[63 - sw_collision.leading_zeros() as usize].sw;

    result
}

fn rook_reachable(pos: SquareIndex, blockers: u64) -> u64 {
    let lut::Rays { n, s, e, w, .. } = lut::RAYS[pos as usize];
    let mut result = n | s | e | w;

    let n_collision = (n & blockers) | (1 << 63);
    result ^= lut::RAYS[n_collision.trailing_zeros() as usize].n;

    let e_collision = (e & blockers) | (1 << 63);
    result ^= lut::RAYS[e_collision.trailing_zeros() as usize].e;

    let s_collision = (s & blockers) | 1;
    result ^= lut::RAYS[63 - s_collision.leading_zeros() as usize].s;

    let w_collision = (w & blockers) | 1;
    result ^= lut::RAYS[63 - w_collision.leading_zeros() as usize].w;

    result
}

fn king_reachable(pos: SquareIndex) -> u64 {
    lut::KING_REACHABLE[pos as usize]
}

fn is_dangerous(sq: SquareIndex, enemies_bb: PieceBitboards, obstacles: u64) -> bool {
    let sq_bb = sq.bb().get();
    let mut attackers = 0;
    attackers |= (lut::shift_ne(sq_bb) | lut::shift_nw(sq_bb)) & enemies_bb[PieceKind::Pawn];
    attackers |= knight_reachable(sq) & enemies_bb[PieceKind::Knight];
    attackers |= bishop_reachable(sq, obstacles)
        & (enemies_bb[PieceKind::Bishop] | enemies_bb[PieceKind::Queen]);
    attackers |= rook_reachable(sq, obstacles)
        & (enemies_bb[PieceKind::Rook] | enemies_bb[PieceKind::Queen]);
    attackers |= king_reachable(sq) & enemies_bb[PieceKind::King];
    attackers != 0
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
        for to in SquareIter(knight_reachable(from) & dst_mask) {
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
        for to in SquareIter(bishop_reachable(from, blockers) & dst_mask) {
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
        for to in SquareIter(rook_reachable(from, blockers) & dst_mask) {
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
            (bishop_reachable(from, blockers) | rook_reachable(from, blockers)) & dst_mask,
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
        for to in SquareIter(king_reachable(from) & dst_mask) {
            f(Move {
                kind: PieceKind::King,
                from,
                to,
                flag: MoveFlag::Normal,
            })
        }
    }

    // En passant
    if let Some(file) = gs.flags.en_passant() {
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
        if gs.flags.castle_east(gs.side_to_move)
            && ((SquareIndex::F1.bb() | SquareIndex::G1.bb()).get() & blockers) == 0
        {
            f(Move {
                kind: PieceKind::King,
                from: SquareIndex::E1,
                to: SquareIndex::G1,
                flag: MoveFlag::CastleEast,
            })
        }
        if gs.flags.castle_west(gs.side_to_move)
            && ((SquareIndex::B1.bb() | SquareIndex::C1.bb() | SquareIndex::D1.bb()).get()
                & blockers)
                == 0
        {
            f(Move {
                kind: PieceKind::King,
                from: SquareIndex::E1,
                to: SquareIndex::C1,
                flag: MoveFlag::CastleWest,
            })
        }
    }
}
