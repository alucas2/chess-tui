use std::num::NonZeroU64;

use crate::{
    game_state::{GameState, PieceBitboards, PieceKind, PlayerColor, RankIndex, SquareIndex},
    lookup_tables as lut,
};

/// Opaque move token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub(crate) who: PieceKind,
    /// Note: src is mirrored when it's a black move
    pub(crate) src: SquareIndex,
    /// Note: dst is mirrored when it's a black move
    pub(crate) dst: SquareIndex,
    pub(crate) flag: MoveFlag,
}

/// Information about a move
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveInfo {
    pub who: PieceKind,
    pub start: SquareIndex,
    pub end: SquareIndex,
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
        let (start, end) = match self.friends_color {
            PlayerColor::White => (mv.src, mv.dst),
            PlayerColor::Black => (mv.src.mirror(), mv.dst.mirror()),
        };
        MoveInfo {
            who: mv.who,
            start,
            end,
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

    /// Perform a move, then flip the position of enemies and friends.
    /// This function must be called with a move that has been generated from this state!
    /// If not, then probably no error will be raised but the returned state will be corrupted.
    pub fn do_move(&self, mv: Move) -> Result<GameState, IllegalMoveError> {
        let GameState {
            mut pieces,
            mut friends_bb,
            mut enemies_bb,
            friends_color,
            mut friends_castle,
            mut enemies_castle,
            en_passant_target,
            fullmoves,
        } = self.clone();
        let mut new_en_passant_target = None;
        let dst_bb = mv.dst.bb().get();
        let src_bb = mv.src.bb().get();
        let move_bb = src_bb | dst_bb;

        // Capture
        if let Some((_, kind)) = pieces.get(mv.dst) {
            enemies_bb[kind] ^= dst_bb;
        }

        // Move
        pieces.set(mv.src, None);
        pieces.set(mv.dst, Some((friends_color, mv.who)));
        friends_bb[mv.who] ^= move_bb;

        // Special moves
        match mv.flag {
            MoveFlag::Promotion(prom) => {
                pieces.set(mv.dst, Some((friends_color, prom)));
                friends_bb[prom] ^= dst_bb;
                friends_bb[PieceKind::Pawn] ^= dst_bb;
            }
            MoveFlag::EnPassant => {
                let en_passant_sq = SquareIndex::from_coords(
                    en_passant_target.expect("En passant has no target"),
                    RankIndex::_5,
                );
                let en_passant_bb = en_passant_sq.bb().get();
                pieces.set(en_passant_sq, None);
                enemies_bb[PieceKind::Pawn] ^= en_passant_bb;
            }
            MoveFlag::DoublePush => {
                let (file, _) = mv.dst.coords();
                new_en_passant_target = Some(file);
            }
            MoveFlag::CastleEast => {
                pieces.set(SquareIndex::H1, None);
                pieces.set(SquareIndex::F1, Some((friends_color, PieceKind::Rook)));
                let rook_move_bb = (SquareIndex::H1.bb() | SquareIndex::F1.bb()).get();
                friends_bb[PieceKind::Rook] ^= rook_move_bb;
            }
            MoveFlag::CastleWest => {
                pieces.set(SquareIndex::A1, None);
                pieces.set(SquareIndex::D1, Some((friends_color, PieceKind::Rook)));
                let rook_move_bb = (SquareIndex::A1.bb() | SquareIndex::D1.bb()).get();
                friends_bb[PieceKind::Rook] ^= rook_move_bb;
            }
            MoveFlag::Normal => {}
        }

        // Check the move's legality
        let blockers = friends_bb.union() | enemies_bb.union();
        for sq in SquareIter(friends_bb[PieceKind::King]) {
            if is_dangerous(sq, enemies_bb, blockers) {
                return Err(IllegalMoveError);
            }
        }
        if (mv.flag == MoveFlag::CastleEast
            && (is_dangerous(SquareIndex::E1, enemies_bb, blockers)
                || is_dangerous(SquareIndex::F1, enemies_bb, blockers)))
            || (mv.flag == MoveFlag::CastleWest
                && (is_dangerous(SquareIndex::E1, enemies_bb, blockers)
                    || is_dangerous(SquareIndex::D1, enemies_bb, blockers)))
        {
            return Err(IllegalMoveError);
        }

        // Revoke own castle rights
        if mv.who == PieceKind::King {
            friends_castle.east = false;
            friends_castle.west = false;
        } else if mv.who == PieceKind::Rook {
            if mv.src == SquareIndex::H1 {
                friends_castle.east = false
            } else if mv.src == SquareIndex::A1 {
                friends_castle.west = false
            }
        }

        // Revoke enemy's castle rights
        if mv.dst == SquareIndex::H8 {
            enemies_castle.east = false
        } else if mv.dst == SquareIndex::A8 {
            enemies_castle.west = false
        }

        // Flip the friends and enemies roles
        Ok(GameState {
            pieces: pieces.mirror(),
            friends_bb: enemies_bb.mirror(),
            enemies_bb: friends_bb.mirror(),
            friends_color: friends_color.opposite(),
            friends_castle: enemies_castle,
            enemies_castle: friends_castle,
            en_passant_target: new_en_passant_target,
            fullmoves: match friends_color {
                PlayerColor::White => fullmoves,
                PlayerColor::Black => fullmoves + 1,
            },
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

/// An iterator over the set squares if a bitboard.
struct SquareIter(pub u64);

impl Iterator for SquareIter {
    type Item = SquareIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let sq = NonZeroU64::new(self.0).map(SquareIndex::from_bb)?;
        self.0 &= self.0 - 1;
        Some(sq)
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
    for src in SquareIter(gs.friends_bb[PieceKind::Pawn] & !RankIndex::_7.bb().get()) {
        let src_bb = src.bb().get();
        let captures = (lut::shift_ne(src_bb) | lut::shift_nw(src_bb)) & enemies_bb_union;
        if JUST_CAPTURES {
            for dst in SquareIter(captures) {
                f(Move {
                    who: PieceKind::Pawn,
                    src,
                    dst,
                    flag: MoveFlag::Normal,
                })
            }
        } else {
            let single_push = lut::shift_n(src_bb) & !blockers;
            for dst in SquareIter(captures | single_push) {
                f(Move {
                    who: PieceKind::Pawn,
                    src,
                    dst,
                    flag: MoveFlag::Normal,
                })
            }
            for dst in SquareIter(lut::shift_n(single_push) & !blockers & RankIndex::_4.bb().get())
            {
                f(Move {
                    who: PieceKind::Pawn,
                    src,
                    dst,
                    flag: MoveFlag::DoublePush,
                })
            }
        }
    }

    // Pawn promotions
    for src in SquareIter(gs.friends_bb[PieceKind::Pawn] & RankIndex::_7.bb().get()) {
        const PROMOTIONS: [PieceKind; 4] = [
            PieceKind::Queen,
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
        ];
        let src_bb = src.bb().get();
        let captures = (lut::shift_ne(src_bb) | lut::shift_nw(src_bb)) & enemies_bb_union;
        if JUST_CAPTURES {
            for dst in SquareIter(captures) {
                for prom in PROMOTIONS {
                    f(Move {
                        who: PieceKind::Pawn,
                        src,
                        dst,
                        flag: MoveFlag::Promotion(prom),
                    })
                }
            }
        } else {
            let single_push = lut::shift_n(src_bb) & !blockers;
            for dst in SquareIter(captures | single_push) {
                for prom in PROMOTIONS {
                    f(Move {
                        who: PieceKind::Pawn,
                        src,
                        dst,
                        flag: MoveFlag::Promotion(prom),
                    })
                }
            }
        }
    }

    // Knight moves
    for src in SquareIter(gs.friends_bb[PieceKind::Knight]) {
        for dst in SquareIter(knight_reachable(src) & dst_mask) {
            f(Move {
                who: PieceKind::Knight,
                src,
                dst,
                flag: MoveFlag::Normal,
            })
        }
    }

    // Bishop moves
    for src in SquareIter(gs.friends_bb[PieceKind::Bishop]) {
        for dst in SquareIter(bishop_reachable(src, blockers) & dst_mask) {
            f(Move {
                who: PieceKind::Bishop,
                src,
                dst,
                flag: MoveFlag::Normal,
            })
        }
    }

    // Rook moves
    for src in SquareIter(gs.friends_bb[PieceKind::Rook]) {
        for dst in SquareIter(rook_reachable(src, blockers) & dst_mask) {
            f(Move {
                who: PieceKind::Rook,
                src,
                dst,
                flag: MoveFlag::Normal,
            })
        }
    }

    // Queen moves
    for src in SquareIter(gs.friends_bb[PieceKind::Queen]) {
        for dst in
            SquareIter((bishop_reachable(src, blockers) | rook_reachable(src, blockers)) & dst_mask)
        {
            f(Move {
                who: PieceKind::Queen,
                src,
                dst,
                flag: MoveFlag::Normal,
            })
        }
    }

    // King moves
    for src in SquareIter(gs.friends_bb[PieceKind::King]) {
        for dst in SquareIter(king_reachable(src) & dst_mask) {
            f(Move {
                who: PieceKind::King,
                src,
                dst,
                flag: MoveFlag::Normal,
            })
        }
    }

    // En passant
    if let Some(file) = gs.en_passant_target {
        let dst = SquareIndex::from_coords(file, RankIndex::_6);
        let dst_bb = dst.bb().get();
        let capturers =
            (lut::shift_se(dst_bb) | lut::shift_sw(dst_bb)) & gs.friends_bb[PieceKind::Pawn];
        for src in SquareIter(capturers) {
            f(Move {
                who: PieceKind::Pawn,
                src,
                dst,
                flag: MoveFlag::EnPassant,
            })
        }
    }

    // Castle
    if !JUST_CAPTURES {
        if gs.friends_castle.east
            && ((SquareIndex::F1.bb() | SquareIndex::G1.bb()).get() & blockers) == 0
        {
            f(Move {
                who: PieceKind::King,
                src: SquareIndex::E1,
                dst: SquareIndex::G1,
                flag: MoveFlag::CastleEast,
            })
        }
        if gs.friends_castle.west
            && ((SquareIndex::B1.bb() | SquareIndex::C1.bb() | SquareIndex::D1.bb()).get()
                & blockers)
                == 0
        {
            f(Move {
                who: PieceKind::King,
                src: SquareIndex::E1,
                dst: SquareIndex::C1,
                flag: MoveFlag::CastleWest,
            })
        }
    }
}
