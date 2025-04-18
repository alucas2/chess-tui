use std::hash::{Hash, Hasher};

use rustc_hash::FxHasher;

use crate::{CastleRights, CompactPieceArray, FileIndex, PlayerSide};

/// Minimal amount of data that can uniqely identify the state of the game board.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GameStateKey {
    pub(crate) pieces: CompactPieceArray,
    pub(crate) side_to_move: PlayerSide,
    pub(crate) friends_castle: CastleRights,
    pub(crate) enemies_castle: CastleRights,
    pub(crate) en_passant: Option<FileIndex>,
}

/// GameState key with its hash
pub type GameStateKeyWithHash = GameStateKeyExtraWithHash<()>;

/// GameStateKey together with some and extra data and the hash of both
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GameStateKeyExtraWithHash<X> {
    // Put the hash first so its equality is tested first
    // giving the opportunity to short-circuit the key equality test
    pub hash: u64,
    pub key: GameStateKey,
    pub key_extra: X,
}

impl GameStateKey {
    pub fn hash(self) -> GameStateKeyWithHash {
        self.hash_with(())
    }

    pub fn hash_with<X: Hash + Copy>(self, key_extra: X) -> GameStateKeyExtraWithHash<X> {
        let mut hash = FxHasher::default();
        (self, key_extra).hash(&mut hash);
        let hash = hash.finish();
        GameStateKeyExtraWithHash {
            hash,
            key: self,
            key_extra,
        }
    }
}
