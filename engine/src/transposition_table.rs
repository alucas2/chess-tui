use rustc_hash::FxHasher;

use std::hash::{Hash, Hasher};

use crate::{
    atomic_cell::AtomicCell,
    game_state::{CastleRights, CompactPieceArray, GameState},
    FileIndex, PlayerSide,
};

/// Size of a table entry is:
/// `8 + 36 + size_of::<X> + size_of::<V>`
/// Where 8 corresponds to the size of the seqlock and 36 the size of the gamestate
pub struct Table<X, V> {
    entries: Vec<AtomicCell<Option<Entry<X, V>>>>,
}

pub struct TableKey<X> {
    tag: Tag<X>,
    hash: u64,
}

/// size is at least: 36 + sizeof::<X>
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Tag<X> {
    pieces: CompactPieceArray,
    side_to_move: PlayerSide,
    friends_castle: CastleRights,
    enemies_castle: CastleRights,
    en_passant: Option<FileIndex>,
    extra: X,
}

#[derive(Clone, Copy)]
struct Entry<X, V> {
    tag: Tag<X>,
    value: V,
}

impl<X: Hash> TableKey<X> {
    pub fn new(gs: &GameState, extra: X) -> TableKey<X> {
        let tag = Tag {
            pieces: gs.pieces,
            side_to_move: gs.side_to_move,
            friends_castle: gs.friends_castle,
            enemies_castle: gs.enemies_castle,
            en_passant: gs.en_passant,
            extra,
        };
        let mut hash = FxHasher::default();
        tag.hash(&mut hash);
        let hash = hash.finish();
        TableKey { tag, hash }
    }
}

impl<X: Eq + Copy, V: Copy> Table<X, V> {
    pub fn new(capacity: usize) -> Table<X, V> {
        assert_eq!(size_of::<AtomicCell<Option<Entry<X, V>>>>(), 64);
        Table {
            entries: (0..capacity).map(|_| AtomicCell::new(None)).collect(),
        }
    }

    pub fn lookup(&self, key: &TableKey<X>) -> Option<V> {
        let index = key.hash as usize % self.entries.len();
        match self.entries[index].try_load()? {
            Some(entry) if entry.tag == key.tag => Some(entry.value),
            _ => None,
        }
    }

    pub fn update(&self, key: TableKey<X>, value: V) -> bool {
        let index = key.hash as usize % self.entries.len();
        self.entries[index].try_store(Some(Entry {
            tag: key.tag,
            value,
        }))
    }
}
