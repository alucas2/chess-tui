use rustc_hash::FxHasher;

use std::hash::{Hash, Hasher};

use crate::{atomic_cell::AtomicCell, game_state::GameStateKey};

/// Size of a table entry is:
/// `8 + 36 + size_of::<X> + size_of::<V>`
/// Where 8 corresponds to the size of the seqlock and 36 the size of the gamestate
pub struct Table<X, V> {
    entries: Vec<AtomicCell<Option<Entry<X, V>>>>,
}

#[derive(PartialEq, Eq)]
pub struct TableKey<X> {
    hash: u64,
    tag: Tag<X>,
}

/// size is at least: 36 + sizeof::<X>
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Tag<X> {
    gs: GameStateKey,
    extra: X,
}

#[derive(Clone, Copy)]
struct Entry<X, V> {
    tag: Tag<X>,
    value: V,
}

impl From<GameStateKey> for TableKey<()> {
    fn from(gs: GameStateKey) -> Self {
        TableKey::with_extra(gs, ())
    }
}

impl<X: Hash> TableKey<X> {
    pub fn with_extra(gs: GameStateKey, extra: X) -> TableKey<X> {
        let tag = Tag { gs, extra };
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
