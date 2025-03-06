use crate::{
    atomic_cell::AtomicCell,
    game_state_key::{GameStateKeyExtra, GameStateKeyExtraWithHash},
};

pub struct Table<X, V> {
    entries: Vec<AtomicCell<Option<Entry<X, V>>>>,
}

#[derive(Clone, Copy)]
struct Entry<X, V> {
    key: GameStateKeyExtra<X>,
    value: V,
}

impl<X: Eq + Copy, V: Copy> Table<X, V> {
    pub const fn entry_size() -> usize {
        size_of::<AtomicCell<Option<Entry<X, V>>>>()
    }

    pub fn new(capacity: usize) -> Table<X, V> {
        Table {
            entries: (0..capacity).map(|_| AtomicCell::new(None)).collect(),
        }
    }

    pub fn lookup(&self, key: &GameStateKeyExtraWithHash<X>) -> Option<V> {
        let index = key.hash as usize % self.entries.len();
        match self.entries[index].try_load()? {
            Some(entry) if entry.key == key.key => Some(entry.value),
            _ => None,
        }
    }

    pub fn update(&self, key: GameStateKeyExtraWithHash<X>, value: V) -> bool {
        let index = key.hash as usize % self.entries.len();
        self.entries[index].try_store(Some(Entry {
            key: key.key,
            value,
        }))
    }
}
