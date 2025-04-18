use std::sync::{LazyLock, RwLock, RwLockReadGuard};

use crate::{atomic_cell::AtomicCell, GameStateKey, Move};

use super::{evaluate::Score, minmax::Depth};

/// Values that are stored in the table.
/// Its size should not be excessive to fit within a table entry
#[derive(Clone, Copy)]
pub struct TableEntry {
    pub key: GameStateKey,
    pub depth: Depth,
    pub score: Score,
    pub score_kind: ScoreKind,
    pub best: Option<Move>,
}

/// Current size of a table entry is 64 bytes
/// - 56 bytes for the entry itself
/// - 8 bytes for the lock
pub type TableEntryCell = AtomicCell<Option<TableEntry>>;

#[derive(Clone, Copy)]
pub enum ScoreKind {
    Exact,
    AtLeast,
    AtMost,
}

pub fn set_table_size_megabytes(megabytes: usize) {
    let num_entries = megabytes * 1024 * 1024 / size_of::<TableEntryCell>();
    let mut table = TABLE.write().unwrap();
    table.resize_with(num_entries, || TableEntryCell::new(None));
    table.shrink_to_fit();
}

pub fn get() -> RwLockReadGuard<'static, Vec<TableEntryCell>> {
    TABLE.read().unwrap()
}

pub const DEFAULT_TABLE_SIZE_MB: usize = 512;

static TABLE: LazyLock<RwLock<Vec<TableEntryCell>>> = LazyLock::new(|| {
    let num_entries = DEFAULT_TABLE_SIZE_MB * 1024 * 1024 / size_of::<TableEntryCell>();
    let table = (0..num_entries)
        .map(|_| TableEntryCell::new(None))
        .collect();
    RwLock::new(table)
});
