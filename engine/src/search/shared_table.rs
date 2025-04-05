use std::sync::{LazyLock, RwLock, RwLockReadGuard};

use crate::Move;

use super::{evaluate::Score, minmax::Depth};

pub type Table = crate::transposition_table::Table<(), TableValue>;

/// Values that are stored in the table.
/// Its size should not be excessive to fit within a table entry
#[derive(Clone, Copy)]
pub struct TableValue {
    pub depth: Depth,
    pub score: Score,
    pub score_kind: ScoreKind,
    pub best: Option<Move>,
}

#[derive(Clone, Copy)]
pub enum ScoreKind {
    Exact,
    AtLeast,
    AtMost,
}

pub fn set_table_size_megabytes(megabytes: usize) {
    let num_entries = megabytes * 1024 * 1024 / Table::entry_size();
    *TABLE.write().unwrap() = Table::new(num_entries);
}

pub fn get() -> RwLockReadGuard<'static, Table> {
    TABLE.read().unwrap()
}

pub const DEFAULT_TABLE_SIZE_MB: usize = 512;

static TABLE: LazyLock<RwLock<Table>> = LazyLock::new(|| {
    let num_entries = DEFAULT_TABLE_SIZE_MB * 1024 * 1024 / Table::entry_size();
    RwLock::new(Table::new(num_entries))
});
