use std::sync::{LazyLock, RwLock, RwLockReadGuard};

use rayon::{ThreadPool, ThreadPoolBuilder};

pub fn set_num_threads(value: usize) {
    *THREAD_POOL.write().unwrap() = ThreadPoolBuilder::new().num_threads(value).build().unwrap()
}

pub fn get() -> RwLockReadGuard<'static, ThreadPool> {
    THREAD_POOL.read().unwrap()
}

pub const DEFAULT_NUM_THREADS: usize = 3;

static THREAD_POOL: LazyLock<RwLock<ThreadPool>> = LazyLock::new(|| {
    RwLock::new(
        ThreadPoolBuilder::new()
            .num_threads(DEFAULT_NUM_THREADS)
            .build()
            .unwrap(),
    )
});
