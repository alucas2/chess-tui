use std::{
    cell::UnsafeCell,
    mem::MaybeUninit,
    ptr,
    sync::atomic::{self, AtomicUsize, Ordering},
};

#[repr(align(64))]
pub struct AtomicCell<T> {
    /// Contains 1 if locked by a writer, or else contains an even sequence number
    seq: AtomicUsize,
    data: UnsafeCell<T>,
}

unsafe impl<T: Send> Send for AtomicCell<T> {}
unsafe impl<T: Send> Sync for AtomicCell<T> {}

impl<T: Copy> AtomicCell<T> {
    pub fn new(value: T) -> AtomicCell<T> {
        AtomicCell {
            seq: AtomicUsize::new(0),
            data: UnsafeCell::new(value),
        }
    }

    // https://pitdicker.github.io/Writing-a-seqlock-in-Rust/
    // https://en.cppreference.com/w/cpp/atomic/memory_order
    // https://en.cppreference.com/w/cpp/atomic/atomic_thread_fence
    //
    // MEMO:
    // Operations after Acquire stay after
    // Operations before Release stay before

    pub fn try_load(&self) -> Option<T> {
        let seq_before = self.seq.load(Ordering::Acquire); // Y
        if seq_before == 1 {
            return None;
        }
        // Safety: reading potential garbage into MaybeUninit is tolerated?
        let result = unsafe { ptr::read_volatile(self.data.get().cast::<MaybeUninit<T>>()) };
        atomic::fence(Ordering::Acquire); // FB, affects operations after
        let seq_after = self.seq.load(Ordering::Relaxed);
        if seq_before == seq_after {
            Some(unsafe { result.assume_init() }) // Safety: we checked that the data is unaltered
        } else {
            None
        }
    }

    pub fn try_store(&self, value: T) -> bool {
        let seq_before = self.seq.swap(1, Ordering::Acquire); // Load is acquire, store is relaxed
        if seq_before == 1 {
            return false;
        }
        atomic::fence(Ordering::Release); // FA, affects operations before
        unsafe { ptr::write(self.data.get(), value) } // Safety: pointer is properly aligned
        let seq_after = seq_before.wrapping_add(2);
        self.seq.store(seq_after, Ordering::Release); // X
        true
    }
}
