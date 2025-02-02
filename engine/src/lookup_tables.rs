#[derive(Debug, Clone, Copy)]
pub struct Rays {
    pub n: u64,
    pub s: u64,
    pub e: u64,
    pub w: u64,
    pub ne: u64,
    pub nw: u64,
    pub se: u64,
    pub sw: u64,
}

pub const fn shift_n(bb: u64) -> u64 {
    bb << 8
}

pub const fn shift_s(bb: u64) -> u64 {
    bb >> 8
}

pub const fn shift_e(bb: u64) -> u64 {
    (bb & 0x7f7f7f7f7f7f7f7f) << 1
}

pub const fn shift_w(bb: u64) -> u64 {
    (bb & 0xfefefefefefefefe) >> 1
}

pub const fn shift_ne(bb: u64) -> u64 {
    (bb & 0x7f7f7f7f7f7f7f7f) << 9
}

pub const fn shift_nw(bb: u64) -> u64 {
    (bb & 0xfefefefefefefefe) << 7
}

pub const fn shift_se(bb: u64) -> u64 {
    (bb & 0x7f7f7f7f7f7f7f7f) >> 7
}

pub const fn shift_sw(bb: u64) -> u64 {
    (bb & 0xfefefefefefefefe) >> 9
}

/// Function that run at compile-time
mod compile_time {
    use super::*;

    macro_rules! ray_fn {
        ($ray_fn: ident, $shift_fn: ident) => {
            const fn $ray_fn(start: usize) -> u64 {
                let mut bb = 1 << start;
                let mut acc = 0;
                while bb != 0 {
                    bb = $shift_fn(bb);
                    acc |= bb;
                }
                acc
            }
        };
    }

    // Compile-time functions that generate a ray in the given direction.
    ray_fn!(ray_n, shift_n);
    ray_fn!(ray_s, shift_s);
    ray_fn!(ray_e, shift_e);
    ray_fn!(ray_w, shift_w);
    ray_fn!(ray_ne, shift_ne);
    ray_fn!(ray_nw, shift_nw);
    ray_fn!(ray_se, shift_se);
    ray_fn!(ray_sw, shift_sw);

    /// Compile-time function that generates the table of knight moves from each square
    pub const fn generate_knight_lut() -> [u64; 64] {
        let mut lut = [0; 64];
        let mut i = 0;
        while i != 64 {
            let bb = 1 << i;
            lut[i] = shift_ne(shift_n(bb))
                | shift_nw(shift_n(bb))
                | shift_se(shift_s(bb))
                | shift_sw(shift_s(bb))
                | shift_ne(shift_e(bb))
                | shift_se(shift_e(bb))
                | shift_nw(shift_w(bb))
                | shift_sw(shift_w(bb));
            i += 1;
        }
        lut
    }

    /// Compile-time function that generates the table of king moves from each square
    pub const fn generate_king_lut() -> [u64; 64] {
        let mut lut = [0; 64];
        let mut i = 0;
        while i != 64 {
            let bb = 1 << i;
            lut[i] = shift_n(bb)
                | shift_s(bb)
                | shift_e(bb)
                | shift_w(bb)
                | shift_ne(bb)
                | shift_nw(bb)
                | shift_se(bb)
                | shift_sw(bb);
            i += 1;
        }
        lut
    }

    /// Compile-time function that generates the table of rays for each square and direction
    pub const fn generate_rays_lut() -> [Rays; 64] {
        let zero = unsafe { std::mem::zeroed() }; // Safety: zero is a valid bit pattern for Rays
        let mut lut = [zero; 64];
        let mut i = 0;
        while i != 64 {
            lut[i] = Rays {
                n: ray_n(i),
                s: ray_s(i),
                e: ray_e(i),
                w: ray_w(i),
                ne: ray_ne(i),
                nw: ray_nw(i),
                se: ray_se(i),
                sw: ray_sw(i),
            };
            i += 1;
        }
        lut
    }
}

/// Knight moves from each square (512 bytes)
pub const KNIGHT_REACHABLE: [u64; 64] = compile_time::generate_knight_lut();

/// King moves from each square (512 bytes)
pub const KING_REACHABLE: [u64; 64] = compile_time::generate_king_lut();

/// Rays from each square in each direction (4096 bytes)
pub const RAYS: [Rays; 64] = compile_time::generate_rays_lut();
