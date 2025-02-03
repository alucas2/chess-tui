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

// Piece square tables: https://www.chessprogramming.org/Simplified_Evaluation_Function
#[rustfmt::skip]
pub const PAWN_VALUE: [i16; 64] = [
    0,   0,   0,   0,   0,   0,   0,   0,
    105, 110, 110, 80,  80,  110, 110, 105,
    105, 95,  90,  100, 100, 90,  95,  105,
    100, 100, 100, 120, 120, 100, 100, 100,
    105, 105, 110, 125, 125, 110, 105, 105,
    110, 110, 120, 130, 130, 120, 110, 110,
    150, 150, 150, 150, 150, 150, 150, 150,
    0,   0,   0,   0,   0,   0,   0,   0,
];
#[rustfmt::skip]
pub const KNIGHT_VALUE: [i16; 64] = [
    270, 280, 290, 290, 290, 290, 280, 270,
    280, 300, 320, 325, 325, 320, 300, 280,
    290, 300, 330, 335, 335, 330, 300, 290,
    290, 320, 335, 340, 340, 335, 320, 290,
    290, 325, 335, 340, 340, 335, 325, 290,
    290, 320, 330, 335, 335, 330, 320, 290,
    280, 300, 320, 320, 320, 320, 300, 280,
    270, 280, 290, 290, 290, 290, 280, 270,
];
#[rustfmt::skip]
pub const BISHOP_VALUE: [i16; 64] = [
    310, 320, 320, 320, 320, 320, 320, 310,
    320, 335, 330, 330, 330, 330, 335, 320,
    320, 340, 340, 340, 340, 340, 340, 320,
    320, 330, 340, 340, 340, 340, 330, 320,
    320, 335, 335, 340, 340, 335, 335, 320,
    320, 330, 335, 340, 340, 335, 330, 320,
    320, 330, 330, 330, 330, 330, 330, 320,
    310, 320, 320, 320, 320, 320, 320, 310,    
];
#[rustfmt::skip]
pub const ROOK_VALUE: [i16; 64] = [
    500, 500, 500, 505, 505, 500, 500, 500,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    495, 500, 500, 500, 500, 500, 500, 495,
    505, 510, 510, 510, 510, 510, 510, 505,
    500, 500, 500, 500, 500, 500, 500, 500,
];
#[rustfmt::skip]
pub const QUEEN_VALUE: [i16; 64] = [
    880, 890, 890, 885, 885, 890, 890, 880,
    890, 900, 905, 900, 900, 900, 900, 890,
    890, 905, 905, 905, 905, 905, 900, 890,
    900, 900, 905, 905, 905, 905, 900, 885,
    885, 900, 905, 905, 905, 905, 900, 885,
    890, 900, 905, 905, 905, 905, 900, 890,
    890, 900, 900, 900, 900, 900, 900, 890,
    880, 890, 890, 900, 900, 890, 890, 880,
];
#[rustfmt::skip]
pub const KING_VALUE: [i16; 64] = [
    1020, 1030, 1010, 1000, 1000, 1010, 1030, 1020,
    1020, 1020, 1000, 1000, 1000, 1000, 1020, 1020,
    990,  980,  980,  980,  980,  980,  980,  990,
    980,  970,  970,  960,  960,  970,  970,  980,
    970,  960,  960,  950,  950,  960,  960,  970,
    970,  960,  960,  950,  950,  960,  960,  970,
    970,  960,  960,  950,  950,  960,  960,  970,
    970,  960,  960,  950,  950,  960,  960,  970,
];
