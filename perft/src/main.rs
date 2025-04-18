//! Quick and dirty program that computes the perft function (total number of states after
//! a search to a certain depth) to test the correctness of the engine's move generation.

use rayon::prelude::*;

use engine::{GameState, GameStateKey};
use smallvec::SmallVec;

// GameState + depth -> perft
type Cache = Vec<engine::AtomicCell<Option<CacheEntry>>>;

#[derive(Clone, Copy)]
struct CacheEntry {
    key: GameStateKey,
    depth: u32,
    value: u32,
}

fn perft(gs: &GameState, depth: u32, parallel: bool, cache: &Cache) -> u32 {
    if depth == 0 {
        return 1;
    }
    let key = gs.key().hash_with(depth);
    let index = key.hash as usize % cache.len();
    match cache[index].try_load() {
        Some(Some(e)) if e.key == key.key && e.depth == depth => e.value,
        _ => {
            let mut moves: SmallVec<[_; 64]> = SmallVec::new();
            gs.pseudo_legal_moves(|mv| moves.push(mv));
            let value = if parallel {
                moves
                    .into_par_iter()
                    .filter_map(|mv| gs.make_move(*mv).ok())
                    .map(|next_gs| perft(&next_gs, depth - 1, false, cache))
                    .sum()
            } else {
                moves
                    .into_iter()
                    .filter_map(|mv| gs.make_move(mv).ok())
                    .map(|next_gs| perft(&next_gs, depth - 1, false, cache))
                    .sum()
            };
            cache[index].try_store(Some(CacheEntry {
                key: key.key,
                depth,
                value,
            }));
            value
        }
    }
}

fn main() {
    // Source: https://github.com/AndyGrant/Ethereal/blob/master/src/perft/standard.epd
    let mut perft_results = include_str!("perft_results.txt").to_string();
    if std::env::args().any(|arg| arg.contains("960")) {
        perft_results.push_str("\n");
        perft_results.push_str(include_str!("perft_results_chess960.txt"));
    }

    let t0 = std::time::Instant::now();
    let cache = (0..2_usize.pow(22))
        .map(|_| engine::AtomicCell::new(None))
        .collect();
    for depth in 0..=6 {
        for line in perft_results.lines() {
            // Parse the test line
            let mut split = line.split(";").map(str::trim);
            let fen = split.next().unwrap();
            let true_value = split.find_map(|s| {
                let (k, v) = s.split_once(char::is_whitespace).unwrap();
                let k = k.strip_prefix("D").unwrap().parse::<u32>().unwrap();
                (k == depth).then_some(v.parse::<u32>().unwrap())
            });

            // Compute perft and compare
            if let Some(true_value) = true_value {
                println!("{fen:<90} Depth: {depth:<2} Value: {true_value:<10}");
                let gs = fen.parse().unwrap();
                let result = perft(&gs, depth, true, &cache);
                assert_eq!(result, true_value);
            }
        }
    }

    println!("Completed in {:.3} s", t0.elapsed().as_secs_f64());
}
