//! Quick and dirty program that computes the perft function (total number of states after
//! a search to a certain depth) to test the correctness of the engine's move generation.

use rayon::prelude::*;

use engine::{TableKey, GameState};
use smallvec::SmallVec;

// GameState + depth -> perft
type Cache = engine::Table<u32, u32>;

fn perft(gs: &GameState, depth: u32, parallel: bool, cache: &Cache) -> u32 {
    if depth == 0 {
        return 1;
    }
    let key = TableKey::new(gs, depth);
    match cache.lookup(&key) {
        Some(value) => value,
        None => {
            let mut moves: SmallVec<[_; 64]> = SmallVec::new();
            gs.pseudo_legal_moves(|mv| moves.push(mv));
            let value = if parallel {
                moves
                    .into_par_iter()
                    .filter_map(|mv| gs.do_move(*mv).ok())
                    .map(|next_gs| perft(&next_gs, depth - 1, false, cache))
                    .sum()
            } else {
                moves
                    .into_iter()
                    .filter_map(|mv| gs.do_move(mv).ok())
                    .map(|next_gs| perft(&next_gs, depth - 1, false, cache))
                    .sum()
            };
            cache.update(key, value);
            value
        }
    }
}

fn main() {
    // Source: https://github.com/AndyGrant/Ethereal/blob/master/src/perft/standard.epd
    let perft_results = include_str!("perft_results.txt");
    let t0 = std::time::Instant::now();

    let cache = Cache::new(2_usize.pow(22));
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
                let gs = game::fen::parse(fen).unwrap();
                let result = perft(&gs, depth, true, &cache);
                assert_eq!(result, true_value);
                assert_eq!(game::fen::unparse(&gs), fen);
            }
        }
    }

    println!("Completed in {:.3} s", t0.elapsed().as_secs_f64());
}
