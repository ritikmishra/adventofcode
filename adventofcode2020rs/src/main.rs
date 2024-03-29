#[macro_use] extern crate lazy_static;
use std::{collections::HashSet, time::Instant};

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;

fn main() {
    let start_time = Instant::now();

    // day1::day1_main();
    // day2::day2_main();
    // day3::day3_main();
    // day4::day4_main();
    // day5::day5_main();
    // day6::day6_main();
    // day7::day7_main();
    // day8::day8_main();
    // day9::day9_main();
    // day10::day10_main();
    // day11::day11_main();
    // day12::day12_main();
    // day13::day13_main();
    // day14::day14_main();
    // day15::day15_main();
    // day16::day16_main();
    // day17::day17_main();
    // day18::day18_main();
    // day19::day19_main();
    // day20::day20_main();
    // day21::day21_main();
    day22::day22_main();
    // day23::day23_main();
    // day24::day24_main();
    // day25::day25_main();

    // let island = vec![
    //     vec![1, 1],
    //     vec![1, 0]
    // ];

    // Solution::largest_island(island);

    // println!("solution took {} milliseconds to complete", start_time.elapsed().as_millis());
}

// struct Solution;
// impl Solution {
//     /// grid is guaranteed to be a square
//     pub fn largest_island(grid: Vec<Vec<i32>>) -> i32 {
//         let size = grid.len();
//         let mut contours: Vec<HashSet<(usize, usize)>> = Vec::new();
//         // row index
//         'outer: for i in 0..size {
//             // col index
//             for j in 0..size {
//                 // novel point
//                 if let Some(1) = grid.get_pos((i, j)) {
//                     if contours.iter().all(|o| !o.contains(&(i, j))) {
//                         Solution::
//                     }
//                 }
//             }
//         }


//         return 0;
//     }
// }

// trait GetCoord {
//     fn get_pos(&self, loc: (usize, usize)) -> Option<i32>;
// }

// impl GetCoord for Vec<Vec<i32>> {
//     fn get_pos(&self, loc: (usize, usize)) -> Option<i32> {
//         let get = self.get(loc.0);
//         return get.map(|o| o.get(loc.1)).flatten().copied();
//     }
// }

