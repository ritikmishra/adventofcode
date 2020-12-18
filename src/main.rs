#[macro_use] extern crate lazy_static;
use std::time::Instant;

// mod day1;
// mod day2;
// mod day3;
// mod day4;
// mod day5;
// mod day6;
// mod day7;
// mod day8;
// mod day9;
// mod day10;
// mod day11;
// mod day12;
// mod day13;
// mod day14;
// mod day15;
// mod day16;
mod day17;


fn main() {
    let start_time = Instant::now();

    // day1::day1_main();
    // day2::day2_main()
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
    day17::day17_main();

    println!("solution took {} milliseconds to complete", start_time.elapsed().as_millis());
}
