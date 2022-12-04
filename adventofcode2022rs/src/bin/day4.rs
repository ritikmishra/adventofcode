const INPUT: &'static str = include_str!("../../../inputs2022/day4.txt");


use regex::Regex;

#[derive(Debug, Clone, Copy)]
struct Range(i32, i32);

impl Range {
    fn we_contain_other(self, other: Self) -> bool {
        let range_inclusive = self.0..=self.1;
        range_inclusive.contains(&other.0) && range_inclusive.contains(&other.1)
    }

    fn overlap(self, other: Self) -> bool {
        let self_range = self.0..=self.1;
        let other_range = other.0..=other.1;
        self_range.contains(&other.0) || self_range.contains(&other.1) || other_range.contains(&self.0) || other_range.contains(&self.1)        
    }
}

fn day4(input: &str) {
    let each_line = Regex::new(r"(\d+)-(\d+),(\d+)-(\d+)").unwrap();
    let ranges: Vec<(Range, Range)> = each_line.captures_iter(input).map(|line| {
        let first_start = &line[1];
        let first_end = &line[2];
        let second_start = &line[3];
        let second_end = &line[4];


        (Range(first_start.parse().unwrap(), first_end.parse().unwrap()), Range(second_start.parse().unwrap(), second_end.parse().unwrap()))
    }).collect();


    let count = ranges.iter().filter(|(left, right)| left.we_contain_other(*right) || right.we_contain_other(*left)).count();
    println!("Part 1: {count:#?}");

    let count = ranges.iter().filter(|(left, right)| left.overlap(*right)).count();
    println!("Edge cases: {:#?}", {
        ranges.iter().filter(|(left, right)| right.overlap(*left) != left.overlap(*right)).collect::<Vec<_>>()
    });
    println!("Part 2: {count:#?}");
}

fn main() {
    day4(INPUT);
}