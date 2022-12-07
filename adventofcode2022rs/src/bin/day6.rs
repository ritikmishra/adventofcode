use std::collections::HashSet;

const INPUT: &'static str = include_str!("../../../inputs2022/day6.txt");
const EXAMPLE: &'static str = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";

fn day6(input: &str) {
    let (offset, bytes) = input.as_bytes().windows(4).enumerate().find(|(offset, bytes)| {
        let set: HashSet<u8> = HashSet::from_iter(bytes.iter().copied());
        set.len() == bytes.len()
    }).unwrap();

    let offset = offset + 4;

    println!("Part 1: {offset}");

    let (offset, bytes) = input.as_bytes().windows(14).enumerate().find(|(offset, bytes)| {
        let set: HashSet<u8> = HashSet::from_iter(bytes.iter().copied());
        set.len() == bytes.len()
    }).unwrap();

    let offset = offset + 14;

    println!("Part 2: {offset}");
}

fn main() {
    day6(INPUT);
}