use std::collections::HashSet;
use itertools::Itertools;

const INPUT: &'static str = include_str!("../../../inputs2022/day3.txt");

fn char_to_priority(char: u8) -> i32 {
    i32::from(if b'a' <= char && char <= b'z' {
        char - b'a' + 1
    } else if b'A' <= char && char <= b'Z' {
        char - b'A' + 27
    } else {
        unreachable!("char to priority was passed an invalid character: {char}");
    })
}


fn day3(input: &str) {
    let sacks = input.split_ascii_whitespace().map(|line| {
        let bytes = line.as_bytes();
        let len = bytes.len();
        assert!(len % 2 == 0);
        (&bytes[..len / 2], &bytes[len/2..])
    }).collect::<Vec<(&[u8], &[u8])>>();

    let sum = sacks.iter().copied().map(|(left, right)| {
        let left: HashSet<&u8> = HashSet::from_iter(left.iter());
        let right: HashSet<&u8> = HashSet::from_iter(right.iter());
        let matching = **left.intersection(&right).next().expect("no matching items");
        char_to_priority(matching)
    }).sum::<i32>();

    println!("Part 1: {sum}");

    let chunks = input.split_ascii_whitespace().chunks(3);
    let sacks2 = chunks.into_iter().map(|mut chunk| {
        let row1: HashSet<u8> = HashSet::from_iter(chunk.next().unwrap().as_bytes().iter().copied());
        let row2: HashSet<u8> = HashSet::from_iter(chunk.next().unwrap().as_bytes().iter().copied());
        let row3: HashSet<u8> = HashSet::from_iter(chunk.next().unwrap().as_bytes().iter().copied());

        let mut shared_char = row1.iter().copied().filter(|item| row2.contains(item)).filter(|item| row3.contains(item));
        char_to_priority(shared_char.next().unwrap())
    }).sum::<i32>();

    println!("Part 2: {sacks2}");
}

fn main() {
    day3(INPUT);
}