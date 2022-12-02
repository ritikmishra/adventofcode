const INPUT: &'static str = include_str!("../../../inputs2022/day1.txt");

fn day1(input: &str) {
    let mut numbers = input.split("\n\n").map(|elf| {
        elf.split("\n")
            .filter_map(|string| match string.parse::<i32>() {
                Ok(n) => Some(n),
                Err(..) => None,
            })
            .sum::<i32>()
    }).collect::<Vec<_>>();

    numbers.sort_by_key(|x| -x);
    let max_sum = numbers[0];
    println!("Max sum: {max_sum:?}");

    let top_3 = numbers[0] + numbers[1] + numbers[2];
    println!("Top 3: {top_3:?}");
}

fn main() {
    day1(INPUT);
}