
pub fn part1(input: &str) -> i32 {
    let nums: Vec<i32> = convert_string_to_numbers(input);

    for i in &nums {
        for j in &nums {
            if (*i + *j) == 2020 {
                return *i * *j;
            }
        }
    }
    return -1;
}


pub fn part2(input: &str) -> i32 {
    let nums: Vec<i32> = convert_string_to_numbers(input);

    for i in &nums {
        for j in &nums {
            for k in &nums {
                if (*i + *j + *k) == 2020 {
                    return *i * *j * *k;
                }
            }
        }
    }
    return -1;
}

pub fn day1_main() {
    let x = include_str!("../../inputs/day1.txt");

    println!("part 1: {}", part1(x));
    println!("part 2: {}", part2(x));
       
}

fn convert_string_to_numbers(stringified_nums: &str) -> Vec<i32> {
    let mut nums: Vec<i32> = Vec::new();

    for number_str in stringified_nums.split_whitespace() {
        nums.push(number_str.parse::<i32>().unwrap())
    }
    return nums;
}
