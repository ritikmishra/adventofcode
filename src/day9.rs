const XMAS_DATA: &str = include_str!("../inputs/day9.txt");

pub fn find_first_noncompliant_number(preamble_size: i64, input: &str) -> Option<i64> {
    let all_nums: Vec<i64> = input.split("\n").map(|str_num| str_num.parse::<i64>().unwrap()).collect();
    
    let mut last_25: Vec<i64> = all_nums.iter().take(preamble_size as usize).map(|a| *a).collect();
    let next_number_iter = all_nums.iter().skip(preamble_size as usize).map(|a| *a);
    for next_number in next_number_iter {
        // do stuff
        let mut any_pair_summed_to_num = false;
        'outer: for (index, x) in last_25.iter().enumerate() {
            for y in last_25.iter().skip(index) {
                if x + y == next_number {
                    any_pair_summed_to_num = true;
                    break 'outer;
                }
            }
        }
        if !any_pair_summed_to_num {
            return Some(next_number);
        }

        // done doing stuff
        last_25.remove(0);
        last_25.push(next_number);
    }
    None
}

fn find_encryption_weakness(invalid_num: i64, input: &str) -> Option<i64> {
    let all_nums: Vec<i64> = input.split("\n").map(|str_num| str_num.parse::<i64>().unwrap()).collect();
    let mut front_of_group: i64 = 0;
    let mut size_of_group: i64 = 1;
    println!("{:?}", all_nums);
    while (front_of_group + size_of_group) < all_nums.len() as i64 {
        let iterr = || all_nums.iter().skip(front_of_group as usize).take(size_of_group as usize);
        println!("{:?}", iterr().collect::<Vec<&i64>>());
        let sum: i64 = iterr().sum();
        if sum > invalid_num {
            front_of_group += 1;
            size_of_group -= 1;
            continue;
        } else if sum < invalid_num {
            size_of_group += 1;
            continue;
        } else {
            // we have found the correct range
            let min_in_range: i64 = *iterr().min().unwrap();
            let max_in_range: i64 = *iterr().max().unwrap();

            return Some(min_in_range + max_in_range);
        }
    }
    None
}

pub fn day9_main() {
    match find_first_noncompliant_number(25, XMAS_DATA.trim_end()) {
        Some(num) => {
            println!("The number was {}!", num);
            match find_encryption_weakness(num, XMAS_DATA.trim_end()) {
                Some(val) => println!("The weakness is {}", val),
                None => println!("Could not find the weakness :/")
            }
        },
        None => println!("Could not find the number :(")
    }
}