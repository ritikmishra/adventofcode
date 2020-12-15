use std::time::Instant;
use std::collections::HashMap;
use std::collections::HashSet;

const GAME_BEGINNING: &str = "8,13,1,0,18,9";

fn add_number(second_to_last: &mut HashMap<i32, usize>, last: &mut HashMap<i32, usize>, num: i32, i: usize) {
    match last.get_mut(&num) {
        None =>  {last.insert(num, i);},
        Some(last_appearance) => {
            second_to_last.insert(num, *last_appearance);
            *last_appearance = i;
        }
    }
}

fn play_game_for_n_iters(initial_nums: &Vec<i32>, iters: i32) -> i32 {
    let mut second_to_last: HashMap<i32, usize>  = HashMap::new();
    let mut last: HashMap<i32, usize>  = HashMap::new();
    
    let mut last_number: i32 = *initial_nums.get(initial_nums.len() - 1).unwrap();
    for (i, num) in initial_nums.iter().enumerate() {
        add_number(&mut second_to_last, &mut last, *num, i);
    }

    for i in initial_nums.len()..iters as usize {
        let next_number: i32;
        match second_to_last.get_mut(&last_number) {
            None =>  {
                next_number = 0;
            },
            Some(second_to_last_appearance) => {
                let last_appearance = last.get(&last_number).unwrap();
                next_number = (*last_appearance - *second_to_last_appearance) as i32;
            }
        }
        add_number(&mut second_to_last, &mut last, next_number, i);
        last_number = next_number;
    }
    return last_number;
}

pub fn day15_main() {
    let now = Instant::now();
    let nums_vec: Vec<i32> = GAME_BEGINNING
        .split(",")
        .map(|x| x.parse::<i32>().unwrap())
        .collect();
    println!("the nth num in the game is {}", play_game_for_n_iters(&nums_vec, 30_000_000));
    println!("took {} seconds", now.elapsed().as_secs());

}
