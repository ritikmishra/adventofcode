use std::collections::LinkedList;
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hasher, Hash};
use crate::Instant;
use std::collections::VecDeque;

const CUPS: &str = "598162734";

fn hash(v: &Vec<i32>) -> u64 {
    let mut hasher = DefaultHasher::new();
    for i in v.iter() {
        (*i).hash(&mut hasher);
    }
    return hasher.finish();
}

fn move_cups(initial_cups: &Vec<i32>, moves: usize) -> Vec<i32> {
    let mut current_cups: Vec<i32> = initial_cups.clone().into_iter().collect();

    let mut last_time: Instant = Instant::now();
    
    for i in 0..moves {
        if i % 1000 == 0 {
            let now = Instant::now();
            println!("Loop iter: {}, Millis to complete: {:?}", i, now.duration_since(last_time).as_millis());
            last_time = now;
        }
        let max_cup_val = initial_cups.len() as i32;

        let current_cup: i32 = *current_cups.get(0).unwrap();
        let next_3_cups: [i32; 3] = [*current_cups.get(1).unwrap(), *current_cups.get(2).unwrap(), *current_cups.get(3).unwrap()]; // removes the next 3 cups

        
        let destination_cup: i32 = {
            let mut candidate = current_cup - 1;
            loop {
                    if candidate < 1 {
                        candidate += max_cup_val;
                    }
                    if next_3_cups.iter().all(|cup3| *cup3 != candidate) {
                        break candidate;
                    }
                    candidate -= 1;
                }
            };


        
        for (i, val) in current_cups.clone().into_iter().enumerate() {
            if val == destination_cup {
                current_cups.insert(i+1, next_3_cups[2]);
                current_cups.insert(i+1, next_3_cups[1]);
                current_cups.insert(i+1, next_3_cups[0]);
                break;
            }
        }

        // let insertion_index: usize = current_cups.clone().into_iter().enumerate().filter(|(_i, cupval)| *cupval == destination_cup).next().unwrap().0 + 1;
        
        // for cup in next_3_cups.iter().rev()  {
        //     current_cups.insert(insertion_index, *cup);
        // }

        current_cups = current_cups.split_off(4);
        current_cups.push(current_cup);
    }
    return current_cups.into_iter().collect();
}

fn print_p1_ans(cups: &Vec<i32>) {
    let index_of_cup_1: usize = cups.clone().into_iter().enumerate().filter(|(_i, cupval)| *cupval == 1).next().unwrap().0;

    println!("index of cup 1: {}", index_of_cup_1);
    let mut ret = String::new();
    for mut i in (index_of_cup_1+1)..(index_of_cup_1+cups.len()) {
        i %= cups.len();
        ret.push_str(format!("{}", cups.get(i).unwrap()).as_str());
    }
    println!("{}", ret);

}

fn print_p2_ans(cups: &Vec<i32>) {
    let index_of_cup_1: usize = cups.clone().into_iter().enumerate().filter(|(_i, cupval)| *cupval == 1).next().unwrap().0;

    println!("index of cup 1: {}", index_of_cup_1);
    let cup_after_1 = (index_of_cup_1+1) % cups.len();
    let second_cup_after = (index_of_cup_1+2) % cups.len();
    println!("{}", (cup_after_1 as u64) * (second_cup_after as u64));

}

pub fn day23_main() {
    let cups: Vec<i32> = CUPS.chars().map(|chr| chr.to_digit(10).unwrap() as i32).collect();

    let new_cups = move_cups(&cups, 100);

    println!("new cups: {:?}", new_cups);
    print_p1_ans(&new_cups);

    let mut part_2_cups: Vec<i32> = Vec::new();
    part_2_cups.extend(cups.clone().iter());
    
    let max_cup_val = part_2_cups.iter().cloned().max().unwrap();
    for new_cup in max_cup_val..1000001 {
        part_2_cups.push(new_cup);
    }

    let new_cups = move_cups(&part_2_cups, 10000000);
    println!("i moved the cups!");
    print_p2_ans(&new_cups);
}