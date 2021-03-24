use crate::Instant;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};

fn hash(v: &Vec<i32>) -> u64 {
    let mut hasher = DefaultHasher::new();
    for i in v.iter() {
        (*i).hash(&mut hasher);
    }
    return hasher.finish();
}


// represents cup ordering
#[derive(Debug, Copy, Clone)]
struct LinkedCup {
    prev_cup: i32,
    next_cup: i32,
}

fn delink_cups(current_cups: &HashMap<i32, LinkedCup>) -> Vec<i32> {
    println!("delinking cups");
    let mut ret: Vec<i32> = Vec::new();
    let mut cup = 1;
    loop {
        let current_link = current_cups.get(&cup).unwrap();
        ret.push(cup);
        cup = current_link.next_cup;
        if cup == 1 {
            break;
        }
    };

    return ret;
}

fn move_cups(initial_cups: &Vec<i32>, moves: usize) ->  HashMap<i32, LinkedCup> {
    // let mut current_cups: Vec<i32> = initial_cups.clone().into_iter().collect();

    let mut current_cups: HashMap<i32, LinkedCup> = HashMap::new();
    current_cups.insert(
        initial_cups[0],
        LinkedCup {
            prev_cup: initial_cups[initial_cups.len() - 1],
            next_cup: initial_cups[1],
        },
    );

    for cups in initial_cups.windows(3) {
        current_cups.insert(
            cups[1],
            LinkedCup {
                prev_cup: cups[0],
                next_cup: cups[2],
            },
        );
    }

    current_cups.insert(
        initial_cups[initial_cups.len() - 1],
        LinkedCup {
            prev_cup: initial_cups[initial_cups.len() - 2],
            next_cup: initial_cups[0],
        },
    );

    let max_cup_val = initial_cups.len() as i32;
    let mut last_time: Instant = Instant::now();
    let mut current_cup: i32 = initial_cups[0];
    for i in 0..moves {
        // println!("i {} | selected {} | {:?}",i, current_cup, delink_cups(&current_cups));
        if i % 1000000 == 0 {
            let now = Instant::now();
            println!(
                "Loop iter: {}, Millis to complete: {:?}",
                i,
                now.duration_since(last_time).as_millis()
            );
            last_time = now;
        }

        let current_cup_link: &LinkedCup = current_cups.get(&current_cup).unwrap();
        let next_1st_cup: i32 = current_cup_link.next_cup;
        let next_2nd_cup: i32 = current_cups.get(&next_1st_cup).unwrap().next_cup;
        let next_3nd_cup: i32 = current_cups.get(&next_2nd_cup).unwrap().next_cup;
        let next_3rd_cup_link: LinkedCup = current_cups.get(&next_3nd_cup).unwrap().clone();

        let destination_cup: i32 = {
            let mut candidate = current_cup - 1;
            loop {
                if candidate < 1 {
                    candidate += max_cup_val;
                }
                if next_1st_cup != candidate
                    && next_2nd_cup != candidate
                    && next_3nd_cup != candidate
                {
                    break candidate;
                }
                candidate -= 1;
            }
        };

        // current cup -> next current cup (skip the 3 we are moving)
        let current_cup_link_mut = current_cups.get_mut(&current_cup).unwrap();
        current_cup_link_mut.next_cup = next_3rd_cup_link.next_cup;
        current_cup = current_cup_link_mut.next_cup;
        
        // destination_cup -> 1 -> 2 -> 3 -> destination_cup_next 
        // link dest to moved cup 1
        let destination_cup_link_mut = current_cups.get_mut(&destination_cup).unwrap();
        let destination_cup_next = destination_cup_link_mut.next_cup;
        destination_cup_link_mut.next_cup = next_1st_cup;

        // link moved cup 3 to next dest
        let next_3rd_cup_link_mut = current_cups.get_mut(&next_3nd_cup).unwrap();
        next_3rd_cup_link_mut.next_cup = destination_cup_next;
    }
    
    return current_cups;
}

pub fn day23_main() {
    let cups: Vec<i32> = include_str!("../inputs/day23.txt")
        .trim_end()
        .chars()
        .map(|chr| chr.to_digit(10).unwrap() as i32)
        .collect();

    let new_cups = delink_cups(&move_cups(&cups, 100));

    println!("new cups: {:?}", new_cups);
    println!("part 1: {}", new_cups.iter().cloned().skip(1).map(|num| format!("{}", num).chars().next().unwrap()).collect::<String>());

    let mut part_2_cups: Vec<i32> = cups.clone();
    let max_cup_val = part_2_cups.len();
    for new_cup in (max_cup_val+1)..=1_000_000 {
        part_2_cups.push(new_cup as i32);
    }

    let new_cups = move_cups(&part_2_cups, 10_000_000);
    println!("i moved the cups!");
    let cup_1: &LinkedCup = new_cups.get(&1).unwrap();
    let cup_a = cup_1.next_cup;
    let cup_b = new_cups.get(&cup_a).unwrap().next_cup;

    println!("cup a: {}", cup_a);
    println!("cup b: {}", cup_b);
    println!("product: {}", (cup_a as u64) * (cup_b as u64))
    // println!("part 2: {:?}", new_cups.iter().cloned().take(5).collect::<Vec<i32>>());
}
