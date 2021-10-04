use itertools::Itertools;

/// The handshake used by the card and the door involves an operation that transforms a subject number. To transform a subject number, start with the value 1. Then, a number of times called the loop size, perform the following steps:
///
/// Set the value to itself multiplied by the subject number.
/// Set the value to the remainder after dividing the value by 20201227.
fn generate_public_key(subject_num: u64, loop_size: u64) -> u64 {
    let mut ret = 1;
    for _ in 0..loop_size {
        ret *= subject_num;
        ret %= 20201227;
    }
    ret
}

fn find_loop_size(public_key: u64, initial_subject: u64) -> u64 {
    let mut ret = 1;
    let mut loop_size = 0;
    loop {
        ret *= initial_subject;
        ret %= 20201227;

        loop_size+=1;

        if ret == public_key {
            return loop_size;
        }
    }
}


pub fn day25_main() {
    let input = include_str!("../../inputs/day25.txt");
    let (door_key_str, card_key_str) = input.trim_end().split_ascii_whitespace().collect_tuple().unwrap();
    let door_pub_key = door_key_str.parse::<u64>().unwrap();
    let card_pub_key = card_key_str.parse::<u64>().unwrap();


    let door_loops = find_loop_size(door_pub_key, 7);
    let card_loops = find_loop_size(card_pub_key, 7);
    let enc_key = generate_public_key(door_pub_key, card_loops);
    println!("{}", enc_key);
}