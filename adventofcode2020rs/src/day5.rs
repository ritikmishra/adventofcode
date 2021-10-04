use regex::Regex;

const BOARDING_PASSES: &str = include_str!("../../inputs/day5.txt");

fn decode_boarding_pass(
    pass: String,
    go_high: char,
    go_low: char,
    lower_bound: i32,
    upper_bound: i32,
) -> Option<i32> {
    if pass.len() > 0 {
        let mut remaining_pass = pass;
        let chr = remaining_pass.remove(0);

        if chr == go_high {
            decode_boarding_pass(
                remaining_pass,
                go_high,
                go_low,
                lower_bound,
                (lower_bound + upper_bound) / 2,
            )
        } else if chr == go_low {
            decode_boarding_pass(
                remaining_pass,
                go_high,
                go_low,
                1 + (lower_bound + upper_bound) / 2,
                upper_bound,
            )
        } else {
            None
        }
    } else {
        if lower_bound == upper_bound {
            Some(lower_bound)
        } else {
            None
        }
    }
}

fn get_seat_id(boarding_pass: &str) -> i32 {
    lazy_static! {
        static ref PASS_REGEX: Regex = Regex::new(r"([FB]{7})([LR]{3})").unwrap();
    }
    let captures = PASS_REGEX.captures(boarding_pass);
    match captures {
        Some(pass_caps) => {
            let vert_pass = String::from(pass_caps.get(1).unwrap().as_str());
            let horiz_pass = String::from(pass_caps.get(2).unwrap().as_str());
            let vert_idx = decode_boarding_pass(vert_pass, 'F', 'B', 0, 127).unwrap();
            let horiz_idx = decode_boarding_pass(horiz_pass, 'L', 'R', 0, 7).unwrap();
            vert_idx * 8 + horiz_idx
        }
        None => -1,
    }
}

pub fn day5_main() {
    let mut pass_ids: Vec<i32> = BOARDING_PASSES
        .trim_end()
        .split_ascii_whitespace()
        .map(|pass| get_seat_id(pass))
        .collect();

    println!("Max boarding pass id: {:?}", pass_ids.iter().max());

    pass_ids.sort();
    let mut pass_ids_iter = pass_ids.iter();
    let mut prev = pass_ids_iter.next().unwrap();
    for curr in pass_ids_iter {
         if (curr - prev) == 2 {
             println!("Gap found between ids {} and {}!", prev, curr);
             println!("Our id is probably {}", prev + 1);
         }
         prev = curr;
    }
}
