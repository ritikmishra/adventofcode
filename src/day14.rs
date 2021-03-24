use regex::Regex;
use std::collections::HashMap;
use std::str::FromStr;

const PORT_INIT_PROGRAM: &str = include_str!("../inputs/day14.txt");

#[derive(Debug, Clone)]
struct Mask {
    ones_mask: u64,  // 0's to allow existing digit, 1's to force a 1
    zeros_mask: u64, // 1's to allow existing digit, 0's to force a 0
}

impl Mask {
    fn create_mask(mask: &str) -> Option<Self> {
        let mut ones_mask = 0;
        let mut zeros_mask = !0;
        for (i, chr) in mask.chars().enumerate() {
            let exp = mask.len() - 1 - i;
            if chr == 'X' {
                // ones mask is already all 0's, no need to do anything
                // zeros mask is already all 1's, no need to do anything
            } else {
                match chr.to_digit(2) {
                    None => return None,
                    Some(1) => {
                        ones_mask |= 1 << exp; // set the force bit on the ones mask
                    }
                    Some(0) => {
                        // ones mask is already 0 - do nothing
                        zeros_mask ^= 1 << exp; // set the force bit on zeros mask
                    }
                    Some(_) => return None,
                };
            }
        }
        Some(Mask {
            ones_mask,
            zeros_mask,
        })
    }

    fn apply_mask(&self, num: u64) -> u64 {
        let ones_mask_applied = self.ones_mask | num;
        let zeros_mask_applied = self.zeros_mask & ones_mask_applied;
        return zeros_mask_applied;
    }
}

fn run_program_v1(program: &str) -> u64 {
    // lazy_static! {
    //     static ref expr_regex: Regex = Regex::new("(\\S+?) = (\\S+?)").unwrap();
    // }

    let expr_regex: Regex = Regex::new(r"(\S+?) = (\S+)").unwrap();
    let mem_regex: Regex = Regex::new(r"mem\[(\d+)\]").unwrap();

    let mut current_mask: Option<Mask> = None;
    let mut mem_vals: HashMap<u32, u64> = HashMap::new();
    for line in program.split("\n") {
        let banana = expr_regex.captures(line).unwrap();
        let lhs = banana.get(1).unwrap().as_str();
        let rhs = banana.get(2).unwrap().as_str();
        if lhs == "mask" {
            current_mask = Some(Mask::create_mask(rhs).unwrap());
        } else {
            let mem_loc = mem_regex
                .captures(lhs)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<u32>()
                .unwrap();
            mem_vals.insert(
                mem_loc,
                current_mask
                    .clone()
                    .unwrap()
                    .apply_mask(rhs.parse::<u64>().unwrap()),
            );
        }
    }
    return mem_vals.values().sum();
}

fn floating_mask_applier(mask: &str, num: u64) -> Vec<u64> {
    let mut base_num_masked = num;
    // step 1: get all the 0s and 1s out of the way
    for (i, chr) in mask.chars().rev().enumerate() {
        match chr {
            '1' => base_num_masked |= 1 << i,
            _ => (),
        };
    }
    let mut ret = Vec::new();
    ret.push(base_num_masked);

    // step 2: deal with the x's
    for (i, chr) in mask.chars().rev().enumerate() {
        match chr {
            'X' => {
                let mut ret_elements_copy: Vec<u64> = ret.iter().map(|a| *a).collect();
                for num in ret_elements_copy.iter_mut() {
                    *num |= 1 << i;
                }
                for num in ret.iter_mut() {
                    *num &= !0 ^ (1 << i);
                }

                ret.append(&mut ret_elements_copy);
            }
            _ => (),
        };
    }

    return ret;
}

fn run_program_v2(program: &str) -> u64 {
    // lazy_static! {
    //     static ref expr_regex: Regex = Regex::new("(\\S+?) = (\\S+?)").unwrap();
    // }

    let expr_regex: Regex = Regex::new(r"(\S+?) = (\S+)").unwrap();
    let mem_regex: Regex = Regex::new(r"mem\[(\d+)\]").unwrap();

    let mut current_mask: Option<&str> = None;
    let mut mem_vals: HashMap<u64, u64> = HashMap::new();
    for line in program.split("\n") {
        let banana = expr_regex.captures(line).unwrap();
        let lhs = banana.get(1).unwrap().as_str();
        let rhs = banana.get(2).unwrap().as_str();
        if lhs == "mask" {
            current_mask = Some(rhs);
        } else {
            let mem_loc = mem_regex
                .captures(lhs)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<u64>()
                .unwrap();
            
            for possible_val in floating_mask_applier(current_mask.unwrap(), mem_loc) {
                mem_vals.insert(
                    possible_val,
                    rhs.parse::<u64>().unwrap()
                );
            }
        }
    }
    return mem_vals.values().sum();
}


pub fn day14_main() {
    println!(
        "the result of running the program 1  is {}",
        run_program_v1(PORT_INIT_PROGRAM.trim_end())
    );
    println!(
        "the result of running the program 2 is {:?}",
        run_program_v2(PORT_INIT_PROGRAM.trim_end())
    );
}
