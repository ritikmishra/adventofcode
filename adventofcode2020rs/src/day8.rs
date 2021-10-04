use std::collections::HashMap;
const INSTRUCTIONS: &str = include_str!("../../inputs/day8.txt");

fn run_program(instrs: &str) -> Result<i32, (i32, &str)> {
    let lines: Vec<&str> = instrs.split("\n").collect();

    // Mapping from line number (index in lines) to number of times the instruction was called
    let mut count: HashMap<i32, i32> = HashMap::new();

    let mut index: i32 = 0;
    let mut acc: i32 = 0;
    while index < (lines.len() as i32) {
        let instr_parts: Vec<&str> = lines.get(index as usize).unwrap().split(" ").collect();
        let instr_name = instr_parts.get(0).unwrap();
        let instr_num = instr_parts.get(1).unwrap().parse::<i32>().unwrap();
        
        let counting_current_line_executions = count.get_mut(&index);
        match counting_current_line_executions {
            Some(num) => {
                *num += 1;
                if *num > 2 {
                    return Err((index, lines.get(index as usize).unwrap()));
                }
            },
            None => {count.insert(index, 1);}
        };

        // Execute the instruction
        match instr_name.as_ref() {
            "nop" => {
                index += 1;
            }, 
            "acc" => {
                acc += instr_num;
                index += 1;
            }, 
            "jmp" => {
                index += instr_num;
            },
            _ => ()
        };
    }
    return Ok(acc);
}

fn brute_force_instr(instrs: &str) -> Option<i32> {
    let lines: Vec<&str> = instrs.split("\n").collect();
    for (i, line) in lines.iter().enumerate() {
        let mut line_parts: Vec<String> = line.split(" ").map(|s| String::from(s)).collect();
        let mut instr_type: &mut String = line_parts.get_mut(0).unwrap();
        if instr_type == "jmp" {
            *instr_type = String::from("nop");
        } else if instr_type == "nop" {
            *instr_type = String::from("nop");
        } else {
            continue;
        }
        let replacement_instr = line_parts.join(" ");
        let mut lines_copy = lines.to_vec();
        let old_instr = lines_copy.get_mut(i).unwrap();
        *old_instr = replacement_instr.as_str();

        // print!("line {} changed to {:?} | ", i, replacement_instr);
        match run_program(lines_copy.join("\n").as_str()) {
            Ok(acc) => {
                // println!("acc was {}", acc);
                return Some(acc);
            },
            Err(line) => ()
        }
    }
    return None;
}

pub fn day8_main() {
    match run_program(INSTRUCTIONS.trim_end()) {
        Ok(acc) => println!("acc was {}", acc),
        Err(line) => println!("line number {:?} was executed over 40 times in the unedited program", line)
    }
    match brute_force_instr(INSTRUCTIONS.trim_end()) {
        Some(terminal_acc) => println!("when there is no inf loop, the program ends with acc {}", terminal_acc),
        None => println!("we were not able to brute force which instruction was the correct one")
    }
}