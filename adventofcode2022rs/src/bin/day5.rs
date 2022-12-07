use itertools::Itertools;
use regex::Regex;

const INPUT: &'static str = include_str!("../../../inputs2022/day5.txt");

const EXAMPLE: &'static str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
";

#[derive(Clone, Copy, Debug)]
struct Instr {
    src: usize,
    dst: usize,
    qty: usize,
}

fn day5(input: &'static str) {
    let (init_stacks, instrs) = input.split("\n\n").collect_tuple().unwrap();
    let stack_row_regex = Regex::new(r"\[(.)\]|    ").unwrap();
    let mut stacks: Vec<Vec<char>> = Vec::new();

    // Parse the stacks
    init_stacks
        .split('\n')
        .rev()
        .skip(1)
        .map(|line| {
            stack_row_regex
                .captures_iter(line)
                .map(|x| x.get(1).and_then(|m| m.as_str().chars().nth(0)))
                .collect::<Vec<_>>()
        })
        .for_each(|stack_letters| {
            if stacks.len() != stack_letters.len() {
                stacks = vec![Vec::new(); stack_letters.len()];
            }

            for (i, letter) in stack_letters.iter().enumerate() {
                letter.map(|letter| stacks[i].push(letter));
            }
        });

    // Parse the instructions
    let instr_regex = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    let instrs = instr_regex
        .captures_iter(instrs)
        .map(|cap| {
            let qty: usize = cap.get(1).unwrap().as_str().parse().unwrap();
            let src: usize = cap.get(2).unwrap().as_str().parse::<usize>().unwrap() - 1;
            let dst: usize = cap.get(3).unwrap().as_str().parse::<usize>().unwrap() - 1;

            Instr { src, dst, qty }
        })
        .collect::<Vec<_>>();

    // Execute the instructions
    let part1 = {
        let mut stacks = stacks.clone();
        for Instr { src, dst, qty } in instrs.iter().copied() {
            for _ in 0..qty {
                let popped = stacks[src].pop();
                match popped {
                    Some(popped) => stacks[dst].push(popped),
                    None => {
                        println!("{stacks:#?}");
                        println!(
                            "total count: {:?}",
                            stacks.iter().map(|x| x.len()).sum::<usize>()
                        );
                        panic!("Attempted to pop {qty} from {src} to {dst}, failed");
                    }
                }
            }
        }
        stacks
            .iter()
            .map(|stack| *stack.last().unwrap())
            .collect::<String>()
    };

    println!("part 1: {part1:?}");

    // Execute the instructions
    let part2 = {
        let mut stacks = stacks.clone();
        for Instr { src, dst, qty } in instrs {
            let src = &mut stacks[src];
            let to_copy = src[src.len() - qty..].to_owned();
            src.drain(src.len() - qty..).for_each(drop);
    
            let dst = &mut stacks[dst];
            dst.extend(to_copy);
        }
        stacks
            .iter()
            .map(|stack| *stack.last().unwrap())
            .collect::<String>()
    };

    println!("part 2: {part2:?}");
}

fn main() {
    day5(INPUT);
}
