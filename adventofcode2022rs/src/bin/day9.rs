use std::collections::HashSet;

const INPUT: &'static str = include_str!("../../../inputs2022/day9.txt");

const EXAMPLE: &'static str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

#[derive(Debug, Copy, Clone)]
enum Dir {
    Up,
    Down,
    Left,
    Right
}

impl Dir {
    fn delta(self) -> (i32, i32) {
        match self {
            Dir::Up => (0, 1),
            Dir::Down => (0, -1),
            Dir::Left => (-1, 0),
            Dir::Right => (1, 0),
        }
    }
}

fn add_delta((a, b): (i32, i32), (c, d): (i32, i32)) -> (i32, i32) {
    (a + c, b + d)
}


fn calc_delta((a, b): (i32, i32), (c, d): (i32, i32)) -> (i32, i32) {
    (a - c, b - d)
}

fn inf_norm((a, b): (i32, i32)) -> i32 {
    a.abs().max(b.abs())
}

fn mag_1(x: i32) -> i32 {
    if x == 0 {
        0
    } else {
        x.signum()
    }
}

fn day9(input: &str) {
    // head is 1
    let mut rope_segments: Vec<(i32, i32)> = vec![(0, 0); 10];

    let mut tail_pos_positions: HashSet<_> = HashSet::new();

    let instructions = input.split('\n').map(|line| {
            let (dir, count) = line.split_at(2);
            let count = count.parse::<i32>().unwrap();
            let dir = match dir {
                "U " => Dir::Up,
                "D " => Dir::Down,
                "L " => Dir::Left,
                "R " => Dir::Right,
                _ => panic!("couldn't parse direction")
            };
    
            (dir, count)
        }).collect::<Vec<_>>();
    
    let len = rope_segments.len();
    for (dir, count) in instructions {
        for _ in 0..count {
            let head = rope_segments[0];
            rope_segments[0] = add_delta(head, dir.delta());
            println!("old head: {head:?}, new head: {:?}", rope_segments[0]);
            for (head_idx, tail_idx) in (0..(len - 1)).into_iter().zip(1..len)  {
                let head_pos = rope_segments[head_idx];
                let tail_pos = &mut rope_segments[tail_idx];
                let tail_head_delta = calc_delta(head_pos, *tail_pos);
                *tail_pos = if inf_norm(tail_head_delta) > 1 {
                    let tail_delta = (
                        mag_1(tail_head_delta.0),
                        mag_1(tail_head_delta.1)
                    );   
    
                    add_delta(*tail_pos, tail_delta)
                } else {
                    *tail_pos
                };
    
                if tail_idx == len - 1 {
                    tail_pos_positions.insert(*tail_pos);
                }
            }
        }
    }

    println!("Part 1: {:?}", tail_pos_positions.len());
}

fn main() {
    day9(INPUT);
}