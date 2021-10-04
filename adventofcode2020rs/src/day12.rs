use nalgebra as na;
use std::str::FromStr;

const NAV_INSTRUCTIONS: &str = include_str!("../../inputs/day12.txt");

#[derive(Debug)]
enum MovementType {
    North,
    South,
    East,
    West,
    Forwards,
    Left,
    Right,
}

#[derive(Debug)]
struct MovementInstr {
    dir: MovementType,
    val: i32,
}

impl FromStr for MovementInstr {
    type Err = i32;

    /// Convert a string N123 into a Movement instruction struct
    fn from_str(val: &str) -> std::result::Result<Self, Self::Err> {
        let mut val_chars = val.chars();
        if let Some(dir_character) = val_chars.next() {
            if let Ok(value) = val_chars.as_str().parse::<i32>() {
                let dir_type = match dir_character {
                    'N' => MovementType::North,
                    'E' => MovementType::East,
                    'S' => MovementType::South,
                    'W' => MovementType::West,
                    'F' => MovementType::Forwards,
                    'L' => MovementType::Left,
                    'R' => MovementType::Right,
                    _ => return Err(2),
                };
                return Ok(MovementInstr {
                    dir: dir_type,
                    val: value,
                });
            }
        }
        Err(1)
    }
}

fn calculate_next_pos(old_pos: &na::Vector3<f64>, instr: &MovementInstr) -> na::Vector3<f64> {
    match instr.dir {
        MovementType::North => {
            return old_pos + na::Vector3::new(0.0, instr.val as f64, 0.0);
        }
        MovementType::East => {
            return old_pos + na::Vector3::new(instr.val as f64, 0.0, 0.0);
        }
        MovementType::South => {
            return old_pos - na::Vector3::new(0.0, instr.val as f64, 0.0);
        }
        MovementType::West => {
            return old_pos - na::Vector3::new(instr.val as f64, 0.0, 0.0);
        }
        MovementType::Forwards => {
            let angle = old_pos.z;
            let ship_dir_ver = na::Unit::from_angle((angle as f64) / 180.0 * 3.14159265);

            let delta = na::Vector3::new(instr.val as f64, 0.0, 0.0);
            return old_pos + (ship_dir_ver.to_homogeneous() * delta);
        }
        MovementType::Left => {
            return old_pos + na::Vector3::new(0.0, 0.0, instr.val as f64);
        },
        MovementType::Right => {
            return old_pos - na::Vector3::new(0.0, 0.0, instr.val as f64);
        },
    }
}

fn estimate_manhattan_dist_travelled(movement_instrs: &Vec<MovementInstr>) -> f64 {
    let mut pos = na::Vector3::new(0.0, 0.0, 0.0);
    for instr in movement_instrs.iter() {
        pos = calculate_next_pos(&pos, instr);
    }
    pos.x.abs() + pos.y.abs()
}

/// ** waypoint pos is relative to the ship+++ **
fn calculate_next_pos_waypoint_based(old_pos: &na::Vector2<f64>, waypoint_pos: &na::Vector2<f64>, instr: &MovementInstr) -> (na::Vector2<f64>, na::Vector2<f64>) {
    match instr.dir {
        MovementType::North => {
            return (*old_pos, waypoint_pos + na::Vector2::new(0.0, instr.val as f64));
        }
        MovementType::East => {
            return (*old_pos, waypoint_pos + na::Vector2::new(instr.val as f64, 0.0));
        }
        MovementType::South => {
            return (*old_pos, waypoint_pos - na::Vector2::new(0.0, instr.val as f64));
        }
        MovementType::West => {
            return (*old_pos, waypoint_pos - na::Vector2::new(instr.val as f64, 0.0));
        }
        MovementType::Forwards => {
            return (old_pos + (instr.val as f64 * waypoint_pos), *waypoint_pos);
        }
        MovementType::Left => {
            let rot = na::Rotation2::new((instr.val as f64) / 180.0 * 3.14159265);

            return (*old_pos, rot * waypoint_pos);
        },
        MovementType::Right => {
            let rot = na::Rotation2::new(-(instr.val as f64) / 180.0 * 3.14159265);

            return (*old_pos, rot * waypoint_pos);
        },
    }
}


fn estimate_manhattan_dist_travelled_wp(movement_instrs: &Vec<MovementInstr>) -> f64 {
    let mut pos = na::Vector2::new(0.0, 0.0);
    let mut wp = na::Vector2::new(10.0, 1.0);
    for instr in movement_instrs.iter() {
        let tmp = calculate_next_pos_waypoint_based(&pos, &wp, instr);
        pos = tmp.0;
        wp = tmp.1;
    }
    pos.x.abs() + pos.y.abs()
}


pub fn day12_main() {
    let apple = "F10
N3
F7
R90
F11";
    let movement_instrs: Vec<MovementInstr> = NAV_INSTRUCTIONS.trim_end().split("\n").map(|x| x.parse::<MovementInstr>().unwrap()).collect();
    println!("manhattan dist moved is {}", estimate_manhattan_dist_travelled(&movement_instrs));
    println!("manhattan dist moved by waypoints is {}", estimate_manhattan_dist_travelled_wp(&movement_instrs));
}
