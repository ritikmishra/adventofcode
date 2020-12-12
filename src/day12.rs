use nalgebra as na;
use std::str::FromStr;

const NAV_INSTRUCTIONS: &str = "F29
E5
L90
W1
R90
E1
R90
W5
F32
E2
S3
R270
F61
R180
R180
E2
F13
R180
F18
L180
F15
N2
S2
R270
N2
F81
W4
N2
E1
N5
F5
N3
R90
W5
S1
F20
L90
S2
R90
F81
S2
E1
F59
N1
L180
W3
R90
E5
F2
R90
F28
R90
F70
L180
S4
L90
F97
W2
R180
S5
F12
N4
L90
F47
R90
F20
W3
S2
E2
F29
S2
L90
S1
L180
F92
L90
S4
W3
L90
W5
F56
N3
L90
S4
F83
S2
F82
W4
F34
R90
N5
F85
R90
F17
R90
F15
W1
L90
F55
S3
F38
W1
F38
S3
W1
N1
F77
R180
S5
F89
L90
F62
N4
R90
E3
L180
E2
F44
S4
R90
W2
L180
S4
R90
N5
F30
S5
R90
F88
R90
F10
L90
N5
F45
E5
F51
W5
N3
E3
F93
E5
F85
N5
L90
F6
W4
N3
F25
L90
E2
S4
W1
R180
E4
S3
E2
F26
N3
E3
F32
N3
L90
F25
L180
E3
F46
F12
N3
W4
L90
S2
L90
F31
W4
F75
L180
E3
F79
R90
N5
L270
W5
L180
S5
F19
N2
F90
L180
W3
S4
E2
F85
N1
F62
W4
F77
L180
W1
N2
L90
F86
R90
W5
F34
E1
F72
R90
E3
F5
N2
R90
S2
R270
W4
F58
F19
R180
R90
W4
F60
N1
E1
R90
F27
W4
L90
F35
E1
F11
E4
R90
W5
F68
L90
S2
W3
F2
L90
F24
W4
N4
R90
F32
L90
L90
F22
F91
R90
W1
F23
W2
S1
W3
S2
F97
W4
L90
W1
F16
S4
L180
W4
F48
E5
F21
N3
F2
E2
F75
N5
E2
L90
E3
F76
L90
N1
L90
N5
E1
L90
E2
R90
F40
L90
S5
E1
F8
S1
L90
E4
N3
F10
L180
F68
R90
F89
R90
W1
L180
E2
F48
S2
L90
F61
N3
L90
L180
W2
N2
F32
R90
E2
F74
W4
N5
F78
N2
F62
S1
R180
S2
E1
L90
N4
F85
R90
W4
R90
E1
R180
W3
S5
E1
R270
N4
F89
N4
R90
N1
E2
N3
F89
N5
E1
F17
R270
F58
E1
L90
F38
W4
S1
E2
R90
E2
W2
L90
W5
L90
E2
F1
E1
F34
S4
F16
W5
L180
F31
L90
E2
F46
L180
N5
F64
R90
F58
L90
F15
L90
S4
E3
F74
F26
S2
W2
S4
F81
S4
L180
E1
F21
F53
E5
F55
L270
W2
S2
E1
S2
E3
N2
F24
L180
N1
E5
F60
S1
F54
N1
F9
N3
L90
E3
L90
E3
L90
N1
F28
N4
R180
N5
F44
N2
E3
R90
W4
R180
F69
W3
S2
R180
E3
R270
S4
W2
R90
F69
E3
F44
L90
F87
E4
F21
E1
S2
R90
E5
L90
W5
R90
E1
F66
W4
F51
S3
S4
W3
L90
E1
R90
W1
R180
W1
N1
L180
N3
L180
E2
F14
N2
E5
F95
L270
N4
R90
N1
R90
N1
E4
F90
N3
F35
N5
F74
R90
N3
F64
N1
F28
N2
W2
F7
W4
N2
F37
E3
F100
R90
F36
L90
F41
E1
L90
L90
W1
N3
W2
W3
L90
W5
F100
R90
N2
L180
F7
W2
E2
F77
L180
N2
R180
F60
L90
N5
L90
F72
L270
W1
L180
N5
R90
F54
S3
F70
N1
F72
E4
N5
R90
S1
W1
N3
R90
F45
E4
F32
W5
F78
W3
R90
W4
F58
R90
E5
L90
F3
R90
N1
R90
W1
N4
E1
W3
N5
R90
W5
S3
F25
W4
N3
F55
W2
R90
N1
F98
L90
W3
L270
E1
N3
N2
R90
S1
F39
L90
W2
L90
N3
E3
F80
W1
N1
W4
F25
R90
E3
L180
F21
S4
F75
L90
F76
W5
N4
E2
R90
F44
E3
N1
W3
F49
N2
L180
S4
L90
F55
S4
R90
F14
R90
W5
L90
F85
N2
L90
L90
E3
R90
N3
E4
S5
F75
L90
F60
N2
R180
E2
L90
E2
L180
F27
E4
N4
L90
N2
L180
F91
N4
F47
W2
N2
F63
F63
W4
F28
L90
N1
F57
S1
E2
L90
E5
L180
N3
E4
F70
R90
W5
L90
W1
E3
F18
E3
F15
N1
E4
L90
W1
F49
E3
R90
F61
N3
E2
F69
E1
F7
L90
S1
F73
R90
S5
E2
L90
N1
S4
R180
F8
S1
E5
F44
N1
F39
L90
S1
F83
L90
E4
F59
N4
W2
S3
W2
R90
L270
E4
R90
N5
E4
F22
E1
S3
R90
W5
R270
N1
S4
F39
N4
F78
N4
W3
F34
E5
F31
L90
N3
R180
F75
S2
E1
R180
F89
E5
R180
S1
W3
F97
R180
F54
W2
F6
W5
N3
F58
R90
N2
L180
F2
W3
E3
N3
R180
W1
E3
S3
E2
F87
W5
E3
R90
F63
F30
R90
S3
F69
E2
F87
N1
F95
S4
L90
F52";

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
    let movement_instrs: Vec<MovementInstr> = NAV_INSTRUCTIONS.split("\n").map(|x| x.parse::<MovementInstr>().unwrap()).collect();
    println!("manhattan dist moved is {}", estimate_manhattan_dist_travelled(&movement_instrs));
    println!("manhattan dist moved by waypoints is {}", estimate_manhattan_dist_travelled_wp(&movement_instrs));
}
