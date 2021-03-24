use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
const SEAT_LAYOUT: &str = include_str!("../inputs/day11.txt");

#[derive(Debug, Copy, Clone)]
enum SeatState {
    FLOOR,
    EMPTY,
    OCCUPIED,
}

type SeatInfo = HashMap<(i32, i32), SeatState>;

fn parse_str_into_seat_info(seats_str: &str) -> Result<SeatInfo, (i32, i32)> {
    let mut ret = HashMap::new();
    for (row, seat_row) in seats_str.split("\n").enumerate() {
        for (col, seat) in seat_row.chars().enumerate() {
            match seat {
                '.' => ret.insert((row as i32, col as i32), SeatState::FLOOR),
                'L' => ret.insert((row as i32, col as i32), SeatState::EMPTY),
                '#' => ret.insert((row as i32, col as i32), SeatState::OCCUPIED),
                _ => return Err((row as i32, col as i32)),
            };
        }
    }
    return Ok(ret);
}

fn count_adjacent_occupied_seats(seat_info: &SeatInfo, location: (i32, i32)) -> i32 {
    let mut adjacent_occupied_seats = 0;
    for row_delta in -1..2 {
        for col_delta in -1..2 {
            if row_delta == 0 && col_delta == 0 {
                continue;
            }

            let new_location = (location.0 + row_delta, location.1 + col_delta);
            if let Some(adj_seat_status) = seat_info.get(&new_location) {
                if let SeatState::OCCUPIED = *adj_seat_status {
                    adjacent_occupied_seats += 1;
                }
            }
        }
    }
    return adjacent_occupied_seats;
}

fn print_seatinfo(seat_info: &SeatInfo) {
    let mut entries: Vec<((i32, i32), SeatState)> =
        seat_info.iter().map(|v| (*v.0, *v.1)).collect();
    entries.sort_by(|a, b| {
        let a_key = a.0;
        let b_key = b.0;
        let row_cmp = a_key.0.cmp(&b_key.0);
        match row_cmp {
            Ordering::Less => row_cmp,
            Ordering::Greater => row_cmp,
            Ordering::Equal => a_key.1.cmp(&b_key.1),
        }
    });

    let mut last_row = 0;
    for (pos, state) in entries {
        if pos.0 > last_row {
            print!("\n");
        }
        match state {
            SeatState::FLOOR => print!("."),
            SeatState::EMPTY => print!("L"),
            SeatState::OCCUPIED => print!("#"),
        };

        last_row = pos.0;
    }
    println!("\n");
}

fn los_seat_count(seat_info: &SeatInfo, location: (i32, i32)) -> i32 {
    let mut directions: HashSet<(i32, i32)> = HashSet::new();
    for row in (-1..2) {
        for col in (-1..2) {
            if row != 0 || col != 0 {
                directions.insert((row, col));
            }
        }
    }

    let mut seat_count = 0;
    let mut radius = 1;
    while directions.len() > 0 {
        let mut to_remove: Vec<(i32, i32)> = Vec::new();
        for (row_dir, col_dir) in directions.iter() {
            let new_location = (location.0 + row_dir * radius, location.1 + col_dir * radius);
            if let Some(adj_seat_status) = seat_info.get(&new_location) {
                match *adj_seat_status {
                    SeatState::OCCUPIED => {
                        seat_count += 1;
                        to_remove.push((*row_dir, *col_dir));
                    }
                    SeatState::EMPTY => {
                        to_remove.push((*row_dir, *col_dir));
                    }
                    SeatState::FLOOR => {}
                };
            } else {
                to_remove.push((*row_dir, *col_dir));
            }
        }
        for val in to_remove.iter() {
            directions.remove(&val);
        }
        to_remove.clear();
        radius += 1;
    }
    return seat_count;
}

fn single_iteration_on_behavior(seat_info: &mut SeatInfo) -> bool {
    let mut changes: SeatInfo = HashMap::new();
    for (seat_pos, seat_status) in seat_info.iter() {
        let adj_occupied_seats = los_seat_count(seat_info, *seat_pos);
        match seat_status {
            SeatState::EMPTY => {
                if adj_occupied_seats == 0 {
                    changes.insert(*seat_pos, SeatState::OCCUPIED);
                }
            }
            SeatState::FLOOR => {}
            SeatState::OCCUPIED => {
                if adj_occupied_seats >= 5 {
                    changes.insert(*seat_pos, SeatState::EMPTY);
                }
            }
        }
    }

    for (change_loc, new_val) in changes.iter() {
        seat_info.insert(*change_loc, *new_val);
    }

    print_seatinfo(seat_info);
    return changes.len() > 0;
}

fn iterate_until_steady_state(seat_info: &mut SeatInfo) -> i32 {
    let mut seats_mutated = true;
    while seats_mutated {
        seats_mutated = single_iteration_on_behavior(seat_info);
    }
    return seat_info.values().fold(0, |acc, val| {
        if let SeatState::OCCUPIED = val {
            return acc + 1;
        }
        return acc;
    });
}

pub fn day11_main() {
    match parse_str_into_seat_info(SEAT_LAYOUT.trim_end(),
    ) {
        Err(problem_location) => println!(
            "Unknown character located at row, col = {:?}",
            problem_location
        ),
        Ok(mut seat_info) => {
            println!(
                "at steady state, there are {} occupied seats",
                iterate_until_steady_state(&mut seat_info)
            );
        }
    }

    match parse_str_into_seat_info(
        ".............
.L.L.#.#.#.#.
.............",
    ) {
        Err(problem_location) => println!(
            "Unknown character located at row, col = {:?}",
            problem_location
        ),
        Ok(mut seat_info) => {
            print_seatinfo(&seat_info);
            println!("{:?}", seat_info.get(&(1, 3)));
            println!(
                "at steady state, there are {} occupied seats",
                los_seat_count(&mut seat_info, (1, 3))
            );
        }
    }
}
