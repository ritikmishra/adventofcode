const BUS_TIMETABLES: &str = include_str!("../inputs/day13.txt");
// 7,13,x,x,59,x,31,19";

#[derive(Debug)]
struct Departure {
    time: i64,
    bus_id: i64,
}

// returns the beizout coefficient of a
fn extended_gcd(a: i64, b: i64) -> i64 {
    let (mut two_ago_r, mut r) = (a, b);
    let (mut two_ago_s, mut s) = (1, 0);
    let (mut two_ago_t, mut t) = (0, 1);
    
    while r != 0 {
        let quotient = two_ago_r / r;

        let one_ago_r = r;
        r = two_ago_r - quotient * one_ago_r;
        two_ago_r = one_ago_r;
        
        let one_ago_s = s;
        s = two_ago_s - quotient * one_ago_s;
        two_ago_s = one_ago_s;

        let one_ago_t = t;
        t = two_ago_t - quotient * one_ago_t;
        two_ago_t = one_ago_t;
    }

    return two_ago_s;

    // output "Bézout coefficients:", (old_s, old_t)
    // output "greatest common divisor:", old_r
    // output "quotients by the gcd:", (t, s)
}

fn earliest_viable_departure_time(time_right_now: i64, bus_ids: &Vec<Option<i64>>) -> Departure {
    return bus_ids
        .iter()
        .filter(|val| match val {
            None => false,
            Some(_) => true,
        })
        .map(|val| val.unwrap())
        .map(|bus_id| {
            let how_long_ago_did_the_last_bus_leave = time_right_now % bus_id;
            if how_long_ago_did_the_last_bus_leave == 0 {
                return Departure {
                    time: 0,
                    bus_id: bus_id,
                }; // we can leave immediately
            }
            let minutes_to_next_bus_arrival = bus_id - how_long_ago_did_the_last_bus_leave;
            return Departure {
                time: minutes_to_next_bus_arrival,
                bus_id: bus_id,
            };
        })
        .min_by_key(|departure| departure.time)
        .unwrap();
}

/// returns the lowest positive number X where 
/// X % n1 = a1 
/// X % n2 = a2
fn chinese_remainder_theorem(bus_id_pair_1: (i64, i64), bus_id_pair_2: (i64, i64)) -> i64 {
    let (a1, n1) = bus_id_pair_1;
    let (a2, n2) = bus_id_pair_2;
    
    let mut cur = a1;
    loop {
        if cur % n2 == a2 {
            return cur;
        }
        cur += n1;
    }
}

fn earliest_time_all_buses_depart_sequentially(bus_ids: &Vec<Option<i64>>) -> i64 {
    
    // list of tuples of pos in list, bus id
    let bus_offset_id_iter: Vec<(i64, i64)> = bus_ids
    .iter()
    .enumerate()
    .filter(|(i, val)| match val {
        None => false,
        Some(_) => true,
    })
    .map(|(i, bus_id)| ((bus_id.unwrap() - i as i64) % bus_id.unwrap(), bus_id.unwrap()))
    .collect();

    let bus_id_prod: i64 = bus_offset_id_iter.iter().map(|pair| pair.1).product();

    let ret = bus_offset_id_iter.iter().map(|(offset, bus_id)| {
        let exclude_this_bus_id_in_prod = bus_id_prod / bus_id;
        let beizout_coeff = extended_gcd(exclude_this_bus_id_in_prod, *bus_id);
        return beizout_coeff * exclude_this_bus_id_in_prod * (*offset);
    }).sum::<i64>() % bus_id_prod;
    if ret < 0 {
        return ret + bus_id_prod;
    }
    return ret;
    // l
    
    // let max_bus_id = bus_offset_id_iter.iter().max_by_key(|val| val.1).unwrap();
    // let mut t: i64 = 100_000_000_000_000 - (100_000_000_000_000 % max_bus_id.0);
    // let increment: i64 = max_bus_id.1;
    // loop {
    //     let mut meets_condition = true;
    //     for (i, bus_id) in bus_offset_id_iter.iter() {
    //         if (t + i) % bus_id != 0 {
    //             meets_condition = false;
    //             break;
    //         }
    //     }
    //     if meets_condition {
    //         return t;
    //     }
    //     t += increment;
    // }
}

pub fn day13_main() {
    let notes_split: Vec<&str> = BUS_TIMETABLES.trim_end().split("\n").collect();
    let time_now = notes_split.get(0).unwrap().parse::<i64>().unwrap();
    let mut bus_ids: Vec<Option<i64>> = Vec::new();

    for maybe_bus_id in notes_split.get(1).unwrap().split(",") {
        match maybe_bus_id {
            "x" => bus_ids.push(None),
            bus_id => bus_ids.push(Some(bus_id.parse::<i64>().unwrap())),
        };
    }

    println!(
        "departure time, bus = {:?}",
        earliest_viable_departure_time(time_now, &bus_ids)
    );

    println!(
        "ceremonial departure time, bus = {:?}",
        earliest_time_all_buses_depart_sequentially(&bus_ids)
    );
}
