use std::collections::HashSet;
use nalgebra;
use nalgebra::Rotation2;
use nalgebra::Vector2;
use regex::Regex;
use std::collections::HashMap;

lazy_static! {
    /// e1 unit vector is (1, 0)
    /// e2 unit vector is (cos(pi/3), sin(pi/3)) (i.e unit vector pointing up right 60 deg)
    static ref DIRECTIONS: Vec<Vector2<i64>> = vec![
        Vector2::new(1, 0),
        Vector2::new(-1, 0),
        Vector2::new(0, 1),
        Vector2::new(1, -1),
        Vector2::new(-1, 1),
        Vector2::new(0, -1),
    ];
}

fn parse_tile_into_vec(tile: &str) -> Vector2<i64> {
    lazy_static! {
        static ref DIRECTION_REGEX: Regex = Regex::new("(ne|se|nw|sw|e|w)").unwrap();
    }
    let instr_map: HashMap<&str, Vector2<i64>> = {
        let mut ret: HashMap<&str, Vector2<i64>> = HashMap::new();
        // working with an altered basis
        // e1 unit vec is same
        // e2 unit vec is e1 rotated ccw pi/3 radians
        ret.insert("e", Vector2::new(1, 0));
        ret.insert("w", Vector2::new(-1, 0));
        ret.insert("ne", Vector2::new(0, 1));
        ret.insert("se", Vector2::new(1, -1));
        ret.insert("nw", Vector2::new(-1, 1));
        ret.insert("sw", Vector2::new(0, -1));
        ret
    };

    let mut ret_location = Vector2::new(0, 0);

    for instr in DIRECTION_REGEX.find_iter(tile) {
        ret_location += instr_map.get(instr.as_str()).unwrap();
    }

    return ret_location;
}

type State = HashSet<Vector2<i64>>;


fn single_iter(black_tiles: &mut State) {
    let mut remove: State = HashSet::new();
    let mut map_white_tiles_to_black_neighbors: HashMap<Vector2<i64>, u32> = HashMap::new();

    for tile in black_tiles.iter() {
        let neighbor_tiles: State = DIRECTIONS
            .iter()
            .map(|dir| dir + tile)
            .collect();
        let white_neighbors: State = neighbor_tiles.difference(black_tiles).cloned().collect();
        let black_neighbors_count: usize = neighbor_tiles
            .intersection(black_tiles)
            .fold(0, |acc, _el| acc + 1);
        if black_neighbors_count == 0 || black_neighbors_count > 2 {
            remove.insert(*tile); // flip tile to white
        }

        for neighbor in white_neighbors.iter() {
            match map_white_tiles_to_black_neighbors.get_mut(neighbor) {
                None => {
                    map_white_tiles_to_black_neighbors.insert(*neighbor, 1);
                }
                Some(val) => {
                    *val += 1;
                }
            };
        }
    }

    let add: State = map_white_tiles_to_black_neighbors
        .iter()
        .filter_map(|(tile, black_neighbor_count)| {
            if *black_neighbor_count == 2 {
                return Some(tile);
            }
            return None;
        })
        .cloned()
        .collect();

    *black_tiles = black_tiles.difference(&remove).cloned().collect();
    *black_tiles = black_tiles.union(&add).cloned().collect();
}

pub fn day24_main() {
    let input = include_str!("../inputs/day24.txt");
    let mut black_tiles: State = HashSet::new();
    let tiles: Vec<Vector2<i64>> = input.trim_end().split("\n").map(&parse_tile_into_vec).collect();
    for tile in tiles.iter() {
        match black_tiles.contains(tile) {
            true => {
                black_tiles.remove(tile); // flipped to black
            }
            false => {black_tiles.insert(tile.clone());}, // flip the other way
        };
    }
    println!(
        "part 1: {} black tiles",
        black_tiles.len()
    );

    for _ in 0..100 {
        single_iter(&mut black_tiles);
    }

    println!(
        "part 2: {} black tiles",
        black_tiles.len()
    );
}
