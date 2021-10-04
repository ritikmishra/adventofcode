use std::hash::Hash;
use std::fmt::Display;
use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

const TILES: &str = include_str!("../../inputs/day20.txt");

/// Describes whether or not a tile has been flipped from the original input.
#[derive(Debug, Clone)]
enum Flipped {
    NotFlipped,
    FlippedHorizontally, // each row is reversed
                         // A vertical flip can be represented by Orientation::Rotated180(FlippedHorizontally)
}

/// Describes the orientation of a tile relative to the original input
#[derive(Debug, Clone)]
enum Orientation {
    NotRotated(Flipped),
    Rotated90(Flipped),
    Rotated180(Flipped),
    Rotated270(Flipped),
}

/// Describes the position of a tile B relative to another tile A
#[derive(Debug)]
enum RelativePosition {
    Above,
    Below,
    Left,
    Right,
}

/// Type alias for a map of coords to a pixel value
type PixelMap = HashMap<(usize, usize), char>;

/// Tile struct
#[derive(Clone)]
struct Tile {
    /// map of (row num, col num) to pixel that is at that location
    pixel_map: PixelMap,
    tile_id: u32,
    orientation: Orientation,
    /// length and width of this tile. this means only square tiles are supported.
    tile_size: usize
}


impl std::fmt::Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // don't print pixel map, that makes the output unreadable
        f.debug_struct("Tile")
         .field("tile_id", &self.tile_id)
         .field("orientation", &self.orientation)
         .finish()
    }
}

/// Default tile size for tiles from the input
const TILE_SIZE: usize = 10;

fn parse_tiles(all_tiles: &str) -> HashMap<u32, Tile> {
    lazy_static! {
        static ref TILE_REGEX: Regex = Regex::new(r"^Tile (\d{4}):\n([\.#\n]*)").unwrap();
    }
    let mut ret = HashMap::new();
    for tile_def in all_tiles.split("\n\n") {
        let tile_captures = TILE_REGEX.captures(tile_def).unwrap();
        let tile_id = tile_captures
            .get(1)
            .unwrap()
            .as_str()
            .parse::<u32>()
            .unwrap();
        let tile = tile_captures.get(2).unwrap().as_str();

        let mut tile_pixel_map: HashMap<(usize, usize), char> = HashMap::new();
        for (i, row) in tile.split("\n").enumerate() {
            for (j, chr) in row.chars().enumerate() {
                tile_pixel_map.insert((i, j), chr);
            }
        }
        ret.insert(
            tile_id,
            Tile {
                tile_id,
                pixel_map: tile_pixel_map,
                orientation: Orientation::NotRotated(Flipped::NotFlipped),
                tile_size: TILE_SIZE
            },
        );
    }
    return ret;
}

fn check_if_boundary_matches(tile_a: &Tile, tile_b: &Tile) -> Option<RelativePosition> {
    check_if_boundary_matches_impl(tile_a, tile_b, false)
}

fn check_if_boundary_matches_impl(
    tile_a: &Tile,
    tile_b: &Tile,
    second: bool,
) -> Option<RelativePosition> {
    // b is above a
    let b_bottom_pixels: PixelMap = tile_b
        .pixel_map
        .iter()
        .filter_map(|(loc, pixel)| {
            if loc.0 == (tile_b.tile_size - 1) {
                Some((*loc, *pixel))
            } else {
                None
            }
        })
        .collect();
    let a_top_pixels: PixelMap = tile_a
        .pixel_map
        .iter()
        .filter_map(|(loc, pixel)| {
            if loc.0 == 0 {
                Some((*loc, *pixel))
            } else {
                None
            }
        })
        .collect();

    if a_top_pixels.iter().all(|(location, pixel)| {
        match b_bottom_pixels.get(&(tile_b.tile_size - 1, location.1)) {
            Some(b_pixel) => pixel == b_pixel,
            None => false,
        }
    }) {
        return Some(RelativePosition::Above); // b bottom pixels match with a top pixels -> b is above a
    }
    // b is left of a
    let b_left_pixels: PixelMap = tile_b
        .pixel_map
        .iter()
        .filter_map(|(loc, pixel)| {
            if loc.1 == 0 {
                Some((*loc, *pixel))
            } else {
                None
            }
        })
        .collect();
    let a_right_pixels: PixelMap = tile_a
        .pixel_map
        .iter()
        .filter_map(|(loc, pixel)| {
            if loc.1 == (tile_a.tile_size - 1) {
                Some((*loc, *pixel))
            } else {
                None
            }
        })
        .collect();

    if a_right_pixels.iter().all(
        |(location, pixel)| match b_left_pixels.get(&(location.0, 0)) {
            Some(b_pixel) => pixel == b_pixel,
            None => false,
        },
    ) {
        return Some(RelativePosition::Right); // b left pixels match with a right pixels -> b is to the right of a
    }

    // b below a or b to the right of a
    if !second {
        match check_if_boundary_matches_impl(tile_b, tile_a, true) {
            Some(a_pos_relative_to_b) => match a_pos_relative_to_b {
                RelativePosition::Above => return Some(RelativePosition::Below),
                RelativePosition::Right => return Some(RelativePosition::Left),
                _ => panic!("how did this happen!"),
            },
            None => return None,
        }
    }
    None
}

fn flip_flipped(flipped: &Flipped) -> Flipped {
    match flipped {
        Flipped::NotFlipped => Flipped::FlippedHorizontally,
        Flipped::FlippedHorizontally => Flipped::NotFlipped,
    }
}

fn flip_orientation(orientation: &Orientation) -> Orientation {
    match orientation {
        Orientation::NotRotated(f) => Orientation::NotRotated(flip_flipped(f)),
        Orientation::Rotated90(f) => Orientation::Rotated90(flip_flipped(f)),
        Orientation::Rotated180(f) => Orientation::Rotated180(flip_flipped(f)),
        Orientation::Rotated270(f) => Orientation::Rotated270(flip_flipped(f)),
    }
}

fn flip_tile(tile_to_flip: &Tile) -> Tile {
    Tile {
        tile_id: tile_to_flip.tile_id,
        orientation: flip_orientation(&tile_to_flip.orientation),
        pixel_map: tile_to_flip
            .pixel_map
            .iter()
            .map(|((row, col), val)| ((*row, tile_to_flip.tile_size - 1 - col), *val))
            .collect(),
        tile_size: tile_to_flip.tile_size
    }
}

/// rotates counterclockwise
fn rotate_orientation(orientation: &Orientation) -> Orientation {
    match orientation {
        Orientation::NotRotated(f) => Orientation::Rotated90(f.clone()),
        Orientation::Rotated90(f) => Orientation::Rotated180(f.clone()),
        Orientation::Rotated180(f) => Orientation::Rotated270(f.clone()),
        Orientation::Rotated270(f) => Orientation::NotRotated(f.clone()),
    }
}

// rotates a tile by 90 deg
fn rotate_tile_90_deg(tile_to_rot: &Tile) -> Tile {
    Tile {
        tile_id: tile_to_rot.tile_id,
        orientation: rotate_orientation(&tile_to_rot.orientation),
        pixel_map: tile_to_rot
            .pixel_map
            .iter()
            .map(|((row, col), val)| {
                ((tile_to_rot.tile_size - 1 - col, *row), *val)
            })
            .collect(),
        tile_size: tile_to_rot.tile_size
    }
}

fn gridlike_object_to_str<T: Display + Eq + Hash + Clone, U: Eq + Hash + Clone + Copy + Ord>(
    tile: &HashMap<(U, U), T>,
) -> String {
    let mut result = String::new();
    let map_row_to_col_val_pair: HashMap<U, HashSet<(U, &T)>> = HashMap::new();
    let mut row_colval_pairs: Vec<(U, HashSet<(U, &T)>)> = tile
        .iter()
        .fold(map_row_to_col_val_pair, |mut acc, ((row, col), val)| {
            match acc.get_mut(row) {
                Some(set) => {
                    set.insert((*col, val));
                }
                None => {
                    let mut set = HashSet::new();
                    set.insert((*col, val));
                    acc.insert(*row, set);
                }
            };
            return acc;
        })
        .iter()
        .map(|(a, b)| (*a, b.clone()))
        .collect();

    row_colval_pairs.sort_by_key(|el| el.0);

    for (_row, colset) in row_colval_pairs {
        let mut sorted_col_values: Vec<(U, &&T)> = colset
            .iter()
            .map(|(col, pixelval)| (*col, pixelval))
            .collect();
        sorted_col_values.sort_by_key(|el| el.0);
        for (_col, pixelval) in sorted_col_values {
            result.push_str(format!("{}", pixelval).as_str());
        }
        result.push('\n');
    }

    return result;
}

/// tries ALL 8 of the combinations
fn check_if_two_tiles_fit(tile_a: &Tile, tile_b: &Tile) -> Option<(RelativePosition, Option<Tile>)> {
    let mut tile_b_transformed: Tile = tile_b.clone();
    for i in 0..4 {
        match check_if_boundary_matches(tile_a, &tile_b_transformed) {
            Some(pos) => return Some((pos, if i == 0 { None } else { Some(tile_b_transformed) })),
            None => {}
        };
        tile_b_transformed = rotate_tile_90_deg(&tile_b_transformed);
    }

    // flip tile_b and try again
    tile_b_transformed = flip_tile(&tile_b_transformed);
    for _ in 0..4 {
        match check_if_boundary_matches(tile_a, &tile_b_transformed) {
            Some(pos) => return Some((pos, Some(tile_b_transformed))),
            None => {}
        };
        tile_b_transformed = rotate_tile_90_deg(&tile_b_transformed);
    }
    return None
}

fn remove_boundary_from_pixel_grid(tile: &Tile) -> Tile {
    Tile {
        tile_id: tile.tile_id,
        pixel_map: tile.pixel_map.clone().into_iter().filter(|(loc, _val)| {
        0 < loc.0 && loc.0 < tile.tile_size - 1 && 0 < loc.1  && loc.1 < tile.tile_size - 1
        })
        .map(|(loc, val)| {
            ((loc.0 - 1, loc.1 - 1), val)
        })
        .collect(),
        tile_size: tile.tile_size - 2,
        orientation: tile.orientation.clone()
    }
}

fn count_sea_monsters_in_image(tile: &Tile) -> usize {
    assert!(tile.tile_size > 18, "triyng to count sea monsters in a tiny tile smh my head");

    lazy_static! {
        static ref MONSTER_MAP: HashMap<(usize, usize), char> = {
            let mut sea_monster = HashMap::new();
            sea_monster.insert((0, 0), ' ');
            sea_monster.insert((0, 1), ' ');
            sea_monster.insert((0, 2), ' ');
            sea_monster.insert((0, 3), ' ');
            sea_monster.insert((0, 4), ' ');
            sea_monster.insert((0, 5), ' ');
            sea_monster.insert((0, 6), ' ');
            sea_monster.insert((0, 7), ' ');
            sea_monster.insert((0, 8), ' ');
            sea_monster.insert((0, 9), ' ');
            sea_monster.insert((0, 10), ' ');
            sea_monster.insert((0, 11), ' ');
            sea_monster.insert((0, 12), ' ');
            sea_monster.insert((0, 13), ' ');
            sea_monster.insert((0, 14), ' ');
            sea_monster.insert((0, 15), ' ');
            sea_monster.insert((0, 16), ' ');
            sea_monster.insert((0, 17), ' ');
            sea_monster.insert((0, 18), '#');
            sea_monster.insert((0, 19), ' ');
            sea_monster.insert((1, 0), '#');
            sea_monster.insert((1, 1), ' ');
            sea_monster.insert((1, 2), ' ');
            sea_monster.insert((1, 3), ' ');
            sea_monster.insert((1, 4), ' ');
            sea_monster.insert((1, 5), '#');
            sea_monster.insert((1, 6), '#');
            sea_monster.insert((1, 7), ' ');
            sea_monster.insert((1, 8), ' ');
            sea_monster.insert((1, 9), ' ');
            sea_monster.insert((1, 10), ' ');
            sea_monster.insert((1, 11), '#');
            sea_monster.insert((1, 12), '#');
            sea_monster.insert((1, 13), ' ');
            sea_monster.insert((1, 14), ' ');
            sea_monster.insert((1, 15), ' ');
            sea_monster.insert((1, 16), ' ');
            sea_monster.insert((1, 17), '#');
            sea_monster.insert((1, 18), '#');
            sea_monster.insert((1, 19), '#');
            sea_monster.insert((2, 0), ' ');
            sea_monster.insert((2, 1), '#');
            sea_monster.insert((2, 2), ' ');
            sea_monster.insert((2, 3), ' ');
            sea_monster.insert((2, 4), '#');
            sea_monster.insert((2, 5), ' ');
            sea_monster.insert((2, 6), ' ');
            sea_monster.insert((2, 7), '#');
            sea_monster.insert((2, 8), ' ');
            sea_monster.insert((2, 9), ' ');
            sea_monster.insert((2, 10), '#');
            sea_monster.insert((2, 11), ' ');
            sea_monster.insert((2, 12), ' ');
            sea_monster.insert((2, 13), '#');
            sea_monster.insert((2, 14), ' ');
            sea_monster.insert((2, 15), ' ');
            sea_monster.insert((2, 16), '#');
            sea_monster.insert((2, 17), ' ');
            sea_monster.insert((2, 18), ' ');
            sea_monster.insert((2, 19), ' ');
            sea_monster
        };
    }


    let mut ret: usize = 0;
    for tile_r in 0..(tile.tile_size - 2) {
        for tile_c in 0..(tile.tile_size - 18) {
            let localized_monster_map: HashMap<(usize, usize), char> = MONSTER_MAP.clone().into_iter().map(|((r, c), val)| ((tile_r + r, tile_c + c), val)).collect();
            if localized_monster_map.iter().all(|((r, c), val)| {
                match  tile.pixel_map.get(&(*r, *c)) {
                    Some(actual_pixel) => *val == ' ' || *actual_pixel == *val,
                    None => false
                }
            }) {
                ret += 1;
            }
        }
    }
    return ret;
}

pub fn day20_main() {
    let selected_tiles = TILES.trim_end();
    let parsed_tiles = parse_tiles(selected_tiles);

    // ### Begin Tile arrangement

    // list of tiles to check next for neighbors
    let mut tiles_to_check_next: Vec<(Tile, (i32, i32))> = parsed_tiles.values().cloned().map(|tile| (tile, (0, 0))).take(1).collect();

    // set of tile ids that we have already checked
    let mut consumed_tiles: HashSet<u32> = HashSet::new();

    // map of position relative to the first tile (tile units) to the tile in question
    let mut assembled_tiles: HashMap<(i32, i32), Tile> = HashMap::new();
    while tiles_to_check_next.len() > 0 {
        let mut new_tiles_to_check_next: Vec<(Tile, (i32, i32))> = Vec::new();
        let mut new_tiles_to_check_next_ids: HashSet<u32> = HashSet::new();

        for (tile_a, tile_a_position) in tiles_to_check_next {
            // check if this tile matches against all other available tiles
            for tile_b in parsed_tiles.iter().filter(|(id, _)| **id != tile_a.tile_id && !consumed_tiles.contains(id)).map(|a| a.1) {
                // if tile_a matches with tile_b, put tile_b into 
                //   -  the assembled tiles map
                //   -  the consumed tiles set
                //   -  the list of tiles to check next
                match check_if_two_tiles_fit(&tile_a, tile_b) {
                    None => {},
                    Some((pos, maybe_rotated_tile_b)) => {
                        let tile_b_position = match pos {
                            RelativePosition::Above => (tile_a_position.0 - 1, tile_a_position.1),
                            RelativePosition::Below => (tile_a_position.0 + 1, tile_a_position.1),
                            RelativePosition::Left => (tile_a_position.0, tile_a_position.1 - 1),
                            RelativePosition::Right => (tile_a_position.0, tile_a_position.1 + 1),
                        };
                        
                        let tile_b_matching_orientation = maybe_rotated_tile_b.unwrap_or(tile_b.clone());
                        
                        if !new_tiles_to_check_next_ids.contains(&tile_b_matching_orientation.tile_id) {
                            new_tiles_to_check_next_ids.insert(tile_b_matching_orientation.tile_id);
                            new_tiles_to_check_next.push((tile_b_matching_orientation.clone(), tile_b_position));
                        }
                    }
                }
            }
            assembled_tiles.insert(tile_a_position, tile_a);
        }

        tiles_to_check_next = new_tiles_to_check_next;
        consumed_tiles.extend(new_tiles_to_check_next_ids);
        // tiles_to_check_next_ids = new_tiles_to_check_next_ids;
    }
    
    // We have figured out the tile arrangement, now we need to get rid of the negative numbers
    
    // If there are negatives, the min calls will be negative, otherwise they'll be 0.
    // abs gives us how much to add to get rid of the negatives
    let row_add = assembled_tiles.keys().map(|(r, _)| r).min().unwrap().abs();
    let col_add = assembled_tiles.keys().map(|(_, c)| c).min().unwrap().abs();
    
    // remaps so that all indexes are positive
    let assembled_tiles: HashMap<(usize, usize), Tile> = assembled_tiles.into_iter().map(|((r, c), val)| (((r + row_add) as usize, (c + col_add) as usize), val)).collect();
     
    // ### Tile arrangement complete

    // Part 1: find corners of the image
    let assembled_tiles_size: usize = assembled_tiles.keys().map(|(r, _c)| *r).max().unwrap();
    let top_left_tile: &Tile = assembled_tiles.get(&(0, 0)).unwrap();
    let top_right_tile: &Tile = assembled_tiles.get(&(0, assembled_tiles_size)).unwrap();
    let bot_left_tile: &Tile = assembled_tiles.get(&(assembled_tiles_size, 0)).unwrap();
    let bot_right_tile: &Tile = assembled_tiles.get(&(assembled_tiles_size, assembled_tiles_size)).unwrap();

    let ids: [u64; 4] = [top_left_tile.tile_id as u64, top_right_tile.tile_id as u64, bot_left_tile.tile_id as u64, bot_right_tile.tile_id as u64];
    println!("part 1 - corner id product: {}\n", ids.iter().cloned().product::<u64>());

    // ### Begin image assembly
    let mut image_pixels: HashMap<(usize, usize), char> = HashMap::new();
    for ((image_tile_row, image_tile_col), tile_with_borders) in assembled_tiles.into_iter() {
        let tile = remove_boundary_from_pixel_grid(&tile_with_borders);
        for ((tile_pixel_row, tile_pixel_col), pixel_value) in tile.pixel_map.clone().into_iter() {
            let image_pixel_row = (image_tile_row * tile.tile_size) + tile_pixel_row;
            let image_pixel_col = (image_tile_col * tile.tile_size) + tile_pixel_col;

            image_pixels.insert((image_pixel_row, image_pixel_col), pixel_value);
        }
    }

    let image_size = image_pixels.keys().map(|(r, _c)| *r).max().unwrap();

    let mut image: Tile = Tile {
        tile_id: 69,
        pixel_map: image_pixels,
        orientation: Orientation::NotRotated(Flipped::NotFlipped),
        tile_size: image_size
    };
    println!("{}", gridlike_object_to_str(&image.pixel_map));
    // ### End image assembly
    
    // Begin part 2
    const SEA_MONSTER_HASH_SIZE: usize = 15;
    for _ in 0..4 {
        let counted_sea_monsters = count_sea_monsters_in_image(&image);
        if counted_sea_monsters > 0 {
            let all_hash_count = gridlike_object_to_str(&image.pixel_map).chars().filter(|chr| *chr == '#').collect::<Vec<char>>().len();
            println!("counted {} monsters, {} total hashes", counted_sea_monsters, all_hash_count);
            let non_monster_hash_count = all_hash_count - (counted_sea_monsters * SEA_MONSTER_HASH_SIZE);
            println!("non monster hash count: {}", non_monster_hash_count)
        }
        // println!("{}\n\n", gridlike_object_to_str(&assembled_image.pixel_map));
        image = rotate_tile_90_deg(&image);
    }

    image = flip_tile(&image);

    for _ in 0..4 {
        let counted_sea_monsters = count_sea_monsters_in_image(&image);
        if counted_sea_monsters > 0 {
            let all_hash_count = gridlike_object_to_str(&image.pixel_map).chars().filter(|chr| *chr == '#').collect::<Vec<char>>().len();
            println!("counted {} monsters, {} total hashes", counted_sea_monsters, all_hash_count);
            let non_monster_hash_count = all_hash_count - (counted_sea_monsters * SEA_MONSTER_HASH_SIZE);
            println!("non monster hash count: {}", non_monster_hash_count)
        }
        image = rotate_tile_90_deg(&image);
    }
}
