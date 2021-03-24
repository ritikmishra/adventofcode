
const MAP_OF_SLOPE_AND_TREES: &str = include_str!("../inputs/day3.txt");

fn count_trees_on_path(map: &str, delta: (i32, i32)) -> i32 {
    let mut pos: (i32, i32) = (0, 0);
    let mut tree_count: i32 = 0;

    for (y, line) in map.split_whitespace().enumerate() {
        if y % (delta.1 as usize) == 0 {
            let char_idx = (pos.0 as usize) % line.len();
            let maybe_tree = line.chars().nth(char_idx as usize);
            match maybe_tree {
                Some(tree_or_rock) => {
                    if tree_or_rock == '#' {
                        tree_count += 1;
                    }
                },
                None => {
                    println!("uh oh even though we took the modulus by str len, couldn't get nth character");
                }
            }
            pos.0 += delta.0;
        }
        pos.1 += delta.1;
    }
    return tree_count;
}

fn day3_part1(map: &str) -> i32 {
    return count_trees_on_path(map, (3, 1));
}

fn day3_part2(map: &str) -> i128 {
    let deltas_to_check: [(i32, i32); 5] = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    return deltas_to_check.iter()
                .map(|delta| {
                    let ret = count_trees_on_path(map, *delta);
                    println!("delta {:?} had {} trees!", delta, ret);
                    return ret;
                })
                .fold(1 as i128, |acc, el| (el as i128) * acc);
}

pub fn day3_main() {
    let tree_count = day3_part1(MAP_OF_SLOPE_AND_TREES.trim_end());
    println!("(Part 1) Tree count: {}", tree_count);
    let multiplied_tree_count = day3_part2(MAP_OF_SLOPE_AND_TREES.trim_end());
    println!("(Part 2) Multiplied tree count: {}", multiplied_tree_count);
}