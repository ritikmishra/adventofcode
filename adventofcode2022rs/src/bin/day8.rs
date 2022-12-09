use std::collections::HashMap;


const INPUT: &'static str = include_str!("../../../inputs2022/day8.txt");

const EXAMPLE: &'static str = "30373
25512
65332
33549
35390";

#[derive(Debug, Clone, Copy, Default)]
struct Thoughts {
    left_score: i32,
    left_visibility: i32,

    right_score: i32,
    right_visibility: i32,
    
    top_score: i32,
    top_visibility: i32,
    
    bottom_score: i32,
    bottom_visibility: i32
}

fn day8(input: &str)  {
    let trees: HashMap<(i32, i32), i32> = HashMap::from_iter(
        input.split('\n')
            .enumerate()
            .flat_map(|(row, line)| line.as_bytes().iter().enumerate().map(move |(col, numeral)| {
                ((row as i32, col as i32), i32::from(*numeral - b'0'))
            }))
    );

    let trees_numrows = trees.keys().map(|(row, _col)| *row).max().unwrap() + 1;
    let trees_numcols = trees.keys().map(|(_, col)| *col).max().unwrap() + 1;

    println!("{trees_numrows}, {trees_numcols}");

    let mut thinking: HashMap<(i32, i32), Thoughts> = HashMap::from_iter(trees.iter().map(|(key, _)| {
        (*key, Thoughts::default())
    }));

    // go top down
    for i in 1..trees_numrows {
        for j in 0..trees_numcols {
            let new_top_scorre = thinking[&(i - 1, j)].top_score.max(trees[&(i - 1, j)]);
            thinking.entry((i, j)).and_modify(|t| t.top_score = new_top_scorre);
        }
    }

    // go bottom up
    for i in (0..(trees_numrows - 1)).into_iter().rev() {
        for j in 0..trees_numcols {
            let new_top_scorre = thinking[&(i + 1, j)].bottom_score.max(trees[&(i + 1, j)]);
            thinking.entry((i, j)).and_modify(|t| t.bottom_score = new_top_scorre);
        }
    }


    // go from left
    for i in 0..trees_numrows {
        for j in 1..trees_numcols {
            let new_top_scorre = thinking[&(i, j - 1)].left_score.max(trees[&(i, j - 1)]);
            thinking.entry((i, j)).and_modify(|t| t.left_score = new_top_scorre);
        }
    }


    // go from right
    for i in 0..trees_numrows {
        for j in (0..trees_numcols - 1).into_iter().rev() {
            let new_top_scorre = thinking[&(i, j + 1)].right_score.max(trees[&(i, j + 1)]);
            thinking.entry((i, j)).and_modify(|t| t.right_score = new_top_scorre);
        }
    }

    let visible_tree_count = trees.iter().map(|(key, &val)| {
        let Thoughts { left_score, right_score, top_score, bottom_score } = thinking[key];
        let visible = left_score < val || right_score < val || top_score < val || bottom_score < val;
        let on_edge = key.0 == 0 || key.0 == trees_numrows - 1 || key.1 == 0 || key.1 == trees_numcols - 1;
        (key, visible || on_edge)
    }).inspect(|(key, visible)| {
        if key.0 == 0 || key.1 == 0 || key.1 == trees_numcols - 1 || key.0 == trees_numrows - 1 {
            // println!("{key:?} is visible: {visible}");
        }
    }).filter(|(_, b)| *b).count();

    println!("{:#?}, {:#?}", thinking[&(0, 1)], trees[&(0, 1)]);
    println!("Part 1: {visible_tree_count}");
}

fn main() {
    day8(INPUT);
}