use std::sync::Arc;
use std::sync::mpsc;
use std::slice::Iter;
use std::collections::HashMap;
use std::collections::HashSet;

type Coords3D = (i32, i32, i32);
type Coords4D = (i32, i32, i32, i32);
type State<T: Day17Ops> = HashSet<T>;

// these directions were precomputed in Python using itertools.product
// i did not sit and write out all 106 directions by hand :)
const DIRECTIONS_3D: [Coords3D; 26] = [
    (-1, -1, -1),
    (-1, -1, 0),
    (-1, -1, 1),
    (-1, 0, -1),
    (-1, 0, 0),
    (-1, 0, 1),
    (-1, 1, -1),
    (-1, 1, 0),
    (-1, 1, 1),
    (0, -1, -1),
    (0, -1, 0),
    (0, -1, 1),
    (0, 0, -1),
    (0, 0, 1),
    (0, 1, -1),
    (0, 1, 0),
    (0, 1, 1),
    (1, -1, -1),
    (1, -1, 0),
    (1, -1, 1),
    (1, 0, -1),
    (1, 0, 0),
    (1, 0, 1),
    (1, 1, -1),
    (1, 1, 0),
    (1, 1, 1),
];


const DIRECTIONS_4D: [Coords4D; 80] = [
    (-1, -1, -1, -1),
    (-1, -1, -1, 0),
    (-1, -1, -1, 1),
    (-1, -1, 0, -1),
    (-1, -1, 0, 0),
    (-1, -1, 0, 1),
    (-1, -1, 1, -1),
    (-1, -1, 1, 0),
    (-1, -1, 1, 1),
    (-1, 0, -1, -1),
    (-1, 0, -1, 0),
    (-1, 0, -1, 1),
    (-1, 0, 0, -1),
    (-1, 0, 0, 0),
    (-1, 0, 0, 1),
    (-1, 0, 1, -1),
    (-1, 0, 1, 0),
    (-1, 0, 1, 1),
    (-1, 1, -1, -1),
    (-1, 1, -1, 0),
    (-1, 1, -1, 1),
    (-1, 1, 0, -1),
    (-1, 1, 0, 0),
    (-1, 1, 0, 1),
    (-1, 1, 1, -1),
    (-1, 1, 1, 0),
    (-1, 1, 1, 1),
    (0, -1, -1, -1),
    (0, -1, -1, 0),
    (0, -1, -1, 1),
    (0, -1, 0, -1),
    (0, -1, 0, 0),
    (0, -1, 0, 1),
    (0, -1, 1, -1),
    (0, -1, 1, 0),
    (0, -1, 1, 1),
    (0, 0, -1, -1),
    (0, 0, -1, 0),
    (0, 0, -1, 1),
    (0, 0, 0, -1),
    (0, 0, 0, 1),
    (0, 0, 1, -1),
    (0, 0, 1, 0),
    (0, 0, 1, 1),
    (0, 1, -1, -1),
    (0, 1, -1, 0),
    (0, 1, -1, 1),
    (0, 1, 0, -1),
    (0, 1, 0, 0),
    (0, 1, 0, 1),
    (0, 1, 1, -1),
    (0, 1, 1, 0),
    (0, 1, 1, 1),
    (1, -1, -1, -1),
    (1, -1, -1, 0),
    (1, -1, -1, 1),
    (1, -1, 0, -1),
    (1, -1, 0, 0),
    (1, -1, 0, 1),
    (1, -1, 1, -1),
    (1, -1, 1, 0),
    (1, -1, 1, 1),
    (1, 0, -1, -1),
    (1, 0, -1, 0),
    (1, 0, -1, 1),
    (1, 0, 0, -1),
    (1, 0, 0, 0),
    (1, 0, 0, 1),
    (1, 0, 1, -1),
    (1, 0, 1, 0),
    (1, 0, 1, 1),
    (1, 1, -1, -1),
    (1, 1, -1, 0),
    (1, 1, -1, 1),
    (1, 1, 0, -1),
    (1, 1, 0, 0),
    (1, 1, 0, 1),
    (1, 1, 1, -1),
    (1, 1, 1, 0),
    (1, 1, 1, 1)
];

/// Extension trait intended for integer tuples
/// Intefer tuples already satisfy all of these trait requirements anyways
trait Day17Ops: Sized + Eq + std::hash::Hash + Clone + Copy {
    /// Iterator over cartesian product of unit vectors (excluding the origin)
    fn directions<'a>() -> Iter<'a, Self>;

    /// Make the first two elements equal to x and y respectively, and the remaining elements 0
    fn extend_from_xy(x: i32, y: i32) -> Self;

    /// Add two tuples together
    fn add(&self, other: &Self) -> Self;
}

trait Print<T> where T: Day17Ops {
    fn print_state(state: &State<T>);
}

impl Day17Ops for Coords3D {
    fn directions<'a>() -> Iter<'a, Self> {
        return DIRECTIONS_3D.iter();
    }
    fn add(&self, other: &Self) -> Self {
        return (self.0 + other.0, self.1 + other.1, self.2 + other.2);
    }
    fn extend_from_xy(x: i32, y: i32) -> Self {
        return (x, y, 0);
    }
}

impl Day17Ops for Coords4D {
    fn directions<'a>() -> Iter<'a, Self> {
        return DIRECTIONS_4D.iter();
    }
    fn add(&self, other: &Self) -> Self {
        return (self.0 + other.0, self.1 + other.1, self.2 + other.2, self.3 + other.3);
    }
    fn extend_from_xy(x: i32, y: i32) -> Self {
        return (x, y, 0, 0);
    }
}

fn init_state<T: Day17Ops>(state: &mut State<T>, initializer: &str) {
    for (y, row) in initializer.split("\n").enumerate() {
        for (x, chr) in row.chars().enumerate() {
            match chr {
                '#' => {
                    state.insert(T::extend_from_xy(x as i32, y as i32));
                }
                _ => (), // do nothing
            }
        }
    }
}

fn count_active_cubes<T>(state: &State<T>) -> usize {
    return state.len();
}

/// Represents a change between states
struct StateDelta<T> {
    /// Cell locations that have become active
    add: State<T>,

    /// Cell locations that have become inactive
    remove: State<T>,
}

fn single_iter<T: Day17Ops>(state: &State<T>) -> StateDelta<T> {
    return single_iter_concurrent(state, 0, 1);
}


/// Execute a single iteration of the state
fn single_iter_concurrent<T: Day17Ops>(state: &State<T>, skip_initial: usize, step: usize) -> StateDelta<T> {
    let mut remove: State<T> = HashSet::new();
    let mut map_inactive_cubes_to_qty_active_neighbors: HashMap<T, u32> = HashMap::new();

    for coord in state.iter().skip(skip_initial as usize).step_by(step) {
        let neighbor_cubes: State<T> = T::directions()
            .map(|dir| dir.add(coord))
            .collect();
        let inactive_neighbors: HashSet<&T> = neighbor_cubes.difference(state).collect();
        let active_neighbors_count: usize = neighbor_cubes
            .intersection(state)
            .fold(0, |acc, _el| acc + 1);
        if active_neighbors_count != 2 && active_neighbors_count != 3 {
            remove.insert(*coord);
        }

        for neighbor in inactive_neighbors.iter() {
            match map_inactive_cubes_to_qty_active_neighbors.get_mut(neighbor) {
                None => {
                    map_inactive_cubes_to_qty_active_neighbors.insert(**neighbor, 1);
                }
                Some(val) => {
                    *val += 1;
                }
            };
        }
    }

    let add: State<T> = map_inactive_cubes_to_qty_active_neighbors
        .iter()
        .filter_map(|(coord, active_neighbor_count)| {
            if *active_neighbor_count == 3 {
                return Some(coord);
            }
            return None;
        })
        .cloned()
        .collect();

    return StateDelta { add, remove };
}

fn apply_delta<T: Day17Ops>(state: &mut State<T>, state_delta: &StateDelta<T>) {
    *state = state.difference(&state_delta.remove).cloned().collect();
    *state = state.union(&state_delta.add).cloned().collect();
}


impl Print<Coords3D> for State<Coords3D> {
    fn print_state(state: &Self) {
        let low_x = state.iter().map(|coord| coord.0).min().unwrap();
        let high_x = state.iter().map(|coord| coord.0).max().unwrap();

        let low_y = state.iter().map(|coord| coord.1).min().unwrap();
        let high_y = state.iter().map(|coord| coord.1).max().unwrap();

        let low_z = state.iter().map(|coord| coord.2).min().unwrap();
        let high_z = state.iter().map(|coord| coord.2).max().unwrap();

        for z in low_z..(high_z + 1) {
            println!("\n\nz = {}", z);
            for y in low_y..(high_y + 1) {
                for x in low_x..(high_x + 1) {
                    if state.contains(&(x, y, z)) {
                        print!("#");
                    } else {
                        print!(".");
                    }
                }
                print!("\n")
            }
        }
    }
}

impl Print<Coords4D> for State<Coords4D> {
    fn print_state(state: &Self) {
        let low_x = state.iter().map(|coord| coord.0).min().unwrap();
        let high_x = state.iter().map(|coord| coord.0).max().unwrap();
    
        let low_y = state.iter().map(|coord| coord.1).min().unwrap();
        let high_y = state.iter().map(|coord| coord.1).max().unwrap();
    
        let low_z = state.iter().map(|coord| coord.2).min().unwrap();
        let high_z = state.iter().map(|coord| coord.2).max().unwrap();
    
        let low_w = state.iter().map(|coord| coord.3).min().unwrap();
        let high_w = state.iter().map(|coord| coord.3).max().unwrap();
    
        for w in low_w..(high_w + 1) {
            for z in low_z..(high_z + 1) {
                println!("\n\nz = {}, w = {}", z, w);
                for y in low_y..(high_y + 1) {
                    for x in low_x..(high_x + 1) {
                        if state.contains(&(x, y, z, w)) {
                            print!("#");
                        } else {
                            print!(".");
                        }
                    }
                    print!("\n")
                }
            }
        }
    }
}

pub fn day17_main() {
    let INIT_STATE_FOR_Z0: &str = include_str!("../../inputs/day17.txt").trim_end();

    let mut state: State<Coords3D> = HashSet::new();
    init_state(&mut state, INIT_STATE_FOR_Z0);
    for _ in 0..6 {
        let state_delta = single_iter(&state);
        apply_delta(&mut state, &state_delta);
    }
    println!("Part 1: Number of active cubes: {}", count_active_cubes(&state));

    let mut state: State<Coords4D> = HashSet::new();
    init_state(&mut state, INIT_STATE_FOR_Z0);
    for _ in 0..6 {
        let state_delta = single_iter(&state);
        apply_delta(&mut state, &state_delta);
    }
    println!("Part 2: Number of active cubes: {}", count_active_cubes(&state));

    let mut state: State<Coords4D> = HashSet::new();
    init_state(&mut state, INIT_STATE_FOR_Z0);
    for _ in 0..100 {
        let threads = 10;
        let (tx, rx) = mpsc::channel::<StateDelta<Coords4D>>();

        let mut join_handles = Vec::new();

        for i in 0..threads {
            let cloned_tx = tx.clone();
            let cloned_state = state.iter().cloned().collect();
            join_handles.push(std::thread::spawn(move || {
                return single_iter_concurrent(&cloned_state, i, threads);
            }));
        }

        for handle in join_handles {
            apply_delta(&mut state, &handle.join().unwrap());
        }
    }
    println!("part 2 but multithreaded: Number of active cubes: {}", count_active_cubes(&state));
}
