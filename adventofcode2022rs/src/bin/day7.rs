use std::collections::HashMap;

const INPUT: &'static str = include_str!("../../../inputs2022/day7.txt");

#[derive(Debug, Clone)]
enum Command<'a> {
    Cd { arg: &'a str },
    Ls,
}

#[derive(Debug, Clone)]
struct Entry<'a> {
    name: &'a str,
    size: Option<i32>,
}

#[derive(Debug, Clone)]
enum FsEntry<'a> {
    Directory { children: Path<'a> },
    File { size: i32 },
}

type Path<'a> = Vec<&'a str>;

fn day7(input: &str) {
    let cmd_responses = input
        .split("$ ")
        .skip(1)
        .map(|cmd_and_response| {
            let mut x = cmd_and_response.split("\n");
            let cmd = x.next().unwrap();
            let response = x
                .filter(|str| str.len() > 0)
                .map(|line| {
                    let (kind, name) = line.split_once(' ').unwrap();
                    Entry {
                        name,
                        size: match kind {
                            "dir" => None,
                            number => Some(number.parse::<i32>().unwrap()),
                        },
                    }
                })
                .collect::<Vec<_>>();

            let cmd = match cmd.split_at(2) {
                ("cd", cd_arg) => Command::Cd {
                    arg: cd_arg.split_at(1).1,
                },
                ("ls", "") => Command::Ls,
                _ => panic!("what the fuck"),
            };
            (cmd, response)
        })
        .collect::<Vec<_>>();

    let mut fs: HashMap<Path, FsEntry> = HashMap::new();
    let mut cwd: Path = Path::new();

    // Parse filesystem
    for (cmd, response) in cmd_responses {
        match cmd {
            Command::Cd { arg: ".." } => {
                cwd.pop().expect("tried to `cd ..` out of root?");
            }
            Command::Cd { arg } => {
                // Change our current directory
                cwd.push(arg);
                // Add it to the filesystem
                fs.entry(cwd.clone()).or_insert(FsEntry::Directory {
                    children: Vec::new(),
                });
            }
            Command::Ls => {
                for Entry { name, size } in response {
                    // Add this item to the filesystem
                    let name_vec = {
                        let mut c = cwd.clone();
                        c.push(name);
                        c
                    };
                    fs.insert(
                        name_vec,
                        size.map_or(FsEntry::Directory { children: Vec::new() }, |size| FsEntry::File { size })
                    );

                    // Mark this item as being a child of our current folder
                    match fs.get_mut(&cwd).unwrap() {
                        FsEntry::File { .. } => panic!(
                            "we are supposed to be in a directory, but we're in {cwd:?} instead!"
                        ),
                        FsEntry::Directory { children, .. } => {
                            children.push(name);
                        }
                    }
                }
            }
        }
    }

    let mut dir_size: HashMap<Path, i32> = HashMap::new();
    /// Recurse through the directory structure, finding the size of each directory
    /// and storing it in `out`
    fn calc_dir_size<'a>(
        path: Vec<&'a str>,
        fs: &mut HashMap<Path<'a>, FsEntry<'a>>,
        out: &mut HashMap<Path<'a>, i32>,
    ) -> i32 {
        let fs_entry = fs.get(&path).unwrap().clone();
        match fs_entry {
            FsEntry::Directory { children } => {
                let our_size = children
                    .iter()
                    .map(|child| {
                        let mut p = path.clone();
                        p.push(*child);
                        p
                    })
                    .map(|path| calc_dir_size(path, fs, out))
                    .sum::<i32>();
                out.insert(path, our_size);
                our_size
            }
            FsEntry::File { size } => size,
        }
    }
    calc_dir_size(vec!["/"], &mut fs, &mut dir_size);

    let part1 = dir_size.values().filter(|v| **v <= 100000).sum::<i32>();

    // let mut cwd: Vec<&str> = Vec::new();
    println!("{part1:?}");
    println!("{:#?}", dir_size[&vec!["/"]]);

    let total_capacity = 70000000;
    let need_to_have = 30000000;
    let current_free_space = total_capacity - dir_size[&vec!["/"]];
    let need_to_free = need_to_have - current_free_space;
    let (name, size) = dir_size
        .iter()
        .filter(|(_, size)| **size >= need_to_free)
        .min_by_key(|(_, size)| **size)
        .unwrap();
    println!("{name:?}, {size:?}");
}

fn main() {
    day7(INPUT);
}
