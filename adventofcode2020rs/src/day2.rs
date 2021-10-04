use regex::Regex;
use std::str::FromStr;

#[derive(Debug)]
struct PasswordDbRow {
    lower_bound: i32,
    upper_bound: i32,
    character: char,

    password: String,
}

impl FromStr for PasswordDbRow {
    type Err = String;

    fn from_str(row: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref PASSWORD_ROW_REGEX: Regex = Regex::new(r"(\d+)-(\d+) (.): (.+)").unwrap();
        }

        if !PASSWORD_ROW_REGEX.is_match(row) {
            return Err(String::from("regex didn't match"));
        }

        let maybe_captures = PASSWORD_ROW_REGEX.captures(row);
        return match maybe_captures {
            Some(capture) => {
                let lower_bound = capture
                    .get(1)
                    .map(|s| s.as_str().parse::<i32>().unwrap())
                    .unwrap();
                let upper_bound = capture
                    .get(2)
                    .map(|s| s.as_str().parse::<i32>().unwrap())
                    .unwrap();
                let character = capture
                    .get(3)
                    .map(|s| s.as_str().chars().nth(0).unwrap())
                    .unwrap();
                let password = String::from(capture.get(4).unwrap().as_str());

                Ok(PasswordDbRow {
                    lower_bound,
                    upper_bound,
                    character,
                    password,
                })
            }
            None => Err(String::from("no captures found")),
        };
    }
}

fn is_valid_password_part1_rules(row: &PasswordDbRow) -> bool {
    let mut count = 0;
    for c in row.password.chars() {
        if c == row.character {
            count += 1;
        }
    }
    return row.lower_bound <= count && count <= row.upper_bound
}

fn is_valid_password_part2_rules(row: &PasswordDbRow) -> bool {
    println!("{:?}", row);
    let first_pos = row.password.chars().nth((row.lower_bound-1) as usize).unwrap();
    let second_pos = row.password.chars().nth((row.upper_bound-1) as usize).unwrap();
    return (first_pos == row.character) ^ (second_pos == row.character);
}

fn check_all_passwords(password_db: &str, validate_row: fn(&PasswordDbRow) -> bool) {
    let mut valid_passwords = 0;
    for password_db_row in password_db.split("\n") {
        let parsed_row = PasswordDbRow::from_str(password_db_row).unwrap();
        if validate_row(&parsed_row) {
            valid_passwords += 1;
        }
    }
    println!("{}", valid_passwords);
}

pub fn day2_main() {
    let password_db = include_str!("../../inputs/day2.txt").trim_end();
    check_all_passwords(password_db, is_valid_password_part2_rules)
}