use regex::Regex;

const PASSPORTS: &str = include_str!("../inputs/day4.txt");

#[derive(Debug, Clone)]
struct InvalidPassport<'a>(&'a str);

const REQUIRED_FIELDS: [&'static str; 7] = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

fn validate_number(captured_str: &str, lower_bound: i32, upper_bound: i32) -> bool {
    let numerical_value = captured_str.parse::<i32>().unwrap();
    let ret = lower_bound <= numerical_value && numerical_value <= upper_bound;
    return ret;
}

fn is_valid_passport(rows_constituting_a_passport: &str, disable_value_checking: bool) -> bool {
    lazy_static! {
        static ref PASSPORT_GROUP_REGEX: Regex = Regex::new(r"(\w{3}):(.+?)(?:\s|$)").unwrap();
        static ref HEIGHT_REGEX: Regex = Regex::new(r"^(\d+)(cm|in)$").unwrap();
        static ref HAIR_COLOR_REGEX: Regex = Regex::new(r"^#[0123456789abcdef]{6}$").unwrap();
        static ref EYE_COLOR_REGEX: Regex =
            Regex::new(r"^amb$|^blu$|^brn$|^gry$|^grn$|^hzl$|^oth$").unwrap();
        static ref PID_REGEX: Regex = Regex::new(r"^\d{9}$").unwrap();
    }
    let mut contains_required_field: [bool; 7] = [false; 7];

    for capture in PASSPORT_GROUP_REGEX.captures_iter(rows_constituting_a_passport) {
        let key = capture.get(1).unwrap().as_str();
        let value = capture.get(2).unwrap().as_str();
        let satisfies_requirements = match key {
            "byr" => validate_number(value, 1920, 2002),
            "iyr" => validate_number(value, 2010, 2020),
            "eyr" => validate_number(value, 2020, 2030),
            "hgt" => {
                let maybe_height_capture = HEIGHT_REGEX.captures(value);
                match maybe_height_capture {
                    Some(height_capture) => {
                        let num_as_str = height_capture.get(1).unwrap().as_str();
                        let unit = height_capture.get(2).unwrap().as_str();
                        match unit {
                            "cm" => validate_number(num_as_str, 150, 193),
                            "in" => validate_number(num_as_str, 59, 76),
                            _ => false,
                        }
                    },
                    None => {
                        false
                    },
                }
            }
            "hcl" => HAIR_COLOR_REGEX.is_match(value),
            "ecl" => EYE_COLOR_REGEX.is_match(value),
            "pid" => PID_REGEX.is_match(value),
            _ => true,
        };

        for (i, required_key) in REQUIRED_FIELDS.iter().enumerate() {
            if *required_key == key.trim() {
                contains_required_field[i] = disable_value_checking || satisfies_requirements;
            }
        }
    }

    return contains_required_field.iter().all(|el| *el);
}

fn count_valid_passports(passports: &str, disable_value_checking: bool) -> i32 {
    let mut count: i32 = 0;
    let mut line_buffer: Vec<&str> = Vec::new();

    for line in passports.lines() {
        if line.trim().len() == 0 {
            let joined_passport = line_buffer.join(" ");
            let passport_str = joined_passport.as_str();
            count += if is_valid_passport(passport_str, disable_value_checking) {
                1
            } else {
                0
            };
            line_buffer.clear();
        } else {
            line_buffer.push(line);
        }
    }

    return count;
}

pub fn day4_main() {
    println!(
        "Part 1: Number of valid passports: {}",
        count_valid_passports(PASSPORTS, true)
    );
    println!(
        "Part 2: Number of valid passports: {}",
        count_valid_passports(PASSPORTS, false)
    );
}
