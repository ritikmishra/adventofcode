use regex::Regex;
use std::collections::HashMap;

const BAG_REGULATIONS: &str = include_str!("../../inputs/day7.txt");

#[derive(Debug)]
struct BagRule<'a> {
    bag_name: &'a str,
    max_qty: i32,
}

type BagRegulationCollection<'a> =  HashMap<&'a str, Vec<BagRule<'a>>>;

fn parse_regulation_rules(lines: &str) -> BagRegulationCollection {
    lazy_static! {
        /// matches the container bag. first capture group is key name, second is remaining string
        static ref BAG_KEY_REGEX: Regex = Regex::new(r"^([\s\S]+?) bags contain (.*)\.").unwrap();

        /// matches non-empty keys. first capture group is max qty of bags, second cap group is bag name
        static ref BAG_VALUE_REGEX: Regex = Regex::new(r"(\d) ([\s\S]+?) bags?").unwrap();
    }

    let mut map_from_shell_to_insides: BagRegulationCollection = HashMap::new();

    for line in lines.lines() {
        match BAG_KEY_REGEX.captures(line) {
            Some(cap_groups) => {
                let bag_key = cap_groups.get(1).unwrap().as_str();
                map_from_shell_to_insides.insert(bag_key, Vec::new());

                let remainder = cap_groups.get(2).unwrap().as_str();
                for allowed_content_type in BAG_VALUE_REGEX.captures_iter(remainder) {
                    let max_qty: i32 = allowed_content_type.get(1).unwrap().as_str().parse::<i32>().unwrap();
                    let bag_name: &str = allowed_content_type.get(2).unwrap().as_str();

                    map_from_shell_to_insides.get_mut(bag_key).unwrap().push(BagRule {bag_name, max_qty})
                }

            },
            None => ()
        };
    }

    return map_from_shell_to_insides;
}


fn bag_can_contain_other_bag(regulations: &BagRegulationCollection, inner_bag_key: &str, outer_bag_key: &str) -> bool {
    match regulations.get(outer_bag_key) {
        Some(inner_bag_rules) => {
            for inner_bag_rule in inner_bag_rules {
                if inner_bag_rule.bag_name == inner_bag_key {
                    return true;
                } else {
                    // Maybe this outer bag cannot directly contain the desired inner bag, but perhaps one of its 
                    // children can.
                    if bag_can_contain_other_bag(regulations, inner_bag_key, inner_bag_rule.bag_name) {
                        return true;
                    }
                }
            }
            false
        },
        None => false
    }
}

/// Count
fn count_outermost_bag_options(regulations: &BagRegulationCollection, bag_key: &str) -> i32 {
    regulations.keys().map(|outer_bag| bag_can_contain_other_bag(regulations, bag_key, outer_bag)).fold(0, |acc, el| {
        if el {
            return acc + 1;
        }
        return acc;
    })
}

fn count_number_of_required_nested_bags(regulations: &BagRegulationCollection, bag_key: &str) -> i32 {
    match regulations.get(bag_key) {
        Some(bag_rules) => {
            let mut ret = 0;
            for rule in bag_rules {
                ret += rule.max_qty * (1 + count_number_of_required_nested_bags(regulations, rule.bag_name));
            }
            ret
        },
        None => 0
    }
}

pub fn day7_main() {
    let regulations = parse_regulation_rules(BAG_REGULATIONS);
    println!("{:?}", regulations);
    let containable = bag_can_contain_other_bag(&regulations, "shiny gold", "bright white");
    println!("{:?}", containable);
    println!("{:?}", count_outermost_bag_options(&regulations, "shiny gold"));
    println!("{:?}", count_number_of_required_nested_bags(&regulations, "shiny gold"));
}