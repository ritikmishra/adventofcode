use std::collections::HashMap;
use std::collections::HashSet;
const GROUP_Q_ANSWERS: &str = include_str!("../inputs/day6.txt");

/// Count the number of unique questions that were answered
/// by the people in this group
fn parse_group_answers_anyone_yes(group: &str) -> i32 {
    let mut questions_answered = HashSet::new();
    for person_answers in group.split("\n") {
        for q in person_answers.chars() {
            questions_answered.insert(q);
        }
    }
    return questions_answered.len() as i32;
}

fn parse_all_answers(input: &str, method: &dyn Fn(&str) -> i32) -> i32 {
    let mut ret = 0;
    for group in input.split("\n\n") {
        ret += method(group);
    }
    return ret;
}

fn parse_group_answers_all_yes(group: &str) -> i32 {
    let mut map_of_q_count: HashMap<char, i32> = HashMap::new();
    let mut num_people = 0;
    for person_answers in group.split("\n") {
        let mut real_person = false;
        for q_answered in person_answers.chars() {
            if q_answered.is_whitespace() {
                continue;
            }
            let existing_map_value = map_of_q_count.get_mut(&q_answered);
            match existing_map_value {
                Some(val) => {
                    *val += 1;
                }
                None => {
                    map_of_q_count.insert(q_answered, 1);
                }
            };
            real_person = true;
        }
        if real_person {
            num_people += 1
        }
    }
    map_of_q_count
        .iter()
        .filter(|(k, v)| !k.is_whitespace() && **v == num_people)
        .collect::<Vec<(&char, &i32)>>()
        .len() as i32
}
pub fn day6_main() {
    println!(
        "part 1 q count: {}",
        parse_all_answers(GROUP_Q_ANSWERS, &parse_group_answers_anyone_yes)
    );
    println!(
        "part 2 q count: {}",
        parse_group_answers_all_yes(
            "zpyjmxrfdtwuhsblav
    vtdjpxsylwbramuf
    urwsdbxamylfptjv
    uaysdwjrvmptxlbf
    vpmsudtajlrwfxyb
    "
        )
    );
    println!(
        "part 2 q count: {}",
        parse_all_answers(GROUP_Q_ANSWERS, &parse_group_answers_all_yes)
    );
}
