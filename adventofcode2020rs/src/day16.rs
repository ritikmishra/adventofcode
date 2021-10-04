use itertools::Itertools;
use regex::Regex;
use std::collections::hash_map::RandomState;
use std::collections::hash_set::Difference;
use std::collections::HashMap;
use std::collections::HashSet;
use std::str::FromStr;

const INPUT: &str = include_str!("../../inputs/day16.txt");

#[derive(Debug, Clone)]
struct Range {
    lower_bound: u32,
    upper_bound: u32,
}

impl Range {
    pub fn in_range(&self, num: u32) -> bool {
        return self.lower_bound <= num && num <= self.upper_bound;
    }
}

#[derive(Debug, Clone)]
struct TicketField {
    field_name: String,
    valid_ranges: [Range; 2],
}

impl FromStr for TicketField {
    type Err = u32;

    fn from_str(field_row: &str) -> std::result::Result<Self, Self::Err> {
        let field_regex: Regex = Regex::new(r"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)").unwrap();

        match field_regex.captures(field_row) {
            None => {
                return Err(0);
            }
            Some(captures) => {
                let name = String::from(captures.get(1).unwrap().as_str());
                let lower_bound_1 = captures.get(2).unwrap().as_str().parse::<u32>().unwrap();
                let upper_bound_1 = captures.get(3).unwrap().as_str().parse::<u32>().unwrap();
                let lower_bound_2 = captures.get(4).unwrap().as_str().parse::<u32>().unwrap();
                let upper_bound_2 = captures.get(5).unwrap().as_str().parse::<u32>().unwrap();

                return Ok(TicketField {
                    field_name: name,
                    valid_ranges: [
                        Range {
                            lower_bound: lower_bound_1,
                            upper_bound: upper_bound_1,
                        },
                        Range {
                            lower_bound: lower_bound_2,
                            upper_bound: upper_bound_2,
                        },
                    ],
                });
            }
        }
    }
}

impl TicketField {
    pub fn num_within_range(&self, num: u32) -> bool {
        return self
            .valid_ranges
            .iter()
            .map(|range| range.in_range(num))
            .any(|x| x);
    }
}

pub fn day16_main() {
    let (REQUIRED_FIELDS, MY_TICKET, NEARBY_TICKETS): (&str, &str, &str) = INPUT.trim_end().split("\n\n").collect_tuple().unwrap();
    
    // remove the header from the my ticket and nearby ticket spots
    let replace = MY_TICKET.replace("your ticket:", "");
    let MY_TICKET = replace.trim();
    
    let replace = NEARBY_TICKETS.replace("nearby tickets:", "");
    let NEARBY_TICKETS = replace.trim();


    // Part 1
    let fields: Vec<TicketField> = REQUIRED_FIELDS
        .split("\n")
        .into_iter()
        .map(|row| TicketField::from_str(row).unwrap())
        .collect();

    println!("{:?}", fields.get(0));

    let mut nearby_tickets: Vec<Vec<u32>> = NEARBY_TICKETS
        .split("\n")
        .map(|ticket_row| {
            ticket_row
                .split(",")
                .into_iter()
                .map(|numstr| numstr.parse::<u32>().unwrap())
                .collect()
        })
        .collect();

    let mut sum = 0;
    let mut invalid_ticket_indexes: Vec<usize> = Vec::new();
    for (i, ticket) in nearby_tickets.iter().enumerate() {
        'ticket_val_loop: for value in ticket.iter() {
            let is_valid = fields.iter().any(|field| field.num_within_range(*value));
            if !is_valid {
                sum += value;
                invalid_ticket_indexes.push(i);
                break 'ticket_val_loop;
            }
        }
    }
    println!("sum of invalid fields: {}", sum);

    // part 2
    // discard invalid tickets
    for idx in invalid_ticket_indexes.iter().rev() {
        nearby_tickets.remove(*idx);
    }

    // map key is index in fields list
    let mut field_to_possible_ticket_indexes_map: HashMap<usize, HashSet<usize>> = HashMap::new();
    let initial_set_value: HashSet<usize> = (0..fields.len()).collect();

    // fill out field_to_possible_ticket_indexes_map with default value, which is each field could be any of the fields in the ticket
    for (i, _) in fields.iter().enumerate() {
        field_to_possible_ticket_indexes_map.insert(i, initial_set_value.iter().cloned().collect());
    }


    for ticket in nearby_tickets.iter() {
        // if the ith field on some ticket is not valid for the field at field_idx,
        // remove it as being possible in the map
        for (i, value) in ticket.iter().enumerate() {
            for (field_idx, field) in fields.iter().enumerate() {
                if !field.num_within_range(*value) {
                    field_to_possible_ticket_indexes_map
                        .get_mut(&field_idx)
                        .unwrap()
                        .remove(&i);
                }
            }
        }
    }

    let mut sets_sorted_by_length: Vec<(usize, HashSet<usize>)> = field_to_possible_ticket_indexes_map
        .iter()
        .map(|(a, b)| (*a, b.iter().cloned().collect()))
        .collect();

    sets_sorted_by_length
        .sort_by_key(|(_, set_of_possible_ticket_idxes)| set_of_possible_ticket_idxes.len());

    // set of ticket field indexes that have been assigned a field
    let mut taken_ticket_idxes: HashSet<usize> = HashSet::new();

    for (_, set) in sets_sorted_by_length.iter_mut() {
        // the field could not correspond to any of the ticket fields that have already been assigned a field
        *set = set.difference(&taken_ticket_idxes).cloned().collect();
        if set.len() == 1 {
            // if there's only one option we are going to claim the ticket index for the field
            taken_ticket_idxes.insert(*set.iter().next().unwrap());
        }
    }

    // create our definitive mapping from field index to ticket index
    let mut definitive_field_ticket_idx_mapping: HashMap<usize, usize> = HashMap::new();
    for (field_idx, set_of_ticket_idxes) in sets_sorted_by_length.iter() {
        definitive_field_ticket_idx_mapping.insert(*field_idx, *set_of_ticket_idxes.iter().next().unwrap());
    }

    let fields_that_start_with_departure: Vec<(usize, &TicketField)> = fields.iter().enumerate().filter(|(idx, field)| field.field_name.starts_with("departure")).collect();
    let mut result: u64 = 1;

    let my_ticket_num: Vec<u64> = MY_TICKET.split(",").into_iter().map(|numstr| numstr.parse::<u64>().unwrap()).collect();

    for (field_idx, field) in fields_that_start_with_departure {
        let ticket_value_idx = definitive_field_ticket_idx_mapping.get(&field_idx).unwrap();
        result *= my_ticket_num.get(*ticket_value_idx).unwrap();
    }

    println!("done, the product of departure fields is {}", result);
    println!("{:?}", definitive_field_ticket_idx_mapping);
}
