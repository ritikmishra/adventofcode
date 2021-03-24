use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use itertools::join;

const INGREDIENTS_LIST: &str = include_str!("../inputs/day21.txt");

pub fn day21_main() {
    let ingredients_line_regex: Regex = Regex::new(r"^(.*) \(contains (.*)\)$").unwrap();
    let mut allergen_to_possible_ingredients_map: HashMap<&str, HashSet<&str>> = HashMap::new();

    let ingredients = INGREDIENTS_LIST.trim_end();

    let mut ingredient_to_appearences_map: HashMap<&str, u32> = HashMap::new();
    let mut all_ingredients: HashSet<&str> = HashSet::new();
    for line in ingredients.split("\n") {
        let captures = ingredients_line_regex.captures(line).unwrap();
        let ingredients: HashSet<&str> = captures.get(1).unwrap().as_str().split(" ").collect();
        let allergens: HashSet<&str> = captures.get(2).unwrap().as_str().split(", ").collect();

        for ingredient in ingredients.iter() {
            match ingredient_to_appearences_map.get_mut(ingredient) {
                Some(val) => {
                    *val += 1;
                }
                None => {
                    ingredient_to_appearences_map.insert(ingredient, 1);
                }
            }
        }
        all_ingredients.extend(ingredients.clone());
        for allergen in allergens {
            match allergen_to_possible_ingredients_map.get_mut(allergen) {
                Some(possible_ingredients_set) => {
                    *possible_ingredients_set = possible_ingredients_set
                        .intersection(&ingredients)
                        .cloned()
                        .collect();
                }
                None => {
                    allergen_to_possible_ingredients_map.insert(allergen, ingredients.clone());
                }
            }
        }
    }

    println!("{:#?}", allergen_to_possible_ingredients_map);
    let mut non_allergenic_ingredients = all_ingredients.clone();
    for potentially_allergenic_ingredients in allergen_to_possible_ingredients_map.values() {
        non_allergenic_ingredients = non_allergenic_ingredients
            .difference(potentially_allergenic_ingredients)
            .cloned()
            .collect();
    }

    let mut non_allergenic_appearances = 0;
    for non_allergen in non_allergenic_ingredients.iter() {
        non_allergenic_appearances += ingredient_to_appearences_map.get(non_allergen).unwrap();
    }
    println!(
        "part 1: there are {} guaranteed non-allergenic ingredients",
        non_allergenic_ingredients.len()
    );
    println!("they show up {} times\n", non_allergenic_appearances);

    // part 2  
    let mut confirmed_allergen_ingredients: HashMap<&str, &str> = HashMap::new();
    loop {
        // find the smallest allergen set
        match allergen_to_possible_ingredients_map
            .iter()
            .map(|(a, b)| (*a, b))
            .min_by_key(|(_name, set)| set.len())
        {
            Some(allergen_possible_ingredients_set_pair) => {
                let allergen = allergen_possible_ingredients_set_pair.0.clone();
                if allergen_possible_ingredients_set_pair.1.len() == 1 {
                    // we know what this allergen is => we can remove it from all of the other sets
                    let allergenic_ingredient: &str = allergen_possible_ingredients_set_pair
                        .1
                        .iter()
                        .next()
                        .unwrap()
                        .clone();
                    confirmed_allergen_ingredients.insert(allergen, allergenic_ingredient);

                    for (_allergen, maybe_allergenic_ingredients_set) in
                        allergen_to_possible_ingredients_map.iter_mut()
                    {
                        maybe_allergenic_ingredients_set.remove(allergenic_ingredient);
                    }
                    allergen_to_possible_ingredients_map.remove(allergen);
                }
            },
            None => break
        }
    }

    let mut allergen_ingredient_pairs: Vec<(&str, &str)> = confirmed_allergen_ingredients.clone().into_iter().collect();
    allergen_ingredient_pairs.sort_by_key(|(allergen, _ingredient)| allergen.clone());

    let ingredients_sorted_by_allergen = join(allergen_ingredient_pairs.into_iter().map(|(_allergen, ingredient)| ingredient), ",");
    println!("part 2: {}", ingredients_sorted_by_allergen);
    
}
