use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Mutex;

use itertools::Itertools;

fn parse_deck(input: &str) -> VecDeque<u32> {
    return input
        .split("\n")
        .map(|num_as_str| num_as_str.parse::<u32>().unwrap())
        .collect();
}

#[derive(Debug, Copy, Clone)]
enum Player {
    P1,
    P2,
}

impl Player {
    fn other(&self) -> Player {
        match self {
            Player::P1 => Player::P2,
            Player::P2 => Player::P1,
        }
    }
}

fn play_until_winner_nonrecursive(
    p1_deck: &mut VecDeque<u32>,
    p2_deck: &mut VecDeque<u32>,
) -> Player {
    loop {
        match (p1_deck.pop_front(), p2_deck.pop_front()) {
            (Some(p1_card), Some(p2_card)) => {
                if p1_card > p2_card {
                    p1_deck.push_back(p1_card);
                    p1_deck.push_back(p2_card);
                } else if p2_card > p1_card {
                    p2_deck.push_back(p2_card);
                    p2_deck.push_back(p1_card);
                } else {
                    panic!("cards of equal value held by both players!")
                }
            }
            (Some(card), None) => {
                p1_deck.push_front(card);
                break Player::P1;
            }
            (None, Some(card)) => {
                p2_deck.push_front(card);
                break Player::P2;
            }
            (None, None) => panic!("somehow both players ran out of cards?"),
        }
    }
}

fn hash_player_deck(p1_deck: VecDeque<u32>) -> u64 {
    let mut hasher = DefaultHasher::new();
    for tuple in p1_deck.into_iter().enumerate().map(|(x, y)| (x, y)) {
        tuple.hash(&mut hasher)
    }
    return hasher.finish();
}

fn play_until_winner_recursive(p1_deck: &mut VecDeque<u32>, p2_deck: &mut VecDeque<u32>) -> Player {
    let mut seen_hashes: HashSet<(u64, u64)> = HashSet::new();

    let winner = loop {
        // println!("Player 1's deck: {:?}", p1_deck);
        // println!("Player 2's deck: {:?}", p2_deck);
        let maybe_done = match (p1_deck.len(), p2_deck.len()) {
            (0, 0) => panic!("somehow both decks are empty!"),
            (0, _) => Some(Player::P2),
            (_, 0) => Some(Player::P1),
            _ => {
                match (p1_deck.front(), p2_deck.front()) {
                    (Some(p1_card), Some(p2_card)) => {
                        let lookup_key = (
                            hash_player_deck(p1_deck.clone()),
                            hash_player_deck(p2_deck.clone()),
                        );
                        // println!("Player 1 plays: {}\nPlayer2 plays: {}", p1_card, p2_card);
                        let subgame_winner: Player;
                        if seen_hashes.contains(&lookup_key) {
                            return Player::P1;
                        } else if *p1_card < p1_deck.len() as u32
                            && *p2_card < p2_deck.len() as u32
                        {
                            // println!("Playing a subgame to determine the winner\n\n");
                            subgame_winner = play_until_winner_recursive(
                                &mut p1_deck
                                    .iter()
                                    .cloned()
                                    .skip(1)
                                    .take(*p1_card as usize)
                                    .collect(),
                                &mut p2_deck
                                    .iter()
                                    .cloned()
                                    .skip(1)
                                    .take(*p2_card as usize)
                                    .collect(),
                            );
                            // println!("\n\nsubgame over!");
                        } else {
                            if p1_card > p2_card {
                                // println!("No subgame, Player 1 plays a higher value card");
                                subgame_winner = Player::P1;
                            } else if p2_card > p1_card {
                                // println!("No subgame, Player 2 plays a higher value card");
                                subgame_winner = Player::P2;
                            } else {
                                panic!("cards of equal value held by both players!")
                            }
                        }

                        seen_hashes.insert(lookup_key);

                        let p1_front = p1_deck.pop_front().unwrap();
                        let p2_front = p2_deck.pop_front().unwrap();
                        match subgame_winner {
                            Player::P1 => {
                                // println!("player 1 wins round!\n\n");
                                p1_deck.push_back(p1_front);
                                p1_deck.push_back(p2_front);
                            }
                            Player::P2 => {
                                // println!("player 2 wins round!\n\n");
                                p2_deck.push_back(p2_front);
                                p2_deck.push_back(p1_front);
                            }
                        }
                        None
                    }
                    _ => panic!("how did this happen?"),
                }
            }
        };

        match maybe_done {
            Some(winner) => {
                break winner;
            }
            None => {}
        };
    };

    winner
}

fn calculate_score(deck: &VecDeque<u32>) -> u32 {
    return deck
        .iter()
        .cloned()
        .rev()
        .enumerate()
        .map(|(i, card_val)| ((i as u32) + 1) * card_val)
        .sum();
}

pub fn day22_main() {
    let INPUT = include_str!("../../inputs/day22.txt");
    let (player1, player2) = INPUT.trim_end().split("\n\n").collect_tuple().unwrap();

    let replace = player1.replace("Player 1:", "");
    let p1_starting_cards = replace.trim();
    let replace = player2.replace("Player 2:", "");
    let p2_starting_cards = replace.trim();
    // part 1
    let mut p1_deck = parse_deck(p1_starting_cards);
    let mut p2_deck = parse_deck(p2_starting_cards);

    println!("Part 1: ");
    println!(
        "{:#?}",
        play_until_winner_nonrecursive(&mut p1_deck, &mut p2_deck)
    );
    println!("p1 score: {}", calculate_score(&p1_deck));
    println!("p2 score: {}", calculate_score(&p2_deck));
    println!("\n\n");
    // part 2
    let mut p1_deck = parse_deck(
        "43
19",
    );
    let mut p2_deck = parse_deck(
        "2
29
14",
    );

    let mut p1_deck = parse_deck(p1_starting_cards);
    let mut p2_deck = parse_deck(p2_starting_cards);

    //     let mut p1_deck = parse_deck(
    //         "9
    // 2
    // 6
    // 3
    // 1");
    //     let mut p2_deck = parse_deck(
    //         "5
    // 8
    // 4
    // 7
    // 10");

    println!("Part 2:");
    println!(
        "\n\n\nWinner: {:#?}",
        play_until_winner_recursive(&mut p1_deck, &mut p2_deck)
    );
    println!("p1 score: {}", calculate_score(&p1_deck));
    println!("p2 score: {}", calculate_score(&p2_deck));
    println!("p1 deck: {:?}", p1_deck);
    println!("p2 deck: {:?}", p2_deck);
}
