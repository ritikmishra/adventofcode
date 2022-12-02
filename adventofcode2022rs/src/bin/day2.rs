const INPUT: &'static str = include_str!("../../../inputs2022/day2.txt");

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TheyPlay {
    Rock,
    Paper,
    Scissors,
}

impl TheyPlay {
    fn win(self) -> IPlay {
        match self {
            TheyPlay::Rock => IPlay::Paper,
            TheyPlay::Paper => IPlay::Scissors,
            TheyPlay::Scissors => IPlay::Rock,
        }
    }

    fn defeat(self) -> IPlay {
        match self {
            TheyPlay::Rock => IPlay::Scissors,
            TheyPlay::Paper => IPlay::Rock,
            TheyPlay::Scissors => IPlay::Paper,
        }
    }

    fn draw(self) -> IPlay {
        match self {
            TheyPlay::Rock => IPlay::Rock,
            TheyPlay::Paper => IPlay::Paper,
            TheyPlay::Scissors => IPlay::Scissors,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IPlay {
    Rock,
    Paper,
    Scissors,
}

impl IPlay {
    fn wins_against(self, them: TheyPlay) -> i32 {
        let win = match them {
            TheyPlay::Rock => self == IPlay::Paper,
            TheyPlay::Paper => self == IPlay::Scissors,
            TheyPlay::Scissors => self == IPlay::Rock,
        };

        let tie = match them {
            TheyPlay::Rock => self == IPlay::Rock,
            TheyPlay::Paper => self == IPlay::Paper,
            TheyPlay::Scissors => self == IPlay::Scissors,
        };

        if win {
            6
        } else if tie {
            3
        } else {
            0
        }
    }

    fn value(self) -> i32 {
        match self {
            IPlay::Rock => 1,
            IPlay::Paper => 2,
            IPlay::Scissors => 3,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DesiredOutcome {
    Lose,
    Draw,
    Win
}

impl DesiredOutcome {
    fn help(self, they_played: TheyPlay) -> IPlay {
        match self {
            DesiredOutcome::Lose => they_played.defeat(),
            DesiredOutcome::Draw => they_played.draw(),
            DesiredOutcome::Win => they_played.win(),
        }
    }
}

fn day2(input: &str) {
    let things: Vec<(TheyPlay, IPlay, DesiredOutcome)> = input
        .split("\n")
        .map(|line| {
            let (them, us) = line.split_at(1);
            let they_play = match them {
                "A" => TheyPlay::Rock,
                "B" => TheyPlay::Paper,
                "C" => TheyPlay::Scissors,
                _ => panic!("they played a {them:?}"),
            };

            let (i_play, desired_outcome) = match us {
                " X" => (IPlay::Rock, DesiredOutcome::Lose),
                " Y" => (IPlay::Paper, DesiredOutcome::Draw),
                " Z" => (IPlay::Scissors, DesiredOutcome::Win),
                _ => panic!("i played a {us:?}"),
            };

            (they_play, i_play, desired_outcome)
        })
        .collect();

    let part1_score = things.iter().map(|(they_play, i_play, _)| {
        i_play.wins_against(*they_play) + i_play.value()
    }).sum::<i32>();

    println!("Part 1 score: {part1_score:?}");

    let part2_score = things.iter().map(|(they_play, _, desired_outcome)| {
        let i_play = desired_outcome.help(*they_play);
        i_play.wins_against(*they_play) + i_play.value()
    }).sum::<i32>();

    println!("Part 2 score: {part2_score:?}");


}

fn main() {
    day2(INPUT);
}
