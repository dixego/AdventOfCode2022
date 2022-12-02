use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let mut match_results_1: Vec<u32> = vec![];
    let mut match_results_2: Vec<u32> = vec![];

    for line in input.lines() {
        let cols: Vec<&str> = line.split(' ').collect();

        let a = decode_col_A(cols[0]);
        let b = decode_col_B(cols[1]);
        let score = match_score(b, a);
        match_results_1.push(score);

        let b_2 = decode_col_B_2(cols[1]);
        let me = match_choice(a, b_2);
        let score_2 = match_score(me, a);
        match_results_2.push(score_2);
    }
    
    println!("Part 1: {}", match_results_1.iter().sum::<u32>());
    println!("Part 2: {}", match_results_2.iter().sum::<u32>());
}

#[derive(PartialEq, Clone, Copy)]
enum RPSChoice {
    Rock, Paper, Scissors
}

#[derive(Clone, Copy)]
enum MatchResult {
    Win, Draw, Lose
}


fn decode_col_A(s: &str) -> RPSChoice {
    match s {
        "A" => RPSChoice::Rock,
        "B" => RPSChoice::Paper,
        "C" => RPSChoice::Scissors,
        _ => panic!()
    }
}

fn decode_col_B(s: &str) -> RPSChoice {
    match s {
        "X" => RPSChoice::Rock,
        "Y" => RPSChoice::Paper,
        "Z" => RPSChoice::Scissors,
        _ => panic!()
    }
}

fn decode_col_B_2(s: &str) -> MatchResult {
    use MatchResult::*;
    match s {
        "X" => Lose,
        "Y" => Draw,
        "Z" => Win,
        _ => panic!()
    }
}

fn match_choice(against: RPSChoice, expected_result: MatchResult) -> RPSChoice {
    use RPSChoice::*;
    use MatchResult::*;

    match expected_result {
        Draw => against,
        Lose => match against {
            Rock => Scissors,
            Paper => Rock,
            Scissors => Paper
        }
        Win => match against {
            Rock => Paper,
            Paper => Scissors,
            Scissors => Rock
        }
    }
}

fn match_score(me: RPSChoice, against:RPSChoice) -> u32 {
    use RPSChoice::*;
    let choice_score = match me {
        Rock => 1,
        Paper => 2,
        Scissors => 3
    };

    let match_score = if me == against { 3 } else {
        match (me, against) {
            (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => 6,
            _ => 0
        }};

    return choice_score + match_score;
}
