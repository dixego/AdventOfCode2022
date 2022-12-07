use std::fs;
use std::collections::HashSet;
use std::collections::hash_map::RandomState;
use std::iter::FromIterator;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().chars().collect::<Vec<_>>();

    println!("{}", start_of_packet(&input));
    println!("{}", start_of_message(&input));
}

fn start_of_packet(chars: &Vec<char>) -> usize {
    find_consecutive_distinct(chars, 4).unwrap()
}

fn start_of_message(chars: &Vec<char>)  -> usize {
    find_consecutive_distinct(chars, 14).unwrap()
}

fn find_consecutive_distinct(chars: &Vec<char>, n: usize) -> Option<usize> {
    let mut i:usize = 0;

    while i+n-1 < chars.len() {
        let letters = &chars[i..=i+n-1];
        let set: HashSet<char, RandomState> = HashSet::from_iter(letters.iter().copied());
        if set.len() == n {
            return Some(i+n);
        }
        i +=1;
    }
    None
}
