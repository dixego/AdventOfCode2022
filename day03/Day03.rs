use std::collections::HashSet;
use std::collections::hash_map::RandomState;
use std::iter::FromIterator;
use std::fs;

// Couldn't get HashSet to work without this.
type S = RandomState;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    println!("{}", part_one(&input));
    println!("{}", part_two(&input));

}

// Split line in half, obtain set of chars of each half,
// obtain intersection of sets, calculate priority and sum
// all priorities.
fn part_one(input: &String) -> u32 {
    input.lines().map(|line| {
              let (comp_a, comp_b) = line.split_at(line.len()/2);
              let set_a: HashSet<char, S> = HashSet::from_iter(comp_a.to_string().chars());
              let set_b: HashSet<char, S> = HashSet::from_iter(comp_b.to_string().chars());
              let inter = set_a.intersection(&set_b).collect::<Vec<&char>>();
              priority(**inter.get(0).unwrap())
    }).sum()

}

// Group lines in threes, calculate the intersection of 
// chars of each group, calculate priority of intersection,
// sum priorities.
fn part_two(input: &String) -> u32 {
    input.lines().collect::<Vec<&str>>().chunks(3)
        .map(|group| {
            if let [a,b,c] = group {
                let set_a: HashSet<char, S> = HashSet::from_iter(a.to_string().chars());
                let set_b: HashSet<char, S> = HashSet::from_iter(b.to_string().chars());
                let set_c: HashSet<char, S> = HashSet::from_iter(c.to_string().chars());

                let i_ab: HashSet<char, S> = set_a.intersection(&set_b).map(|c| *c).collect();
                let badge_priority = i_ab.intersection(&set_c).map(|c| priority(*c)).sum();
                badge_priority
            } else {
                0
            }
        }).sum()
}

// Hacky way to get priority of char: ASCII codes.
fn priority(c: char) -> u32 {
    let n = c as u32;
    if n >= 65 && n <= 90 {
        n - 65 + 27
    } else {
        n - 97 + 1
    }
}
