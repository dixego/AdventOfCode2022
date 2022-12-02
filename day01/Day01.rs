use std::fs;

fn main() {
    let input: String = fs::read_to_string("input.txt").unwrap();
    let lines = 
        input
        .lines()
        .collect::<Vec<&str>>();
    let mut groups = 
        lines
        .split(|l| *l == "")
        .map(|g| g.iter().map(|s| s.parse::<u32>().unwrap_or(0)).sum::<u32>())
        .collect::<Vec<u32>>();
    groups.sort();
    let top = &groups[groups.len()-3..];

    println!("Part 1: {:?}", top.last().unwrap());
    println!("Part 2: {:?}", top.iter().sum::<u32>());

}
