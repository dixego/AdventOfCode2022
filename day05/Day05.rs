use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let mut stacks: Vec<Vec<char>> = vec![];
    for _ in 0..9 {
        stacks.push(vec![]);
    }

    let mut instructions: Vec<(usize,usize,usize)> = vec![];

    let mut rows = vec![];
    for line in input.lines() {
        if !line.starts_with("[") {
            if line == "\n" || line.starts_with(" ") { continue; }
            if let [a,b,c] = line.split(' ')
                .enumerate()
                .filter(|(i, _)| i % 2 == 1)
                .map(|(_, s)| s.parse::<usize>().unwrap())
                .collect::<Vec<_>>()[..] {
                    instructions.push((a,b,c));
            }
            continue;
        }

        let chars = line.chars()
            .collect::<Vec<_>>();
        let row = chars.chunks(4)
            .flat_map(|chunk| 
                       chunk.iter()
                            .enumerate()
                            .filter(|(i, _t)| *i == 1)
                            .collect::<Vec<_>>())
            .map(|(_, t)| *t)
            .collect::<Vec<_>>();

        rows.push(row);
    }
    rows.reverse();
    for row in rows.iter() {
        for (i, c) in row.iter().enumerate() {
            if *c != ' ' {
                stacks[i].push(*c);
            }
        }
    }
    let mut stacks2 = stacks.clone();

    for (items, stack1, stack2) in instructions.iter().by_ref() {
        for _ in 0..*items {
            let item = stacks[stack1-1].pop().unwrap();
            stacks[stack2-1].push(item);
        }
    }
    for stack in stacks {
        println!("{}", stack.last().unwrap());
    }

    println!("----");

    for (items, stack1, stack2) in instructions {
        let mut moved = vec![];
        for _ in 0..items {
            moved.push(stacks2[stack1-1].pop().unwrap());
        }
        moved.reverse();
        stacks2[stack2-1].append(&mut moved);

    }
    for stack in stacks2 {
        println!("{}", stack.last().unwrap());
    }
}
