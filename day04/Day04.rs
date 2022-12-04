use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    // ProTip: don't do this
    let rs: Vec<Vec<Vec<u32>>> = 
        input
        .lines()
        .map(|line| 
              line
              .split(",")
              .map(|r| 
                    r
                    .split("-")
                    .map(|n| n.parse::<u32>().unwrap())
                    .collect::<Vec<u32>>())
              .collect::<Vec<Vec<u32>>>())
        .collect::<Vec<_>>();

    let mut contained = 0;
    let mut overlapped = 0;

    for range in rs {
        let (r1,r2) = if let [r1, r2] = &range[..] {(r1,r2)} else {panic!("at the disco")} ;
        let (a1,b1) = if let [a1,b1] = &r1[..]{(a1,b1)} else {panic!()};
        let (a2,b2) = if let [a2,b2] = &r2[..]{(a2,b2)} else {panic!()};

        if (a1 >= a2 && b1 <= b2) ||
           (a2 >= a1 && b2 <= b1) {
           contained += 1;
        }
        if a1 <= b2 && b1 >= a2 {
            overlapped += 1;
        }
    }
    println!("part 1:{}",contained);
    println!("part 2:{}",overlapped);
}
