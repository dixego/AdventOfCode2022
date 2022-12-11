use std::fs;

type Matrix<T> = Vec<Vec<T>>;

fn column<T: Copy>(m: &Matrix<T>, i: usize) -> Vec<T> {
    m.iter().map(|row| row[i]).collect()

}

fn row<T: Copy>(m: &Matrix<T>, i: usize) -> Vec<T> {
    m[i].clone()
}

fn count_visible(m: &Matrix<u32>) -> (usize, usize) {
    let mut count = 0;
    let mut max_score = 0;

    for r in 0..m.len() {
        let row = row(m, r);

        for c in 0..m[r].len() {
            if r == 0 || r == m.len()-1 || c == 0 || c == m[r].len()-1 {
                count += 1;
                continue;
            }

            let e = m[r][c];


            let left_row = &row[..c];    
            // visible to the left in the same row: reverse the slice then take elements until you
            // find one that's greater than the one you're at. visible to the right: same thing,
            // but don't reverse the slice. same goes for visible down.
            let left_row_visible = left_row.iter().copied().rev().take_while(|&c| c < e).collect::<Vec<u32>>();

            let right_row = &row[c+1..];
            let right_row_visible = right_row.iter().copied().take_while(|&c| c < e).collect::<Vec<u32>>();

            let col = column(m, c);
            let left_col = &col[..r];
            let left_col_visible = left_col.iter().copied().rev().take_while(|&c| c < e).collect::<Vec<u32>>();
            let right_col = &col[r+1..];
            let right_col_visible = right_col.iter().copied().take_while(|&c| c < e).collect::<Vec<u32>>();

            if left_row.len() == left_row_visible.len() ||
               right_row.len() == right_row_visible.len() ||
               left_col.len() == left_col_visible.len() ||
               right_col.len() == right_col_visible.len() {
                   count += 1;
            }

            let lr_score = left_row_visible.len() + if left_row_visible.len() < left_row.len() { 1 } else {0};
            let rr_score = right_row_visible.len() + if right_row_visible.len() < right_row.len() { 1 } else {0};
            let lc_score = left_col_visible.len() + if left_col_visible.len() < left_col.len() { 1 } else {0};
            let rc_score = right_col_visible.len() + if right_col_visible.len() < right_col.len() { 1 } else {0};

            let score = lr_score * rr_score * lc_score * rc_score;

            if score > max_score {
                max_score = score;
            }

        }
    }
    (count, max_score)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Wheres the file, Tony?");

    let m: Matrix<u32> = input.lines().map(|l| l.chars().map(|c| c.to_string().parse().unwrap()).collect()).collect();

    dbg!(count_visible(&m));


}
