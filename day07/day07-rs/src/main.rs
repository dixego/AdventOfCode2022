use color_eyre;
use color_eyre::eyre::eyre;
use regex::Regex;
use std::fs;
use std::str::{FromStr};


#[derive(Debug)]
enum FsOutputLine {
    Ls,
    Cd(String),
    Dir(String),
    File(u64, String),
}

impl FromStr for FsOutputLine {
    type Err = color_eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cd_up = Regex::new(r"\$ cd \.\.").unwrap();
        let cd = Regex::new(r"\$ cd (.*)").unwrap();
        let ls = Regex::new(r"\$ ls$").unwrap();
        let dir = Regex::new(r"dir (.*)$").unwrap();
        let file = Regex::new(r"^(\d*) (.*)$").unwrap();

        if cd_up.is_match(s) {
            return Ok(Self::Cd("..".to_string()));
        } else if ls.is_match(s) {
            return Ok(Self::Ls);
        } else if let Some(caps) = cd.captures(s) {
            return Ok(Self::Cd(
                caps.get(1).ok_or(eyre!("Bad capture"))?.as_str().to_owned(),
            ));
        } else if let Some(caps) = dir.captures(s) {
            return Ok(Self::Dir(
                caps.get(1).ok_or(eyre!("Bad capture"))?.as_str().to_owned(),
            ));
        } else if let Some(caps) = file.captures(s) {
            return Ok(Self::File(
                caps.get(1).ok_or(eyre!("Bad capture"))?.as_str().parse()?,
                caps.get(2).ok_or(eyre!("Bad capture"))?.as_str().to_owned(),
            ));
        } else {
            return Err(eyre!("Bad line: {}", s));
        }
    }
}

#[derive(Debug)]
struct Entry {
    path: String,
    size: u64,
    children: Vec<Entry>,
}

impl Entry {
    fn build(mut self, lines: &mut dyn Iterator<Item = FsOutputLine>) -> Self {
        use FsOutputLine::*;

        while let Some(line) = lines.next() {
            match line {
                Cd(s) if s.as_str() == "/" => {}
                Cd(s) if s.as_str() == ".." =>  break,
                Cd(s) => {
                    let e = Entry { path: s.clone(), size: 0, children: vec![]}.build(lines);
                    self.size += e.size;
                    self.children.push(e);
                },
                File(size, path) => {
                    let e = Entry { path, size, children: vec![]};
                    self.size += e.size;
                    self.children.push(e);
                },
                _ => {}
            }
        }
        self
    }

    fn iter_dirs(&self) -> Box<dyn Iterator<Item = &Entry> + '_> {
        Box::new(
            std::iter::once(self).chain(
                self.children
                    .iter()
                    .filter(|c| !c.children.is_empty())
                    .flat_map(|c| c.iter_dirs()),
            ),
        )
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut out_lines = input
        .lines()
        .map(|line| line.parse::<FsOutputLine>().unwrap());
    let r = Entry {
        path: "/".to_string(),
        size: 0,
        children: vec![]
    }.build(&mut out_lines);

    let total_size = r.size;

    let space_needed = 30000000 - (70000000 - total_size);

    println!("{}", r.iter_dirs().map(|d| d.size).filter(|&s| s <100000).sum::<u64>());
    let smallest = r.iter_dirs().map(|d| d.size).filter(|&s| s > space_needed).min();
    println!("{:?}", smallest.unwrap());
        
        
}
