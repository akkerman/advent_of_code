use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let mut forward:i32=0;
    let mut depth:i32=0;
    let mut aim:i32=0;

    let file = File::open("./02-input.txt").unwrap();
    let reader = BufReader::new(file);
    for (_index, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        let mut iter = line.split_whitespace();
        let command = iter.next();
        let num = iter.next().map(|s| s.parse::<i32>());

        match (command, num) {
            (Some("down"), Some(Ok(x))) =>
                aim = aim + x,
            (Some("up"), Some(Ok(x)) ) => 
                aim = aim - x,
            (Some("forward"), Some(Ok(x))) => {
                forward = forward + x;
                depth = depth + (aim * x);
            },
            _ => (),
        }
    }

    println!("{}", forward * depth)
}

