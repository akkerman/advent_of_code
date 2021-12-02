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
        let command = iter.next().unwrap();
        let x = iter.next().unwrap();
        let x = x.parse::<i32>().unwrap();

        if command == "down" {
            aim = aim + x;
        } else if command == "up" {
            aim = aim - x;
        } else if command == "forward" {
            forward = forward + x;
            depth = depth + (aim * x);
        }
    }

    println!("{}", forward * depth)
}

