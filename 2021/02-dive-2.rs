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
        let parts = line.split_once(" ").map(|(s,i)| (s, i.parse::<i32>()));

        if let Some((command, Ok(x))) = parts {
            match command {
                "down" =>
                    aim = aim + x,
                "up" => 
                    aim = aim - x,
                "forward"=> {
                    forward = forward + x;
                    depth = depth + (aim * x);
                },
                _ => (),
            }
        }
    }

    println!("{}", forward * depth)
}

