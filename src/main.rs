use std::io::{self, BufRead};

use reregex::parse;

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        match parse(&line.unwrap()) {
            Ok(p) => println!(" = {}\n = {:?}\n", p, p),
            Err(err) => println!("Error: {}", err),
        }
    }
}
