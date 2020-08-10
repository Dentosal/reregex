use std::io::{self, BufRead};
use std::{env, process::exit};

use reregex::matcher::match_find;
use reregex::pattern::parse;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.len() != 1 {
        eprintln!("Error: Exactly one argument required");
        exit(1);
    }

    let pattern = match parse(&args[0]) {
        Ok(p) => p,
        Err(err) => {
            eprintln!("Error: {}", err);
            exit(2);
        }
    };

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let text = line.unwrap();
        if let Some((s, l)) = match_find(&pattern, &text) {
            println!("{}", text.chars().skip(s).take(l).collect::<String>());
        }
    }
}
