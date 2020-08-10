use std::{env, process::exit};

use reregex::matcher::build_graph;
use reregex::pattern::parse;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.len() != 1 {
        eprintln!("Error: Exactly one argument required");
        exit(1);
    }

    match parse(&args[0]) {
        Ok(p) => {
            let graph = build_graph(&p);
            println!("{}", graph.to_gw_dot());
        }
        Err(err) => {
            eprintln!("Error: {}", err);
            exit(2);
        }
    }
}
