#[macro_use]
extern crate pest_derive;

use std::{fs, env, io::{self, BufRead}};

mod syntax;

fn interactive() {
    for line in io::stdin().lock().lines() {
        syntax::parser::parse_contents(line.unwrap())
    }
}

fn parse_file() {
    let path = "./src/main.ao";
    let code = fs::read_to_string(path)
        .expect("Cannot read file.")
        .replace('\n', "");

    // also sterilize the file of newlines
    
    syntax::parser::parse_contents(code);
}

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        parse_file();
    } else if &args[1] == "--interactive" || &args[1] == "-i" {
        interactive();
    } else {
        parse_file();
    }
}