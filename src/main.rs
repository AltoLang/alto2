#[macro_use]
extern crate pest_derive;

use std::{fs, env, io::{self, BufRead}};

mod syntax;
mod binding;

fn interactive() {
    for line in io::stdin().lock().lines() {
        let root = syntax::parser::parse_contents(line.unwrap());
        println!("parsed: {:?}", root);
    }
}

fn parse_file() {
    let path = "./src/main.ao";
    let code = fs::read_to_string(path)
        .expect("Cannot read file.")
        .replace('\n', ""); // sterilize file of newlines
    
    let root = syntax::parser::parse_contents(code).unwrap();
    let _bound = binding::binder::bind(root);
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