#[macro_use]
extern crate pest_derive;

mod binding;
mod eval;
mod syntax;

use std::{
    env, fs,
    io::{self, BufRead},
};

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
    dbg!(&root);

    let bound = binding::binder::bind_global_scope(root);
    dbg!(&bound);

    // evaluate the file
    eval::evaluator::eval_root(bound);
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
