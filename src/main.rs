extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

use std::io;
use pest::{Parser, pratt_parser::PrattParser, iterators::Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct AltoParser;

#[derive(Debug)]
pub enum Expr {
    NumberToken(i32),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>
    }
}

#[derive(Debug)]
pub enum Op {
    Addition,
    Subtract,
    Division,
    Multiplication
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // the `op` call order determines operation precedences
        PrattParser::new()
            .op(Op::infix(addition, Left) | Op::infix(subtraction, Left))
            .op(Op::infix(division, Left) | Op::infix(multiplication, Left))
    };
}

pub fn parse_expression(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number_token => { println!("prim: {:?}", primary.as_str()); Expr::NumberToken(primary.as_str().parse::<i32>().unwrap()) },
            rule => unreachable!("Expr::parse expects an atom, found {:?}", rule)
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::addition => Op::Addition,
                Rule::subtraction => Op::Subtract,
                Rule::multiplication => Op::Multiplication,
                Rule::division => Op::Division,
                rule => unreachable!("Expected an infix operation, got '{:?}'", rule)
            };
            Expr::BinOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
        })
        .parse(pairs)
}

fn main() {
    for line in io::stdin().lines() {
        let ln = line.unwrap();
        match AltoParser::parse(Rule::expression, &ln) {
            Ok(mut pairs) => {
                println!(
                    "Parsed: {:#?}",
                    parse_expression(
                        pairs.next().unwrap().into_inner()
                    )
                );
            }
            Err(e) => {
                eprintln!("Parse failed: {:?}", e)
            }
        }
    }
}