extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

use std::{io};
use pest::{Parser, pratt_parser::PrattParser, iterators::Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct AltoParser;

#[derive(Debug)]
pub enum Token <'a> {
    NumberToken(i32),
    StringToken(&'a str),
    IdentifierToken(&'a str),
    KeywordToken(Keyword),
    BinExpression {
        lhs: Box<Token<'a>>,
        op: Op,
        rhs: Box<Token<'a>>
    },
    AssignmentExpression {
        identifier: Box<Token<'a>>,
        expression: Box<Token<'a>>
    },
    CallExpression {
        identifier: Box<Token<'a>>,
        arguments: Box<Token<'a>>
    },
    DeclarationStatement {
        identifier: Box<Token<'a>>,
        expression: Box<Token<'a>>
    },
    FunctionArgumentsToken {
        args: Box<Vec<Token<'a>>>
    }
}

#[derive(Debug)]
pub enum Op {
    Addition,
    Subtract,
    Division,
    Multiplication
}

#[derive(Debug)]
pub enum Keyword {
    VarKeyword,
    IfKeyword
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

fn parse_assignment_expression(exp: pest::iterators::Pair<Rule>) -> Token {
    let mut subtokens = exp.into_inner();

    let identifier = match subtokens.nth(0) {
        Some(t) => { Token::IdentifierToken(t.as_str()) },
        None => unreachable!("Cannot find identifier when parsing assignment expression")
    };

    let expression = match subtokens.nth(1) {
        Some(t) => { parse( Pairs::single(t) ) },
        None => unreachable!("Cannot find identifier when parsing assignment expression")
    };

    return Token::AssignmentExpression { identifier: Box::new(identifier), expression: Box::new(expression) }
}

fn parse_declaration_statement(stmt: pest::iterators::Pair<Rule>) -> Token {
    let mut subtokens = stmt.into_inner();

    let identifier = match subtokens.nth(1) {
        Some(t) => { Token::IdentifierToken(t.as_str()) },
        None => unreachable!("Cannot find identifier when parsing declaration statement")
    };

    let expression = match subtokens.nth(1) {
        Some(t) => { parse( Pairs::single(t) ) },
        None => unreachable!("Cannot find expression when parsing declaration statement")
    };
    
    return Token::DeclarationStatement { identifier: Box::new(identifier), expression: Box::new(expression) }
}

fn parse_call_expression(exp: pest::iterators::Pair<Rule>) -> Token {
    let mut subtokens = exp.into_inner();

    let identifier = match subtokens.nth(0) {
        Some(t) => { Token::IdentifierToken(t.as_str()) },
        None => unreachable!("Cannot find identifier when parsing call expression")
    };

    let arguments = match subtokens.nth(0) {
        Some(t) => { parse( Pairs::single(t) ) },
        None => unreachable!("Cannot find arguments when parsing call expression")
    };

    return Token::CallExpression { identifier: Box::new(identifier), arguments: Box::new(arguments) }
}

fn parse_function_arguments(expr: pest::iterators::Pair<Rule>) -> Token {
    let subtokens = expr.into_inner();

    let mut expressions: Vec<Token> = Vec::new();
    subtokens.for_each(|token| {
        let parsed = parse( Pairs::single(token) );
        expressions.push(parsed);
    });

    return Token::FunctionArgumentsToken { args: Box::new(expressions) }
}

fn parse(pairs: Pairs<Rule>) -> Token {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number_token => { Token::NumberToken(primary.as_str().parse::<i32>().unwrap()) },
            Rule::string_token => { Token::StringToken(primary.as_str()) },
            Rule::identifier_token => { Token::IdentifierToken(primary.as_str()) }
            Rule::expression => { parse(primary.into_inner()) }
            Rule::expression_statement => { parse(primary.into_inner()) }
            Rule::var_keyword => { Token::KeywordToken(Keyword::VarKeyword) }
            Rule::declaration_statement => { parse_declaration_statement(primary) }
            Rule::assignment_expression => { parse_assignment_expression(primary) }
            Rule::call_expression => { parse_call_expression(primary) }
            Rule::function_args => { parse_function_arguments(primary) }
            rule => unreachable!("Token::parse expects an atom, found {:?}", rule)
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::addition => Op::Addition,
                Rule::subtraction => Op::Subtract,
                Rule::multiplication => Op::Multiplication,
                Rule::division => Op::Division,
                rule => unreachable!("Expected an infix operation, got '{:?}'", rule)
            };

            Token::BinExpression { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
        })
        .parse(pairs)
}

fn main() {
    for line in io::stdin().lines() {
        let ln = line.unwrap();
        match AltoParser::parse(Rule::statement, &ln) {
            Ok(mut pairs) => {
                println!(
                    "Parsed: {:#?}",
                    parse(
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