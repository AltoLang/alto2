extern crate lazy_static;
extern crate pest;

use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Parser,
};

#[derive(Parser)]
#[grammar = "syntax/grammar.pest"]
struct AltoParser;

#[derive(Debug)]
pub enum ParseError {
    Failed,
}

#[derive(Debug)]
pub enum SyntaxToken {
    NumberToken(i32),
    StringToken(String),
    IdentifierToken(String),
    KeywordToken(Keyword),
    BinExpression {
        lhs: Box<SyntaxToken>,
        op: Op,
        rhs: Box<SyntaxToken>,
    },
    ReferenceExpression {
        identifier: Box<SyntaxToken>,
    },
    AssignmentExpression {
        identifier: Box<SyntaxToken>,
        expression: Box<SyntaxToken>,
    },
    CallExpression {
        identifier: Box<SyntaxToken>,
        arguments: Box<SyntaxToken>,
    },
    DeclarationStatement {
        identifier: Box<SyntaxToken>,
        expression: Box<SyntaxToken>,
    },
    FunctionArgumentsToken {
        args: Box<Vec<SyntaxToken>>,
    },
    FunctionDeclarationExpression {
        identifier: Box<SyntaxToken>,
        parameters: Box<Vec<SyntaxToken>>,
        code_block: Box<SyntaxToken>,
    },
    FunctionParameter {
        name: Box<SyntaxToken>,
        type_annotation: Box<SyntaxToken>,
    },
    CodeBlockStatement {
        tokens: Box<Vec<SyntaxToken>>,
    },
    Module {
        tokens: Box<Vec<SyntaxToken>>,
    },
}

#[derive(Debug)]
pub enum Op {
    Addition,
    Subtract,
    Division,
    Multiplication,
}

#[derive(Debug)]
pub enum Keyword {
    VarKeyword,
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

fn parse_reference_expression(exp: Pair<Rule>) -> SyntaxToken {
    let mut subtokens = exp.into_inner();
    let identifier = match subtokens.nth(0) {
        Some(t) => SyntaxToken::IdentifierToken(String::from(t.as_str())),
        None => unreachable!("Cannot find identifier when parsing reference expression"),
    };

    if subtokens.count() > 0 {
        panic!("Reference expression cannot have more than 1 subtoken");
    }

    SyntaxToken::ReferenceExpression {
        identifier: Box::new(identifier),
    }
}

fn parse_assignment_expression(exp: Pair<Rule>) -> SyntaxToken {
    let mut subtokens = exp.into_inner();

    let identifier = match subtokens.nth(0) {
        Some(t) => SyntaxToken::IdentifierToken(String::from(t.as_str())),
        None => unreachable!("Cannot find identifier when parsing assignment expression"),
    };

    let expression = match subtokens.nth(1) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find identifier when parsing assignment expression"),
    };

    SyntaxToken::AssignmentExpression {
        identifier: Box::new(identifier),
        expression: Box::new(expression),
    }
}

fn parse_declaration_statement(stmt: Pair<Rule>) -> SyntaxToken {
    let mut subtokens = stmt.into_inner();

    let identifier = match subtokens.nth(1) {
        Some(t) => SyntaxToken::IdentifierToken(String::from(t.as_str())),
        None => unreachable!("Cannot find identifier when parsing declaration statement"),
    };

    let expression = match subtokens.nth(1) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find expression when parsing declaration statement"),
    };

    SyntaxToken::DeclarationStatement {
        identifier: Box::new(identifier),
        expression: Box::new(expression),
    }
}

fn parse_call_expression(exp: Pair<Rule>) -> SyntaxToken {
    let mut subtokens = exp.into_inner();

    let identifier = match subtokens.nth(0) {
        Some(t) => SyntaxToken::IdentifierToken(String::from(t.as_str())),
        None => unreachable!("Cannot find identifier when parsing call expression"),
    };

    let arguments = match subtokens.nth(0) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find arguments when parsing call expression"),
    };

    SyntaxToken::CallExpression {
        identifier: Box::new(identifier),
        arguments: Box::new(arguments),
    }
}

fn parse_function_arguments(expr: Pair<Rule>) -> SyntaxToken {
    let subtokens = expr.into_inner();

    let mut expressions: Vec<SyntaxToken> = Vec::new();
    subtokens.for_each(|token| {
        let parsed = parse(Pairs::single(token));
        expressions.push(parsed);
    });

    SyntaxToken::FunctionArgumentsToken {
        args: Box::new(expressions),
    }
}

fn parse_code_block_statement(expr: Pair<Rule>) -> SyntaxToken {
    let subtokens = expr.into_inner();

    let mut tokens: Vec<SyntaxToken> = Vec::new();
    subtokens.for_each(|token| {
        let parsed = parse(Pairs::single(token));
        tokens.push(parsed);
    });

    SyntaxToken::CodeBlockStatement {
        tokens: Box::new(tokens),
    }
}

fn parse_function_declaration(expr: Pair<Rule>) -> SyntaxToken {
    let mut subtokens = expr.into_inner();

    let identifier = match subtokens.nth(0) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find identifier token when parsing function declaration"),
    };

    let mut params: Vec<SyntaxToken> = Vec::new();
    match subtokens.nth(0) {
        Some(t) => {
            let subtokens = t.into_inner();
            subtokens.for_each(|token| {
                let bounded_param = parse(Pairs::single(token));
                params.push(bounded_param)
            });
        }
        None => unreachable!("Cannot find parameters when parsing function delcaration"),
    };

    let code_block = match subtokens.nth(0) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find code block when parsing function delcaration"),
    };

    SyntaxToken::FunctionDeclarationExpression {
        identifier: Box::new(identifier),
        parameters: Box::new(params),
        code_block: Box::new(code_block),
    }
}

fn parse_function_parameter(param: Pair<Rule>) -> SyntaxToken {
    let mut subtokens = param.into_inner();

    let name = match subtokens.nth(0) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find name identifier when parsing function parameters"),
    };

    let type_ann = match subtokens.nth(0) {
        Some(t) => parse(Pairs::single(t)),
        None => unreachable!("Cannot find type identifier when parsing function parameters"),
    };

    SyntaxToken::FunctionParameter {
        name: Box::new(name),
        type_annotation: Box::new(type_ann),
    }
}

fn parse_module(module: Pair<Rule>) -> SyntaxToken {
    let subtokens = module.into_inner();

    let mut tokens: Vec<SyntaxToken> = Vec::new();
    subtokens.for_each(|token| {
        let parsed = parse(Pairs::single(token));
        tokens.push(parsed);
    });

    SyntaxToken::Module {
        tokens: Box::new(tokens),
    }
}

fn parse(pairs: Pairs<Rule>) -> SyntaxToken {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number_token => {
                SyntaxToken::NumberToken(primary.as_str().parse::<i32>().unwrap())
            }
            Rule::string_token => SyntaxToken::StringToken(String::from(primary.as_str())),
            Rule::identifier_token => SyntaxToken::IdentifierToken(String::from(primary.as_str())),
            Rule::expression => parse(primary.into_inner()),
            Rule::expression_statement => parse(primary.into_inner()),
            Rule::reference_expression => parse_reference_expression(primary),
            Rule::var_keyword => SyntaxToken::KeywordToken(Keyword::VarKeyword),
            Rule::declaration_statement => parse_declaration_statement(primary),
            Rule::assignment_expression => parse_assignment_expression(primary),
            Rule::call_expression => parse_call_expression(primary),
            Rule::function_args => parse_function_arguments(primary),
            Rule::function_definition_expression => parse_function_declaration(primary),
            Rule::func_parameter => parse_function_parameter(primary),
            Rule::code_block_statement => parse_code_block_statement(primary),
            Rule::module => parse_module(primary),
            rule => unreachable!("Token::parse expects an atom, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::addition => Op::Addition,
                Rule::subtraction => Op::Subtract,
                Rule::multiplication => Op::Multiplication,
                Rule::division => Op::Division,
                rule => unreachable!("Expected an infix operation, got '{:?}'", rule),
            };

            SyntaxToken::BinExpression {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

pub fn parse_contents(text: String) -> Result<SyntaxToken, ParseError> {
    let t = text.clone();
    match AltoParser::parse(Rule::module, t.as_str()) {
        Ok(mut pairs) => {
            let token = parse(Pairs::single(pairs.next().unwrap()));
            return Ok(token);
        }
        Err(_) => return Err(ParseError::Failed),
    }
}
