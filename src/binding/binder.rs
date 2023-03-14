use crate::syntax::parser::{SyntaxToken, Op};

/*
QUICK NOTE:
    this is how you can check for enum variants

    fn e_test(t: SyntaxToken) {
        if let SyntaxToken::DeclarationStatement { identifier, expression } = t {

        }
    }
*/

#[derive(Debug)]
pub enum BoundNode {
    NumberLiteral(i32),
    StringLiteral(String),
    RootStatement {
        tokens: Box<Vec<BoundNode>>
    },
    BinExpression {
        lhs: Box<BoundNode>,
        op: Op,
        rhs: Box<BoundNode>,
        tp: Type
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Void
}

fn bind_root_statement(tokens: Vec<SyntaxToken>) -> BoundNode {
    let mut bounded = Vec::new();
    for t in tokens {
        bounded.push(bind(t));
    }

    BoundNode::RootStatement { tokens: Box::new(bounded) }
}

fn bind_bin_expression(lhs: SyntaxToken, op: Op, rhs: SyntaxToken) -> BoundNode {
    let left = bind(lhs);
    let right = bind(rhs);

    let left_type = get_type(&left);
    let right_type = get_type(&right);
    if left_type != right_type {
        panic!("Undefined binary operator for types left: '{:?}' and right: '{:?}'", left_type, right_type);
    }

    BoundNode::BinExpression { lhs: Box::new(left), op: op, rhs: Box::new(right), tp: left_type }
}

fn bind_number_token(num: i32) -> BoundNode {
    BoundNode::NumberLiteral(num)
}

fn bind_string_token(str: String) -> BoundNode {
    BoundNode::StringLiteral(str)
}

fn get_type(node: &BoundNode) -> Type {
    match node {
        BoundNode::NumberLiteral(..) => Type::Number,
        BoundNode::StringLiteral(..) => Type::String,
        BoundNode::RootStatement {..} => Type::Void,
        BoundNode::BinExpression { lhs: _, op: _, rhs: _, tp } => tp.clone(),
    }
}

pub fn bind(token: SyntaxToken) -> BoundNode {
    match token {
        SyntaxToken::RootStatement { tokens } => { bind_root_statement(*tokens) }
        SyntaxToken::BinExpression { lhs, op, rhs } => { bind_bin_expression(*lhs, op, *rhs) },
        SyntaxToken::NumberToken(num) => { bind_number_token(num) },
        SyntaxToken::StringToken(str) => { bind_string_token(str) },
        _ => unreachable!("Unknown token: '{:?}'", token)
    }
}