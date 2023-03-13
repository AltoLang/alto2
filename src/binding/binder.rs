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
pub enum BoundToken {
    NumberToken(i32),
    RootStatement {
        tokens: Box<Vec<BoundToken>>
    },
    BinExpression {
        lhs: Box<BoundToken>,
        op: Op,
        rhs: Box<BoundToken>
    }
}

fn bind_root_statement(tokens: Vec<SyntaxToken>) -> BoundToken {
    let mut bounded = Vec::new();
    for t in tokens {
        bounded.push(bind(t));
    }

    BoundToken::RootStatement { tokens: Box::new(bounded) }
}

fn bind_bin_expression(lhs: SyntaxToken, op: Op, rhs: SyntaxToken) -> BoundToken {
    let left = bind(lhs);
    let right = bind(rhs);

    BoundToken::BinExpression { lhs: Box::new(left), op: op, rhs: Box::new(right) }
}

fn bind_number_token(num: i32) -> BoundToken {
    BoundToken::NumberToken(num)
}


pub fn bind(token: SyntaxToken) -> BoundToken {
    match token {
        SyntaxToken::RootStatement { tokens } => { bind_root_statement(*tokens) }
        SyntaxToken::BinExpression { lhs, op, rhs } => { bind_bin_expression(*lhs, op, *rhs) },
        SyntaxToken::NumberToken(num) => { bind_number_token(num) },
        _ => unreachable!("Unknown token {:?}", token)
    }
}