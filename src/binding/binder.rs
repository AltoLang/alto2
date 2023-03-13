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
    BinExpression {
        lhs: Box<BoundToken>,
        op: Op,
        rhs: Box<BoundToken>
    }
}

fn bind_bin_expression(lhs: SyntaxToken, op: Op, rhs: SyntaxToken) -> BoundToken {
    let left = bind(lhs);
    let right = bind(rhs);

    return BoundToken::BinExpression { lhs: Box::new(left), op: op, rhs: Box::new(right) };
}

pub fn bind(token: SyntaxToken) -> BoundToken {
    match token {
        SyntaxToken::BinExpression { lhs, op, rhs } => { bind_bin_expression(*lhs, op, *rhs) },
        _ => unreachable!("Unknown token {:?}", token)
    }
}