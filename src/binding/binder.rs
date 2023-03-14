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
        members: Box<Vec<BoundNode>>
    },
    BinExpression {
        lhs: Box<BoundNode>,
        op: Op,
        rhs: Box<BoundNode>,
        tp: Type
    },
    AssignmentExpression {
        identifier: String,
        expression: Box<BoundNode>
    },
    DeclarationStatement {
        identifier: String,
        expression: Box<BoundNode>
    },
    CodeBlockStatement {
        members: Box<Vec<BoundNode>>
    },
    FunctionDeclarationExpression {
        identifier: String,
        params: Box<BoundNode>,
        code_block: Box<BoundNode>
    },
    FunctionArguments {
        agrs: Box<Vec<BoundNode>>
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

    BoundNode::RootStatement { members: Box::new(bounded) }
}

fn bind_number_token(num: i32) -> BoundNode {
    BoundNode::NumberLiteral(num)
}

fn bind_string_token(str: String) -> BoundNode {
    BoundNode::StringLiteral(str)
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

fn bind_assignment_expression(identifier: SyntaxToken, expression: SyntaxToken) -> BoundNode {
    if let SyntaxToken::IdentifierToken(str) = identifier {
        let bound_expr = bind(expression);
        if get_type(&bound_expr) == Type::Void {
            panic!("Cannot assign void to '{:?}'", str)
        }

        BoundNode::AssignmentExpression { identifier: str, expression: Box::new(bound_expr) }
    } else {
        panic!("Cannot assign to: '{:?}'", identifier)
    }
}

fn bind_declaration_statement(identifier: SyntaxToken, expression: SyntaxToken) -> BoundNode {
    if let SyntaxToken::IdentifierToken(str) = identifier {
        let bound_expr = bind(expression);
        if get_type(&bound_expr) == Type::Void {
            panic!("Cannot assign void to '{:?}'", str)
        }

        BoundNode::DeclarationStatement { identifier: str, expression: Box::new(bound_expr) }
    } else {
        panic!("Cannot assign to: '{:?}'", identifier)
    }
}

fn bind_code_block_statement(tokens: Vec<SyntaxToken>) -> BoundNode {
    let mut bounded = Vec::new();
    for t in tokens {
        bounded.push(bind(t));
    }

    BoundNode::CodeBlockStatement { members: Box::new(bounded) }
}

fn bind_function_declaration_expression(identifier: SyntaxToken, params: SyntaxToken, block: SyntaxToken) -> BoundNode {
    let (SyntaxToken::IdentifierToken(func_ident), SyntaxToken::FunctionArgumentsToken { .. }, SyntaxToken::CodeBlockStatement { .. }) = (identifier, &params, &block) else {
        panic!("Incorrect token signature for function declaration")
    };

    let params = bind(params);
    let block = bind(block);

    BoundNode::FunctionDeclarationExpression { identifier: func_ident, params: Box::new(params), code_block: Box::new(block) }
}

fn bind_function_arguments(args: Vec<SyntaxToken>) -> BoundNode {
    let mut arguments: Vec<BoundNode> = Vec::new();
    for arg in args {
        let bounded = bind(arg);
        arguments.push(bounded);
    }

    BoundNode::FunctionArguments { agrs: Box::new(arguments) }    
}

fn get_type(node: &BoundNode) -> Type {
    match node {
        BoundNode::NumberLiteral(..) => Type::Number,
        BoundNode::StringLiteral(..) => Type::String,
        BoundNode::RootStatement {..} => Type::Void,
        BoundNode::BinExpression { lhs: _, op: _, rhs: _, tp } => tp.clone(),
        BoundNode::AssignmentExpression { identifier: _, expression } => get_type(expression),
        BoundNode::DeclarationStatement { identifier: _, expression: _ } => Type::Void,
        BoundNode::CodeBlockStatement { members: _ } => Type::Void,
        BoundNode::FunctionDeclarationExpression { identifier: _, params: _, code_block: _ } => Type::Void, // this might be changed later, expressions cannot be of type void...
        BoundNode::FunctionArguments { agrs: _ } => Type::Void
    }
}

pub fn bind(token: SyntaxToken) -> BoundNode {
    match token {
        SyntaxToken::RootStatement { tokens } => { bind_root_statement(*tokens) }
        SyntaxToken::NumberToken(num) => { bind_number_token(num) },
        SyntaxToken::StringToken(str) => { bind_string_token(str) },
        SyntaxToken::BinExpression { lhs, op, rhs } => { bind_bin_expression(*lhs, op, *rhs) },
        SyntaxToken::AssignmentExpression { identifier, expression } => { bind_assignment_expression(*identifier, *expression) },
        SyntaxToken::DeclarationStatement { identifier, expression } => { bind_declaration_statement(*identifier, *expression) },
        SyntaxToken::CodeBlockStatement { tokens } => { bind_code_block_statement(*tokens) },
        SyntaxToken::FunctionDeclarationExpression { identifier, parameters, code_block } => { bind_function_declaration_expression(*identifier, *parameters, *code_block) },
        SyntaxToken::FunctionArgumentsToken { args } => { bind_function_arguments(*args) },
        _ => unreachable!("Unknown token: '{:?}'", token)
    }
}