use core::panic;

use crate::syntax::parser::{SyntaxToken, Op};

pub struct VariableSymbol {
    name: String,
    tp: Type
}

pub struct BoundScope<'a> {
    variables: Vec<VariableSymbol>,
    parent: Option<Box<&'a BoundScope<'a>>>
}

impl<'a> BoundScope<'a> {
    pub fn declare_variable(&mut self, variable: VariableSymbol) {
        self.variables.push(variable);
    }

    pub fn get_variable(&self, name: String) -> Option<&VariableSymbol> {
        let vs: Vec<&VariableSymbol> = self.variables.iter().filter(|v| v.name == name).map(|v| v).collect();
        if vs.len() > 0 {
            Some(vs[0])
        } else {
            match &self.parent {
                Some(p) => p.get_variable(name),
                None => None
            }
        }
    }

    pub fn new_root() -> BoundScope<'a> {
        BoundScope { variables: vec![], parent: None }
    }

    pub fn new(parent: &'a BoundScope<'a>) -> BoundScope<'a> {
        BoundScope { variables: vec![], parent: Some(Box::new(parent)) }
    }
}

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
    ReferenceExpression {
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
    },
    CallExpression {
        identifier: String,
        args: Box<BoundNode>
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Void
}

fn bind_root_statement(scope: &mut BoundScope, tokens: Vec<SyntaxToken>) -> BoundNode {
    let mut bounded = Vec::new();
    for t in tokens {
        bounded.push(bind(t, scope));
    }

    BoundNode::RootStatement { members: Box::new(bounded) }
}

fn bind_number_token(num: i32) -> BoundNode {
    BoundNode::NumberLiteral(num)
}

fn bind_string_token(str: String) -> BoundNode {
    BoundNode::StringLiteral(str)
}

fn bind_bin_expression(scope: &mut BoundScope, lhs: SyntaxToken, op: Op, rhs: SyntaxToken) -> BoundNode {
    let left = bind(lhs, scope);
    let right = bind(rhs, scope);

    let left_type = get_type(&left);
    let right_type = get_type(&right);
    if left_type != right_type {
        panic!("Undefined binary operator for types left: '{:?}' and right: '{:?}'", left_type, right_type);
    }

    BoundNode::BinExpression { lhs: Box::new(left), op: op, rhs: Box::new(right), tp: left_type }
}

fn bind_reference_expression(scope: &mut BoundScope, identifier: SyntaxToken) -> BoundNode {
    // check if variable reference exists
    // currently reference expressions only reference variables

    if let SyntaxToken::IdentifierToken(str) = identifier {
        let symbol = scope.get_variable(str.clone());
        match symbol {
            Some(s) => { 
                BoundNode::ReferenceExpression { tp: s.tp.clone() }
            },
            None => panic!("Cannot find reference to '{}'", str)
        }
    } else {
        panic!("Cannot find identifier when binding reference expression")
    }
}

fn bind_assignment_expression(scope: &mut BoundScope, identifier: SyntaxToken, expression: SyntaxToken) -> BoundNode {
    if let SyntaxToken::IdentifierToken(str) = identifier {
        let bound_expr = bind(expression, scope);
        if get_type(&bound_expr) == Type::Void {
            panic!("Cannot assign void to '{:?}'", str)
        }

        // TODO: Check if variable exists in scope
        let existing_symbol = scope.get_variable(str.clone());
        match existing_symbol {
            Some(_) => unreachable!("Variable with name '{}' already exists in the current scope", &str),
            None => {
                // declare the variable
                let symbol = VariableSymbol { name: str.clone(), tp: get_type(&bound_expr) };
                scope.declare_variable(symbol);
            }
        }

        BoundNode::AssignmentExpression { identifier: str, expression: Box::new(bound_expr) }
    } else {
        panic!("Cannot assign to: '{:?}'", identifier)
    }
}

fn bind_declaration_statement(scope: &mut BoundScope, identifier: SyntaxToken, expression: SyntaxToken) -> BoundNode {
    if let SyntaxToken::IdentifierToken(str) = identifier {
        let bound_expr = bind(expression, scope);
        if get_type(&bound_expr) == Type::Void {
            panic!("Cannot assign void to '{:?}'", str)
        }

        // TODO: Check for duplicate variables

        // declare the variable
        let symbol = VariableSymbol { name: str.clone(), tp: get_type(&bound_expr) };
        scope.declare_variable(symbol);

        BoundNode::DeclarationStatement { identifier: str, expression: Box::new(bound_expr) }
    } else {
        panic!("Cannot assign to: '{:?}'", identifier)
    }
}

fn bind_code_block_statement(scope: &mut BoundScope, tokens: Vec<SyntaxToken>) -> BoundNode {
    // TODO: Create new scope
    let mut child_scope = BoundScope::new(scope);

    let mut bounded = Vec::new();
    for t in tokens {
        let bounded_token = bind(t, &mut child_scope);
        bounded.push(bounded_token);
    }

    BoundNode::CodeBlockStatement { members: Box::new(bounded) }
}

fn bind_function_declaration_expression(scope: &mut BoundScope, identifier: SyntaxToken, params: SyntaxToken, block: SyntaxToken) -> BoundNode {
    let (SyntaxToken::IdentifierToken(func_ident), SyntaxToken::FunctionArgumentsToken { .. }, SyntaxToken::CodeBlockStatement { .. }) = (identifier, &params, &block) else {
        panic!("Incorrect token signature for function declaration")
    };

    let params = bind(params, scope);
    let block = bind(block, scope);

    BoundNode::FunctionDeclarationExpression { identifier: func_ident, params: Box::new(params), code_block: Box::new(block) }
}

fn bind_function_arguments(scope: &mut BoundScope, args: Vec<SyntaxToken>) -> BoundNode {
    let mut arguments: Vec<BoundNode> = Vec::new();
    for arg in args {
        let bounded = bind(arg, scope);
        arguments.push(bounded);
    }

    BoundNode::FunctionArguments { agrs: Box::new(arguments) }    
}

fn bind_call_expression(scope: &mut BoundScope, identifier: SyntaxToken, args: SyntaxToken) -> BoundNode {
    let (SyntaxToken::IdentifierToken(call_ident), SyntaxToken::FunctionArgumentsToken { .. }) = (identifier, &args) else {
        panic!("Incorrect call expression signature")
    };

    let arguments = bind(args, scope);
    BoundNode::CallExpression { identifier: call_ident, args: Box::new(arguments) }
}

fn get_type(node: &BoundNode) -> Type {
    match node {
        BoundNode::NumberLiteral(..) => Type::Number,
        BoundNode::StringLiteral(..) => Type::String,
        BoundNode::RootStatement {..} => Type::Void,
        BoundNode::BinExpression { lhs: _, op: _, rhs: _, tp } => tp.clone(),
        BoundNode::ReferenceExpression { tp } => tp.clone(),
        BoundNode::AssignmentExpression { identifier: _, expression } => get_type(expression),
        BoundNode::DeclarationStatement { identifier: _, expression: _ } => Type::Void,
        BoundNode::CodeBlockStatement { members: _ } => Type::Void,
        BoundNode::FunctionDeclarationExpression { identifier: _, params: _, code_block: _ } => Type::Void, // this might be changed later, expressions cannot be of type void...
        BoundNode::FunctionArguments { agrs: _ } => Type::Void,
        BoundNode::CallExpression { identifier: _, args: _ } => Type::Void // TODO: check for function type
    }
}

fn bind(token: SyntaxToken, scope: &mut BoundScope) -> BoundNode {
    match token {
        SyntaxToken::RootStatement { tokens } => { bind_root_statement(scope, *tokens) }
        SyntaxToken::NumberToken(num) => { bind_number_token(num) },
        SyntaxToken::StringToken(str) => { bind_string_token(str) },
        SyntaxToken::BinExpression { lhs, op, rhs } => { bind_bin_expression(scope, *lhs, op, *rhs) },
        SyntaxToken::ReferenceExpression { identifier } => { bind_reference_expression(scope, *identifier) }
        SyntaxToken::AssignmentExpression { identifier, expression } => { bind_assignment_expression(scope, *identifier, *expression) },
        SyntaxToken::DeclarationStatement { identifier, expression } => { bind_declaration_statement(scope, *identifier, *expression) },
        SyntaxToken::CodeBlockStatement { tokens } => { bind_code_block_statement(scope, *tokens) },
        SyntaxToken::FunctionDeclarationExpression { identifier, parameters, code_block } => { bind_function_declaration_expression(scope, *identifier, *parameters, *code_block) },
        SyntaxToken::FunctionArgumentsToken { args } => { bind_function_arguments(scope, *args) },
        SyntaxToken::CallExpression { identifier, arguments } => { bind_call_expression(scope, *identifier, *arguments) }
        _ => unreachable!("Unknown token: '{:?}'", token)
    }
}

pub fn bind_global_scope(token: SyntaxToken) -> BoundNode {
    // create global scope and enter binding APIs
    let mut global_scope = BoundScope::new_root();
    bind(token, &mut global_scope)
}