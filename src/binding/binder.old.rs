use core::panic;
use crate::syntax::parser::{SyntaxToken, Op};

pub struct VariableSymbol {
    name: String,
    tp: Type
}

pub struct FunctionSymbol {
    name: String,
    tp: Type
}

pub struct BoundScope<'a> {
    variables: Vec<VariableSymbol>,
    functions: Vec<FunctionSymbol>,
    parent: Option<Box<&'a BoundScope<'a>>>
}

impl<'a> BoundScope<'a> {
    pub fn declare_variable(&mut self, variable: VariableSymbol) {
        self.variables.push(variable);
    }

    pub fn declare_function(&mut self, function: FunctionSymbol) {
        self.functions.push(function);
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

    pub fn get_function(&self, name: String) -> Option<&FunctionSymbol> {
        let funcs: Vec<&FunctionSymbol> = self.functions.iter().filter(|f| f.name == name).map(|f| f).collect();
        if funcs.len() > 0 {
            Some(funcs[0])
        } else {
            match &self.parent {
                Some(p) => p.get_function(name),
                None => None
            }
        }
    }

    pub fn new_root() -> BoundScope<'a> {
        BoundScope { variables: vec![], functions: vec![], parent: None }
    }

    pub fn new(parent: &'a BoundScope<'a>) -> BoundScope<'a> {
        BoundScope { variables: vec![], functions: vec![], parent: Some(Box::new(parent)) }
    }
}

#[derive(Debug)]
pub enum BoundNode {
    NumberLiteral(i32),
    StringLiteral(String),
    Module {
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
    FunctionParametersToken {
        params: Box<Vec<BoundNode>>
    },
    FunctionArguments {
        agrs: Box<Vec<BoundNode>>
    },
    CallExpression {
        identifier: String,
        args: Box<BoundNode>,
        tp: Type
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Void
}

fn bind_module(scope: &mut BoundScope, tokens: Vec<SyntaxToken>) -> BoundNode {
    let mut bounded = Vec::new();
    for t in tokens {
        bounded.push(bind(t, scope));
    }

    BoundNode::Module { members: Box::new(bounded) }
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

    // declare the function
    let existing_symbol = scope.get_function(func_ident.clone());
    match existing_symbol {
        None => {
            // for now, all functions will be of type void
            let symbol = FunctionSymbol { name: func_ident.clone(), tp: Type::Void };
            scope.declare_function(symbol);
        },
        Some(_) => panic!("Function with name '{}' already declared in this scope", func_ident)
    }

    let params = bind(params, scope);
    let block = bind(block, scope);

    BoundNode::FunctionDeclarationExpression { identifier: func_ident, params: Box::new(params), code_block: Box::new(block) }
}

fn bind_function_parameters_token(scope: &mut BoundScope, params: Vec<SyntaxToken>) -> BoundNode {
    let mut bounded: Vec<BoundNode> = Vec::new();
    for p in params {
        let b = bind(p, scope);
        bounded.push(b);
    }

    BoundNode::FunctionParametersToken { params: Box::new(bounded) }
}

fn bind_parameter(param_syntax: SyntaxToken) -> BoundNode {

}

fn bind_call_expression(scope: &mut BoundScope, identifier: SyntaxToken, args: SyntaxToken) -> BoundNode {
    let (SyntaxToken::IdentifierToken(call_ident), SyntaxToken::FunctionArgumentsToken { .. }) = (identifier, &args) else {
        panic!("Incorrect call expression signature")/lioiyu5kghj78klofv mjk gmi,

    // have to bind this before we
    // check if the function exists
    // due to mutable borrow rules
    let arguments = bind(args, scope);

    // check if function exists
    let symbol = scope.get_function(call_ident.clone());
    if let None = symbol {
        panic!("Unknown reference to function with name '{}'", call_ident.clone());
    }

    // TODO: Check if function argument signatures correct

    match symbol {
        None => panic!("Unknown reference to function with name '{}'", call_ident.clone()),
        Some(f) => {
            BoundNode::CallExpression { identifier: call_ident, args: Box::new(arguments), tp: f.tp.clone() }
        }
    }
}

fn bind_function_arguments(scope: &mut BoundScope, args: Vec<SyntaxToken>) -> BoundNode {
    let mut arguments: Vec<BoundNode> = Vec::new();
    for arg in args {
        let bounded = bind(arg, scope);
        arguments.push(bounded);
    }

    BoundNode::FunctionArguments { agrs: Box::new(arguments) }    
}

fn get_type(node: &BoundNode) -> Type {
    match node {
        BoundNode::NumberLiteral(..) => Type::Number,
        BoundNode::StringLiteral(..) => Type::String,
        BoundNode::Module {..} => Type::Void,
        BoundNode::BinExpression { lhs: _, op: _, rhs: _, tp } => tp.clone(),
        BoundNode::ReferenceExpression { tp } => tp.clone(),
        BoundNode::AssignmentExpression { identifier: _, expression } => get_type(expression),
        BoundNode::DeclarationStatement { identifier: _, expression: _ } => Type::Void,
        BoundNode::CodeBlockStatement { members: _ } => Type::Void,
        BoundNode::FunctionDeclarationExpression { identifier: _, params: _, code_block: _ } => Type::Void, // this might be changed later, expressions cannot be of type void...
        BoundNode::FunctionArguments { agrs: _ } => Type::Void,
        BoundNode::CallExpression { identifier: _, args: _, tp } => tp.clone()
    }
}

fn bind(token: SyntaxToken, scope: &mut BoundScope) -> BoundNode {
    match token {
        SyntaxToken::Module { tokens } => { bind_module(scope, *tokens) }
        SyntaxToken::NumberToken(num) => { bind_number_token(num) },
        SyntaxToken::StringToken(str) => { bind_string_token(str) },
        SyntaxToken::BinExpression { lhs, op, rhs } => { bind_bin_expression(scope, *lhs, op, *rhs) },
        SyntaxToken::ReferenceExpression { identifier } => { bind_reference_expression(scope, *identifier) }
        SyntaxToken::AssignmentExpression { identifier, expression } => { bind_assignment_expression(scope, *identifier, *expression) },
        SyntaxToken::DeclarationStatement { identifier, expression } => { bind_declaration_statement(scope, *identifier, *expression) },
        SyntaxToken::CodeBlockStatement { tokens } => { bind_code_block_statement(scope, *tokens) },
        SyntaxToken::FunctionDeclarationExpression { identifier, parameters, code_block } => { bind_function_declaration_expression(scope, *identifier, *parameters, *code_block) },
        SyntaxToken::FunctionParametersToken { params } => { bind_function_parameters_token(*params) },
        SyntaxToken::FunctionParameter { name, type_annotation } => {  }
        SyntaxToken::CallExpression { identifier, arguments } => { bind_call_expression(scope, *identifier, *arguments) }
        SyntaxToken::FunctionArgumentsToken { args } => { bind_function_arguments(scope, *args) },
        _ => unreachable!("Unknown token: '{:?}'", token)
    }
}

pub fn bind_global_scope(token: SyntaxToken) -> BoundNode {
    // create global scope and enter binding APIs
    let mut global_scope = BoundScope::new_root();
    bind(token, &mut global_scope)
}