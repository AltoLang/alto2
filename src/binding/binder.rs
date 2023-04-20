use crate::syntax::parser::{Op, SyntaxToken};
use core::panic;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Type {
    Number,
    String,
    Void,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableSymbol {
    pub name: String,
    pub tp: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSymbol {
    pub name: String,
    pub params: Vec<VariableSymbol>,
    pub tp: Type,
}

pub struct BoundScope<'a> {
    variables: Vec<VariableSymbol>,
    functions: Vec<FunctionSymbol>,
    parent: Option<Box<&'a BoundScope<'a>>>,
}

impl<'a> BoundScope<'a> {
    pub fn declare_variable(&mut self, variable: VariableSymbol) {
        self.variables.push(variable);
    }

    pub fn declare_function(&mut self, function: FunctionSymbol) {
        self.functions.push(function);
    }

    pub fn get_variable(&self, name: String) -> Option<&VariableSymbol> {
        let vs: Vec<&VariableSymbol> = self
            .variables
            .iter()
            .filter(|v| v.name == name)
            .map(|v| v)
            .collect();
        if vs.len() > 0 {
            Some(vs[0])
        } else {
            match &self.parent {
                Some(p) => p.get_variable(name),
                None => None,
            }
        }
    }

    pub fn get_function(&self, name: String) -> Option<&FunctionSymbol> {
        let funcs: Vec<&FunctionSymbol> = self
            .functions
            .iter()
            .filter(|f| f.name == name)
            .map(|f| f)
            .collect();
        if funcs.len() > 0 {
            Some(funcs[0])
        } else {
            match &self.parent {
                Some(p) => p.get_function(name),
                None => None,
            }
        }
    }

    pub fn new_root() -> BoundScope<'a> {
        BoundScope {
            variables: vec![],
            functions: vec![],
            parent: None,
        }
    }

    pub fn new(parent: &'a BoundScope<'a>) -> BoundScope<'a> {
        BoundScope {
            variables: vec![],
            functions: vec![],
            parent: Some(Box::new(parent)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BoundNode {
    NumberLiteral(i32),
    StringLiteral(String),
    Module {
        members: Box<Vec<BoundNode>>,
    },
    BinExpression {
        lhs: Box<BoundNode>,
        op: Op,
        rhs: Box<BoundNode>,
        tp: Type,
    },
    ReferenceExpression {
        identifier: String,
        tp: Type,
    },
    AssignmentExpression {
        identifier: String,
        expression: Box<BoundNode>,
    },
    DeclarationStatement {
        symbol: VariableSymbol,
        expression: Box<BoundNode>,
    },
    CodeBlockStatement {
        members: Box<Vec<BoundNode>>,
    },
    FunctionDeclarationExpression {
        symbol: FunctionSymbol,
        code_block: Box<BoundNode>,
    },
    FunctionParameter {
        name: String,
        type_annotation: Type,
    },
    FunctionArguments {
        agrs: Box<Vec<BoundNode>>,
    },
    CallExpression {
        identifier: String,
        args: Box<BoundNode>,
        tp: Type,
    },
}

fn bind_module(scope: &mut BoundScope, tokens: Vec<SyntaxToken>) -> BoundNode {
    let mut bounded = Vec::new();
    for t in tokens {
        bounded.push(bind(t, scope));
    }

    BoundNode::Module {
        members: Box::new(bounded),
    }
}

fn bind_number_token(num: i32) -> BoundNode {
    BoundNode::NumberLiteral(num)
}

fn bind_string_token(str: String) -> BoundNode {
    BoundNode::StringLiteral(str)
}

fn bind_bin_expression(
    scope: &mut BoundScope,
    lhs: SyntaxToken,
    op: Op,
    rhs: SyntaxToken,
) -> BoundNode {
    let left = bind(lhs, scope);
    let right = bind(rhs, scope);

    let left_type = get_type(&left);
    let right_type = get_type(&right);
    if left_type != right_type {
        panic!(
            "Undefined binary operator for types left: '{:?}' and right: '{:?}'",
            left_type, right_type
        );
    }

    BoundNode::BinExpression {
        lhs: Box::new(left),
        op: op,
        rhs: Box::new(right),
        tp: left_type,
    }
}

fn bind_reference_expression(scope: &mut BoundScope, identifier: SyntaxToken) -> BoundNode {
    // check if variable reference exists
    // currently reference expressions only reference variables

    if let SyntaxToken::IdentifierToken(str) = identifier {
        let symbol = scope.get_variable(str.clone());
        match symbol {
            Some(s) => BoundNode::ReferenceExpression {
                identifier: str,
                tp: s.tp.clone(),
            },
            None => panic!("Cannot find reference to '{}'", str),
        }
    } else {
        panic!("Cannot find identifier when binding reference expression")
    }
}

fn bind_assignment_expression(
    scope: &mut BoundScope,
    identifier: SyntaxToken,
    expression: SyntaxToken,
) -> BoundNode {
    if let SyntaxToken::IdentifierToken(str) = identifier {
        let bound_expr = bind(expression, scope);
        if get_type(&bound_expr) == Type::Void {
            panic!("Cannot assign void to '{:?}'", str)
        }

        // Check if variable exists in scope, if doesn't throw error
        let existing_symbol = scope.get_variable(str.clone());
        match existing_symbol {
            Some(symbol) => {
                // check if types match
                if symbol.tp != get_type(&bound_expr) {
                    panic!(
                        "Cannot assign type '{:?}' to variable of type '{:?}'",
                        get_type(&bound_expr),
                        symbol.tp
                    )
                }

                // we're good
            }
            None => panic!("Cannot assign to undeclared variable: '{}'", &str),
        }

        BoundNode::AssignmentExpression {
            identifier: str,
            expression: Box::new(bound_expr),
        }
    } else {
        panic!("Cannot assign to: '{:?}'", identifier)
    }
}

fn bind_declaration_statement(
    scope: &mut BoundScope,
    identifier: SyntaxToken,
    expression: SyntaxToken,
) -> BoundNode {
    if let SyntaxToken::IdentifierToken(str) = identifier {
        let bound_expr = bind(expression, scope);
        if get_type(&bound_expr) == Type::Void {
            panic!("Cannot assign void to '{:?}'", str)
        }

        let existing_symbol = scope.get_variable(str.clone());
        let symbol = match existing_symbol {
            Some(_) => unreachable!(
                "Variable with name '{}' already exists in the current scope",
                &str
            ),
            None => {
                // declare the variable
                let symbol = VariableSymbol {
                    name: str.clone(),
                    tp: get_type(&bound_expr),
                };

                scope.declare_variable(symbol.clone());
                symbol
            }
        };

        BoundNode::DeclarationStatement {
            symbol: symbol,
            expression: Box::new(bound_expr),
        }
    } else {
        panic!("Cannot assign to: '{:?}'", identifier)
    }
}

fn bind_code_block_statement(scope: &mut BoundScope, tokens: Vec<SyntaxToken>) -> BoundNode {
    let mut child_scope = BoundScope::new(scope);

    let mut bounded = Vec::new();
    for t in tokens {
        let bounded_token = bind(t, &mut child_scope);
        bounded.push(bounded_token);
    }

    BoundNode::CodeBlockStatement {
        members: Box::new(bounded),
    }
}

fn bind_code_block_use_scope(scope: &mut BoundScope, tokens: Vec<SyntaxToken>) -> BoundNode {
    // same as bind_code_block_statement
    // but uses the supplied scope
    // instead of creating a new one

    let mut bounded = Vec::new();
    for t in tokens {
        let bounded_token = bind(t, scope);
        bounded.push(bounded_token);
    }

    BoundNode::CodeBlockStatement {
        members: Box::new(bounded),
    }
}

fn bind_function_declaration_expression(
    scope: &mut BoundScope,
    identifier: SyntaxToken,
    params: Vec<SyntaxToken>,
    type_annotation: Option<Box<SyntaxToken>>,
    block: SyntaxToken,
) -> BoundNode {
    let (SyntaxToken::IdentifierToken(func_ident), SyntaxToken::CodeBlockStatement { .. }) = (identifier, &block) else {
        panic!("Incorrect token signature for function declaration")
    };

    // parameters
    let mut param_symbols: Vec<VariableSymbol> = Vec::new();
    let mut bounded_parameters: Vec<BoundNode> = Vec::new();
    for param in params {
        let bounded_param = bind(param, scope);

        let BoundNode::FunctionParameter { ref name, type_annotation } = bounded_param else {
            unreachable!("Incorrect signature for function parameter");
        };

        let symbol = VariableSymbol {
            name: name.clone(),
            tp: type_annotation,
        };
        param_symbols.push(symbol);
        bounded_parameters.push(bounded_param);
    }

    // bind type annotation
    let mut func_type = Type::Void;
    match type_annotation {
        Some(syntax_box) => {
            let syntax = *syntax_box;
            func_type = bind_function_type_annotation(syntax);
        }
        None => (),
    };

    // bind the code block
    let SyntaxToken::CodeBlockStatement { tokens: block_tokens } = block else {
        unreachable!("Cannot find code block when binding function declaration.")
    };

    // declare parameters as variables in new scope
    let mut block_scope = BoundScope::new(&scope);
    for param in &param_symbols {
        block_scope.declare_variable(param.clone().to_owned());
    }

    let block = bind_code_block_use_scope(&mut block_scope, *block_tokens);
    let block_type = get_type(&block);
    if block_type != func_type {
        panic!(
            "Mismatched types, expected {:?}, found {:?}",
            func_type,
            block_type.clone()
        )
    }

    // declare the function
    let existing_symbol = scope.get_function(func_ident.clone());
    let symbol = match existing_symbol {
        None => {
            // for now, all functions will be of type void
            let symbol = FunctionSymbol {
                name: func_ident.clone(),
                tp: func_type,
                params: param_symbols,
            };

            scope.declare_function(symbol.clone());
            symbol
        }
        Some(_) => panic!(
            "Function with name '{}' already declared in this scope",
            func_ident
        ),
    };

    BoundNode::FunctionDeclarationExpression {
        symbol: symbol,
        code_block: Box::new(block),
    }
}

fn bind_function_type_annotation(annotation: SyntaxToken) -> Type {
    if let SyntaxToken::FunctionTypeAnnotation { identifier } = annotation {
        if let SyntaxToken::IdentifierToken(str) = *identifier {
            get_type_annotation(str)
        } else {
            panic!("Type annotation must contain identifier")
        }
    } else {
        panic!("Incorrect token signature for function type annotation")
    }
}

fn bind_parameter(name_ident: SyntaxToken, type_annotation_ident: SyntaxToken) -> BoundNode {
    let (SyntaxToken::IdentifierToken(name), SyntaxToken::IdentifierToken(type_name)) = (name_ident, type_annotation_ident) else {
        panic!("Incorrect parameter signature");
    };

    let annotation = get_type_annotation(type_name);
    BoundNode::FunctionParameter {
        name: name,
        type_annotation: annotation,
    }
}

fn bind_call_expression(
    scope: &mut BoundScope,
    identifier: SyntaxToken,
    args: SyntaxToken,
) -> BoundNode {
    let (SyntaxToken::IdentifierToken(call_ident), SyntaxToken::FunctionArgumentsToken { .. }) = (identifier, &args) else {
        panic!("Incorrect call expression signature");
    };

    // have to bind this before we
    // check if the function exists
    // due to mutable borrow rules
    let arguments = bind(args, scope);

    // check if function exists
    let symbol = scope.get_function(call_ident.clone());
    if let None = symbol {
        panic!(
            "Unknown reference to function with name '{}'",
            call_ident.clone()
        );
    }

    let symbol = symbol.unwrap();

    // TODO: Check if function argument signatures correct
    let BoundNode::FunctionArguments { agrs: bound_args } = &arguments else {
        panic!("Incorrect surface of function arguments");
    };

    if bound_args.len() != symbol.params.len() {
        panic!("Incorrect number of args");
    }

    let mut arguments_iter = bound_args.iter();
    let mut parameters_iter = symbol.params.iter();
    while let Some(param) = parameters_iter.next() {
        let Some(arg) = arguments_iter.next() else {
            panic!("Incorrect number of args");
        };

        let arg_tp = get_type(arg);
        if param.tp != arg_tp {
            panic!("Argument type does not match parameter type");
        }
    }

    dbg!(&symbol.tp);

    BoundNode::CallExpression {
        identifier: call_ident,
        args: Box::new(arguments),
        tp: symbol.tp.clone(),
    }
}

fn bind_function_arguments(scope: &mut BoundScope, args: Vec<SyntaxToken>) -> BoundNode {
    let mut arguments: Vec<BoundNode> = Vec::new();
    for arg in args {
        let bounded = bind(arg, scope);
        arguments.push(bounded);
    }

    BoundNode::FunctionArguments {
        agrs: Box::new(arguments),
    }
}

fn get_type(node: &BoundNode) -> Type {
    match node {
        BoundNode::NumberLiteral(..) => Type::Number,
        BoundNode::StringLiteral(..) => Type::String,
        BoundNode::Module { .. } => Type::Void,
        BoundNode::BinExpression {
            lhs: _,
            op: _,
            rhs: _,
            tp,
        } => tp.clone(),
        BoundNode::ReferenceExpression { tp, .. } => tp.clone(),
        BoundNode::AssignmentExpression {
            identifier: _,
            expression,
        } => get_type(expression),
        BoundNode::DeclarationStatement {
            symbol: _,
            expression: _,
        } => Type::Void,
        BoundNode::CodeBlockStatement { members } => {
            let last = members.last().unwrap();
            get_type(last)
        }
        BoundNode::FunctionDeclarationExpression {
            symbol: _,
            code_block: _,
        } => Type::Void, // this might be changed later, expressions cannot be of type void...
        BoundNode::FunctionParameter {
            name: _,
            type_annotation: _,
        } => Type::Void,
        BoundNode::CallExpression {
            identifier: _,
            args: _,
            tp,
        } => tp.clone(),
        BoundNode::FunctionArguments { agrs: _ } => Type::Void,
    }
}

fn get_type_annotation(name: String) -> Type {
    if name == "void" {
        Type::Void
    } else if name == "string" {
        Type::String
    } else if name == "number" {
        Type::Number
    } else {
        unreachable!("Cannot resolve type '{}'", name)
    }
}

fn add_builtinfunctions(global_scope: &mut BoundScope) {
    // declare print function
    let symbol = FunctionSymbol {
        name: "print".to_string(),
        tp: Type::Void,
        params: vec![VariableSymbol {
            name: "value".to_string(),
            tp: Type::String,
        }],
    };

    global_scope.declare_function(symbol);

    // declare print_num function
    // this is a temporary fix
    // until we add functions with
    // same names but diff params

    let symbol = FunctionSymbol {
        name: "print_num".to_string(),
        tp: Type::Void,
        params: vec![VariableSymbol {
            name: "value".to_string(),
            tp: Type::Number,
        }],
    };

    global_scope.declare_function(symbol);
}

fn bind(token: SyntaxToken, scope: &mut BoundScope) -> BoundNode {
    match token {
        SyntaxToken::Module { tokens } => bind_module(scope, *tokens),
        SyntaxToken::NumberToken(num) => bind_number_token(num),
        SyntaxToken::StringToken(str) => bind_string_token(str),
        SyntaxToken::BinExpression { lhs, op, rhs } => bind_bin_expression(scope, *lhs, op, *rhs),
        SyntaxToken::ReferenceExpression { identifier } => {
            bind_reference_expression(scope, *identifier)
        }
        SyntaxToken::AssignmentExpression {
            identifier,
            expression,
        } => bind_assignment_expression(scope, *identifier, *expression),
        SyntaxToken::DeclarationStatement {
            identifier,
            expression,
        } => bind_declaration_statement(scope, *identifier, *expression),
        SyntaxToken::CodeBlockStatement { tokens } => bind_code_block_statement(scope, *tokens),
        SyntaxToken::FunctionDeclarationExpression {
            identifier,
            parameters,
            type_annotation,
            code_block,
        } => bind_function_declaration_expression(
            scope,
            *identifier,
            *parameters,
            type_annotation,
            *code_block,
        ),
        SyntaxToken::FunctionParameter {
            name,
            type_annotation,
        } => bind_parameter(*name, *type_annotation),
        SyntaxToken::CallExpression {
            identifier,
            arguments,
        } => bind_call_expression(scope, *identifier, *arguments),
        SyntaxToken::FunctionArgumentsToken { args } => bind_function_arguments(scope, *args),
        _ => unreachable!("Unknown token: '{:?}'", token),
    }
}

pub fn bind_global_scope(token: SyntaxToken) -> BoundNode {
    // create global scope and enter binding APIs
    let mut global_scope = BoundScope::new_root();
    add_builtinfunctions(&mut global_scope);

    bind(token, &mut global_scope)
}
