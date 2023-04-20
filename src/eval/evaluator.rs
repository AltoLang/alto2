use crate::binding::binder::{BoundNode, FunctionSymbol, VariableSymbol};
use crate::syntax::parser::Op;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// just an enclosure for values
#[derive(Clone)]
struct AnyValue {
    // primitive values, other types will be added later
    // TODO: Add other types than primitives
    string_value: Option<String>,
    int_value: Option<i32>,
    is_void: bool,
}

struct EvalScope {
    parent: Option<Rc<RefCell<EvalScope>>>,
    variables: HashMap<String, AnyValue>,
    functions: HashMap<String, EvalFunctionDefinition>,
}

struct EvalFunctionDefinition {
    symbol: Rc<RefCell<FunctionSymbol>>,
    parent_scope: Rc<RefCell<EvalScope>>,
    body: Rc<RefCell<BoundNode>>,
}

impl AnyValue {
    fn new_void() -> AnyValue {
        AnyValue {
            string_value: None,
            int_value: None,
            is_void: true,
        }
    }

    fn new_string(s: String) -> AnyValue {
        AnyValue {
            string_value: Some(s),
            int_value: None,
            is_void: false,
        }
    }

    fn new_int(i: i32) -> AnyValue {
        AnyValue {
            string_value: None,
            int_value: Some(i),
            is_void: false,
        }
    }

    fn to_string(self) -> String {
        if self.is_void {
            String::from("void")
        } else if (&self.string_value).is_some() {
            self.string_value.unwrap().to_string()
        } else if self.int_value.is_some() {
            self.int_value.unwrap().to_string()
        } else {
            String::from("unknown")
        }
    }
}

impl EvalScope {
    fn new(parent_scope: Rc<RefCell<EvalScope>>) -> EvalScope {
        EvalScope {
            parent: Some(parent_scope),
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn declare_variable(&mut self, name: String, value: AnyValue) {
        self.variables.insert(name, value);
    }

    pub fn declare_function(
        &mut self,
        symbol: FunctionSymbol,
        parent_scope: Rc<RefCell<EvalScope>>,
        body: BoundNode,
    ) {
        let name = symbol.name.clone();
        let symbol = Rc::new(RefCell::new(symbol));
        let scope = Rc::clone(&parent_scope);
        let body = Rc::new(RefCell::new(body));

        let definition = EvalFunctionDefinition {
            symbol,
            parent_scope: scope,
            body,
        };

        self.functions.insert(name, definition);
    }

    pub fn get_variable(&self, name: String) -> Option<AnyValue> {
        let vs: Vec<(&String, &AnyValue)> = self
            .variables
            .iter()
            .filter(|v| v.0.to_owned() == name)
            .map(|v| v)
            .collect();
        if vs.len() > 0 {
            let owned = vs[0].1.clone();
            Some(owned)
        } else {
            // check in parent
            match &self.parent {
                Some(p) => {
                    let borrow = p.borrow();
                    borrow.get_variable(name)
                }
                None => None,
            }
        }
    }

    pub fn get_function(&self, name: String) -> Option<EvalFunctionDefinition> {
        let vs: Vec<(&String, &EvalFunctionDefinition)> = self
            .functions
            .iter()
            .filter(|v| v.0.to_owned() == name)
            .map(|v| v)
            .collect();
        if vs.len() > 0 {
            let owned = vs[0];

            // construct new from old
            let definition = owned.1;
            let symbol = Rc::clone(&definition.symbol);
            let parent_scope = Rc::clone(&definition.parent_scope);
            let body = Rc::clone(&definition.body);

            let new_definition = EvalFunctionDefinition {
                symbol,
                parent_scope,
                body,
            };

            Some(new_definition)
        } else {
            // check in parent
            match &self.parent {
                Some(p) => {
                    let borrow = p.borrow();
                    borrow.get_function(name)
                }
                None => None,
            }
        }
    }

    pub fn change_variable_value(&mut self, name: String, value: AnyValue) {
        if !self.variables.contains_key(&name) {
            match &mut self.parent {
                Some(p) => {
                    let mut borrow = p.borrow_mut();
                    borrow.change_variable_value(name.clone(), value)
                }
                None => panic!("Variable not found"),
            }

            return;
        }

        self.variables.remove(&name);
        self.variables.insert(name, value);
    }
}

fn eval_declaration_statement(
    symbol: VariableSymbol,
    expression: BoundNode,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let value = evaluate(expression, Rc::clone(&scope));

    let mut borrow = scope.borrow_mut();
    borrow.declare_variable(symbol.name, value);

    AnyValue::new_void()
}

fn eval_function_declaration(
    symbol: FunctionSymbol,
    body: BoundNode,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let mut borrow = scope.borrow_mut();
    borrow.declare_function(symbol, Rc::clone(&scope), body);

    AnyValue::new_void()
}

fn eval_code_block_statement(members: Vec<BoundNode>, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    let new_scope = Rc::new(RefCell::new(EvalScope::new(scope)));
    for member in members {
        evaluate(member, Rc::clone(&new_scope));
    }

    AnyValue::new_void()
}

fn eval_bin_expression(
    lhs: BoundNode,
    op: Op,
    rhs: BoundNode,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let lhs = evaluate(lhs, Rc::clone(&scope));
    let rhs = evaluate(rhs, scope);

    match op {
        Op::Addition => {
            if lhs.string_value.is_some() && rhs.string_value.is_some() {
                AnyValue::new_string(format!(
                    "{}{}",
                    lhs.string_value.unwrap(),
                    rhs.string_value.unwrap()
                ))
            } else if lhs.int_value.is_some() && rhs.int_value.is_some() {
                AnyValue::new_int(lhs.int_value.unwrap() + rhs.int_value.unwrap())
            } else {
                panic!("Invalid types for addition");
            }
        }
        Op::Subtraction => {
            if lhs.int_value.is_some() && rhs.int_value.is_some() {
                AnyValue::new_int(lhs.int_value.unwrap() - rhs.int_value.unwrap())
            } else {
                panic!("Invalid types for subtraction");
            }
        }
        Op::Multiplication => {
            if lhs.int_value.is_some() && rhs.int_value.is_some() {
                AnyValue::new_int(lhs.int_value.unwrap() * rhs.int_value.unwrap())
            } else {
                panic!("Invalid types for multiplication");
            }
        }
        Op::Division => {
            if lhs.int_value.is_some() && rhs.int_value.is_some() {
                AnyValue::new_int(lhs.int_value.unwrap() / rhs.int_value.unwrap())
            } else {
                panic!("Invalid types for division");
            }
        }
    }
}

fn eval_reference_expression(identifier: String, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    let borrow = scope.borrow();
    let value = borrow.get_variable(identifier);
    if value.is_some() {
        value.unwrap()
    } else {
        panic!("Variable not found");
    }
}

fn eval_assignment_expression(
    identifier: String,
    expression: BoundNode,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let value = evaluate(expression, Rc::clone(&scope));

    let mut borrow = scope.borrow_mut();
    borrow.change_variable_value(identifier, value.clone());

    value
}

fn eval_builtin_print(args: BoundNode, scope: Rc<RefCell<EvalScope>>) {
    let BoundNode::FunctionArguments { agrs } = args else {
        panic!("Invalid arguments for print");
    };

    let mut args = *agrs;
    if args.len() > 1 || args.len() == 0 {
        panic!("Invalid arguments for print");
    }

    let arg = args.remove(0);
    let value = evaluate(arg, scope);

    // print the value out
    println!("{}", value.to_string());
}

fn eval_call_expression(
    identifier: String,
    args: BoundNode,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    if identifier == "print" {
        eval_builtin_print(args, scope);
        return AnyValue::new_void();
    } else if identifier == "print_num" {
        eval_builtin_print(args, scope);
        return AnyValue::new_void();
    }

    let borrowed_scope = scope.borrow();
    let function = borrowed_scope.get_function(identifier);

    if function.is_none() {
        panic!("Function not found");
    }

    let definition = function.unwrap();
    let symbol = definition.symbol.borrow();

    // all of the following panics should have
    // been dealt with in the binder, consider
    // removing them...

    let BoundNode::FunctionArguments { agrs: bound_args } = args else {
        panic!("Incorrect surface of function arguments in eval");
    };

    if bound_args.len() != symbol.params.len() {
        panic!("Incorrect number of arguments in eval");
    }

    // create a scope for function execution
    let mut new_scope = EvalScope::new(Rc::clone(&definition.parent_scope));

    // declare all arguments as variables in new scope
    let mut param_iter = symbol.params.iter();
    let bound_args = *bound_args;
    for n in bound_args {
        let param_symbol = param_iter.next().unwrap();
        let value = evaluate(n, Rc::clone(&scope));

        // declare the argument as a variable in the new scope
        new_scope.declare_variable(param_symbol.name.clone(), value);
    }

    // cloning the body and then evaluating it
    // is by all means not ideal, it consumes
    // a lot of resouces and passing nodes as
    // references would be a lot more efficient,
    // but also a lot less ergonomic.

    // evaluate the function body
    let body = definition.body.borrow().clone();
    let value = evaluate(body, Rc::new(RefCell::new(new_scope)));
    value
}

fn eval_module(members: Vec<BoundNode>, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    // eval for the members
    for member in members {
        evaluate(member, Rc::clone(&scope));
    }

    AnyValue::new_void()
}

fn evaluate(node: BoundNode, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    // walk the tree and evaluate the nodes
    match node {
        BoundNode::DeclarationStatement { symbol, expression } => {
            eval_declaration_statement(symbol, *expression, scope)
        }
        BoundNode::FunctionDeclarationExpression { symbol, code_block } => {
            eval_function_declaration(symbol, *code_block, scope)
        }
        BoundNode::CodeBlockStatement { members } => eval_code_block_statement(*members, scope),
        BoundNode::BinExpression { lhs, op, rhs, .. } => eval_bin_expression(*lhs, op, *rhs, scope),
        BoundNode::ReferenceExpression { identifier, .. } => {
            eval_reference_expression(identifier, scope)
        }
        BoundNode::AssignmentExpression {
            identifier,
            expression,
        } => eval_assignment_expression(identifier, *expression, scope),
        BoundNode::CallExpression {
            identifier, args, ..
        } => eval_call_expression(identifier.to_owned(), *args, scope),
        BoundNode::Module { members } => eval_module(*members, scope),
        BoundNode::StringLiteral(s) => AnyValue::new_string(s.to_owned()),
        BoundNode::NumberLiteral(n) => AnyValue::new_int(n),
        _ => {
            panic!("Unimplemented {:?}", node)
        }
    }
}

pub fn eval_root(root: BoundNode) {
    let scope = EvalScope {
        parent: None,
        variables: HashMap::new(),
        functions: HashMap::new(),
    };

    evaluate(root, Rc::new(RefCell::new(scope)));
}
