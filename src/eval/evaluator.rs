use crate::binding::binder::{BoundNode, FunctionSymbol, VariableSymbol};
use crate::syntax::parser::Op;
use core::panic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env::Args;
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
        body: Rc<RefCell<BoundNode>>,
    ) {
        let name = symbol.name.clone();
        let symbol = Rc::new(RefCell::new(symbol));
        let scope = Rc::clone(&parent_scope);

        let definition = EvalFunctionDefinition {
            symbol,
            parent_scope: scope,
            body: Rc::clone(&body),
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
    symbol: &VariableSymbol,
    expression: Rc<RefCell<BoundNode>>,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let value = evaluate(expression, Rc::clone(&scope));

    let mut borrow = scope.borrow_mut();
    borrow.declare_variable(symbol.name, value);

    AnyValue::new_void()
}

fn eval_function_declaration(
    symbol: &FunctionSymbol,
    body: Rc<RefCell<BoundNode>>,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let mut borrow = scope.borrow_mut();
    borrow.declare_function(symbol, Rc::clone(&scope), body);

    AnyValue::new_void()
}

fn eval_code_block_statement(members: Vec<BoundNode>, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    let new_scope = Rc::new(RefCell::new(EvalScope::new(scope)));
    for member in members {
        let member = Rc::new(RefCell::new(member));
        evaluate(member, Rc::clone(&new_scope));
    }

    AnyValue::new_void()
}

fn eval_bin_expression(
    lhs: Rc<RefCell<BoundNode>>,
    op: Op,
    rhs: Rc<RefCell<BoundNode>>,
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
    expression: Rc<RefCell<BoundNode>>,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let value = evaluate(expression, Rc::clone(&scope));

    let mut borrow = scope.borrow_mut();
    borrow.change_variable_value(identifier, value.clone());

    value
}

fn eval_builtin_print(args_node: Rc<RefCell<BoundNode>>, scope: Rc<RefCell<EvalScope>>) {
    let args_node = args_node.borrow();
    let BoundNode::FunctionArguments { ref args } = *args_node else {
        panic!("Invalid arguments for print");
    };

    if args.len() > 1 || args.len() == 0 {
        panic!("Invalid arguments for print");
    }

    // what if there was a vec that used RCs instead of &
    // TODO: Research

    let arg = Rc::clone(&args[0]);
    let value = evaluate(arg, scope);

    // print the value out
    println!("{}", value.to_string());
}

fn eval_call_expression(
    identifier: String,
    args: Rc<RefCell<BoundNode>>,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    if identifier == "print" {
        eval_builtin_print(args, scope);
        return AnyValue::new_void();
    } else if identifier == "printnum" {
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

    let args = args.borrow();
    let BoundNode::FunctionArguments { args: ref bound_args } = *args else {
        panic!("Incorrect surface of function arguments in eval");
    };

    if bound_args.len() != symbol.params.len() {
        panic!("Incorrect number of arguments in eval");
    }

    // create a scope for function execution
    let mut new_scope = EvalScope::new(Rc::clone(&definition.parent_scope));

    // declare all arguments as variables in new scope
    /*
    let mut param_iter = symbol.params.iter();
    let bound_args = *bound_args;

    for bound_argument in bound_args {
        let param_symbol = param_iter.next().unwrap();

        let m = Rc::new(RefCell::from(bound_argument));
        let value = evaluate(m, Rc::clone(&scope));

        // declare the argument as a variable in the new scope
        new_scope.declare_variable(param_symbol.name.clone(), value);
    }

    // evaluate the function body
    let value = evaluate(definition.body, Rc::new(RefCell::new(new_scope)));
    value
    */

    AnyValue::new_void()
}

fn eval_module(members: Vec<BoundNode>, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    // eval for the members
    for member in members {
        let member = Rc::new(RefCell::new(member));
        evaluate(member, Rc::clone(&scope));
    }

    AnyValue::new_void()
}

fn evaluate(node: Rc<RefCell<BoundNode>>, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    let node = node.borrow();

    // walk the tree and evaluate the nodes
    match *node {
        BoundNode::DeclarationStatement {
            ref symbol,
            ref expression,
        } => eval_declaration_statement(symbol, Rc::clone(expression), scope),
        BoundNode::FunctionDeclarationExpression {
            ref symbol,
            ref code_block,
        } => eval_function_declaration(symbol, Rc::clone(code_block), scope),
        BoundNode::CodeBlockStatement { ref members } => eval_code_block_statement(*members, scope),
        BoundNode::BinExpression {
            ref lhs,
            ref op,
            ref rhs,
            ..
        } => eval_bin_expression(lhs, op, rhs, scope),
        BoundNode::ReferenceExpression { ref identifier, .. } => {
            eval_reference_expression(identifier, scope)
        }
        BoundNode::AssignmentExpression {
            ref identifier,
            ref expression,
        } => eval_assignment_expression(identifier, expression, scope),
        BoundNode::CallExpression {
            ref identifier,
            ref args,
            ..
        } => eval_call_expression(identifier.to_owned(), args, scope),
        BoundNode::Module { ref members } => eval_module(*members, scope),
        BoundNode::StringLiteral(ref s) => AnyValue::new_string(s.to_owned()),
        BoundNode::NumberLiteral(ref n) => AnyValue::new_int(n),
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

    let node = Rc::new(RefCell::new(root));
    evaluate(node, Rc::new(RefCell::new(scope)));
}
