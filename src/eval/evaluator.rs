use crate::binding::binder::{BoundNode, FunctionSymbol};
use crate::syntax::parser::Op;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Bound;
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
    functions: HashMap<FunctionSymbol, BoundNode>
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
            functions: HashMap::new()
        }
    }

    pub fn declare_variable(&mut self, name: String, value: AnyValue) {
        self.variables.insert(name, value);
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
    identifier: String,
    expression: BoundNode,
    scope: Rc<RefCell<EvalScope>>,
) -> AnyValue {
    let value = evaluate(expression, Rc::clone(&scope));

    let mut borrow = scope.borrow_mut();
    borrow.declare_variable(identifier, value);

    AnyValue::new_void()
}

fn eval_function_declaration(identifier: String, parameters: Vec<BoundNode>, body: BoundNode, scope: Rc<RefCell<EvalScope>>) -> AnyValue {
    
    
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
    } else if identifier == "printnum" {
        eval_builtin_print(args, scope);
        return AnyValue::new_void();
    }

    // evaluate user functions
    // TODO: Implement this
    panic!("Unimplemented");
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
        BoundNode::DeclarationStatement {
            identifier,
            expression,
        } => eval_declaration_statement(identifier, *expression, scope),
        BoundNode::FunctionDeclarationExpression { identifier, params, code_block } => eval_function_declaration(identifier, *params, *code_block, scope),
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
