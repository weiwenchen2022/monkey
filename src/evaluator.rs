#![allow(unused_imports)]

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use crate::ast::{Expression, Node, Program, Statement};
use crate::error;
use crate::object::{Environment, Object};

mod builtins;

pub type Result<T> = std::result::Result<T, String>;

pub fn eval<N: Into<Node>>(node: N, env: &Environment) -> Result<Object> {
    match node.into() {
        Node::Program(program) => eval_program(program, env),

        // Statements
        Node::Statement(Statement::Expression { expression, .. }) => eval(expression, env),
        Node::Statement(block @ Statement::Block { .. }) => eval_block_statement(block, env),
        Node::Statement(Statement::Return { return_value, .. }) => {
            let val = eval(return_value, env)?;
            Ok(Object::ReturnValue(Box::new(val)))
        }
        Node::Statement(Statement::Let { name, value, .. }) => {
            let val = eval(value, env)?;
            let Expression::Identifier { value: name, .. } = name else {
                panic!("not identifier expression got {name:?}");
            };
            env.borrow_mut().set(name, val);
            Ok(Object::Null)
        }

        // Expressions
        Node::Expression(Expression::IntegerLiteral { value, .. }) => Ok(Object::Integer(value)),
        Node::Expression(Expression::Boolean { value, .. }) => Ok(Object::Boolean(value)),

        Node::Expression(Expression::Prefix {
            operator, right, ..
        }) => {
            let right = eval(*right, env)?;
            eval_prefix_expression(&operator, right)
        }
        Node::Expression(Expression::Infix {
            left,
            operator,
            right,
            ..
        }) => {
            let left = eval(*left, env)?;
            let right = eval(*right, env)?;
            eval_infix_expression(&operator, left, right)
        }

        Node::Expression(Expression::If {
            condition,
            consequence,
            alternative,
            ..
        }) => {
            let condition = eval(*condition, env)?;
            if condition.into() {
                eval(*consequence, env)
            } else if let Some(alternative) = alternative {
                eval(*alternative, env)
            } else {
                Ok(Object::Null)
            }
        }

        Node::Expression(Expression::Identifier { value: name, .. }) => eval_identifier(&name, env),

        Node::Expression(Expression::FunctionLiteral {
            parameters, body, ..
        }) => Ok(Object::Function {
            parameters,
            body: *body,
            env: Rc::clone(env),
        }),
        Node::Expression(Expression::Call {
            function,
            mut arguments,
            ..
        }) => {
            use crate::ast::Tokenizer;

            if "quote" == function.token_literal() {
                return Ok(quote(Node::Expression(arguments.pop().unwrap()), env));
            }

            let function = eval(*function, env)?;
            let args = eval_expressions(arguments, env)?;
            apply_function(function, args)
        }

        Node::Expression(Expression::StringLiteral { value, .. }) => Ok(value.into()),

        Node::Expression(Expression::ArrayLiteral { elements, .. }) => {
            let elements = eval_expressions(elements, env)?;
            Ok(elements.into())
        }

        Node::Expression(Expression::Index { left, index, .. }) => {
            let left = eval(*left, env)?;
            let index = eval(*index, env)?;
            eval_index_expression(left, index)
        }

        Node::Expression(Expression::HashLiteral { pairs, .. }) => eval_hash_literal(pairs, env),

        node => unreachable!("{node}"),
    }
}

fn eval_program(program: Program, env: &Environment) -> Result<Object> {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval(statement, env)?;

        if let Object::ReturnValue(v) = result {
            return Ok(*v);
        }
    }

    Ok(result)
}

fn eval_block_statement(block: Statement, env: &Environment) -> Result<Object> {
    match block {
        Statement::Block { statements, .. } => {
            let mut result = Object::Null;

            for statement in statements {
                result = eval(statement, env)?;
                if let Object::ReturnValue(_) = result {
                    return Ok(result);
                }
            }

            Ok(result)
        }
        _ => unreachable!("not block statement got {block:?}"),
    }
}

fn eval_hash_literal(nodes: HashMap<Expression, Expression>, env: &Environment) -> Result<Object> {
    let mut pairs = HashMap::with_capacity(nodes.len());

    for (key_node, value_node) in nodes {
        let key = eval(key_node, env)?;
        if !key.is_hashable() {
            return error!("unusable as hash key: {}", key.ty());
        }

        let value = eval(value_node, env)?;

        pairs.insert(key, value);
    }

    Ok(pairs.into())
}

fn eval_index_expression(left: Object, index: Object) -> Result<Object> {
    match (&left, &index) {
        (Object::Array(_), Object::Integer(_)) => eval_array_index_expression(left, index),
        (Object::Hash(_), _) => eval_hash_index_expression(left, index),
        _ => error!("index operator not supported: {}", left.ty()),
    }
}

fn eval_hash_index_expression(hash: Object, index: Object) -> Result<Object> {
    let Object::Hash(hash) = hash else {
        return error!("index operator not supported: {}", hash.ty());
    };

    if !index.is_hashable() {
        return error!("unusable as hash key: {}", index.ty());
    }

    Ok(hash.get(&index).cloned().unwrap_or(Object::Null))
}

fn eval_array_index_expression(array: Object, index: Object) -> Result<Object> {
    let Object::Array(elements) = array else {
        return error!("index operator not supported: {}", array.ty());
    };

    let Object::Integer(index) = index else {
        return error!("index operator not supported: {}", index.ty());
    };

    Ok(elements
        .get(index as usize)
        .cloned()
        .unwrap_or(Object::Null))
}

fn apply_function(f: Object, args: Vec<Object>) -> Result<Object> {
    match f {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let extend_env = extend_function_env(env, parameters, args);
            let evaluated = eval(body, &extend_env)?;
            unwrap_return_value(evaluated)
        }

        Object::Builtin(f) => f(args),

        _ => error!("not a function: {}", f.ty()),
    }
}

fn extend_function_env(
    env: Environment,
    parameters: Vec<Expression>,
    mut args: Vec<Object>,
) -> Environment {
    use crate::object::environment::_Environment;
    let mut env = _Environment::new(Some(env));

    for (param_idx, param) in parameters.into_iter().enumerate() {
        let Expression::Identifier { value: name, .. } = param else {
            panic!("not identifier expression got {:?}", param);
        };
        env.set(name, std::mem::replace(&mut args[param_idx], Object::Null));
    }

    Rc::new(RefCell::new(env))
}

fn unwrap_return_value(obj: Object) -> Result<Object> {
    if let Object::ReturnValue(return_value) = obj {
        Ok(*return_value)
    } else {
        Ok(obj)
    }
}

fn eval_expressions(exps: Vec<Expression>, env: &Environment) -> Result<Vec<Object>> {
    exps.into_iter().map(|e| eval(e, env)).collect()
}

fn eval_identifier(name: &str, env: &Environment) -> Result<Object> {
    if let Some(val) = env.borrow().get(name) {
        return Ok(val);
    }

    if let Some(builtin) = builtins::BUILTINS.get(name).copied() {
        return Ok(Object::Builtin(builtin));
    }

    error!("identifier not found: {}", name)
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object> {
    match operator {
        "!" => !right,
        "-" => -right,
        _ => error!("unknown operator: {}{}", operator, right.ty()),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object> {
    if left.ty() != right.ty() {
        return error!("type mismatch: {} {} {}", left.ty(), operator, right.ty());
    }

    match operator {
        "+" => left + right,
        "-" => left - right,
        "*" => left * right,
        "/" => left / right,

        "==" => Ok(Object::Boolean(left == right)),
        "!=" => Ok(Object::Boolean(left != right)),

        "<" => left
            .partial_cmp(&right)
            .map(|ord| Ok(Object::Boolean(matches!(ord, Ordering::Less))))
            .unwrap_or_else(|| error!("unknown operator: {} < {}", left.ty(), right.ty())),
        ">" => left
            .partial_cmp(&right)
            .map(|ord| Ok(Object::Boolean(matches!(ord, Ordering::Greater))))
            .unwrap_or_else(|| error!("unknown operator: {} > {}", left.ty(), right.ty())),

        _ => error!(
            "unknown operator: {} {} {}",
            left.ty(),
            operator,
            right.ty()
        ),
    }
}

mod quote_unquote;
pub(crate) use quote_unquote::quote;

mod macro_expansion;
pub(crate) use macro_expansion::{define_macros, expand_macros};

#[cfg(test)]
mod tests;

#[cfg(test)]
mod macro_expansion_test;
