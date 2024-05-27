#![allow(unused_imports)]

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use crate::ast::{BlockStatement, Expression, Identifier, Node, Program, Statement};
use crate::error;
use crate::object::{Environment, Function, Object};

mod builtins;

pub type Result<T> = std::result::Result<T, String>;

pub fn eval<N: Into<Node>>(node: N, env: &Environment) -> Result<Object> {
    match node.into() {
        Node::Program(program) => eval_program(&program, env),

        // Statements
        Node::Statement(stmt) => eval_statement(&stmt, env),

        // Expressions
        Node::Expression(exp) => eval_expression(&exp, env),
    }
}

fn eval_program(program: &Program, env: &Environment) -> Result<Object> {
    let mut result = Object::Null;

    for stmt in &program.statements {
        result = eval_statement(stmt, env)?;
        if let Object::ReturnValue(v) = result {
            return Ok(*v);
        }
    }

    Ok(result)
}

fn eval_statement(stmt: &Statement, env: &Environment) -> Result<Object> {
    match stmt {
        Statement::Expression { expression, .. } => eval_expression(expression, env),
        Statement::Block(block) => eval_block_statement(block, env),
        Statement::Return { return_value, .. } => {
            let val = eval_expression(return_value, env)?;
            Ok(Object::ReturnValue(Box::new(val)))
        }
        Statement::Let { name, value, .. } => {
            let val = eval_expression(value, env)?;
            env.borrow_mut().set(name.value.clone(), val);
            Ok(Object::Null)
        }
    }
}

fn eval_expression(exp: &Expression, env: &Environment) -> Result<Object> {
    match exp {
        &Expression::IntegerLiteral { value, .. } => Ok(Object::Integer(value)),
        &Expression::Boolean { value, .. } => Ok(Object::Boolean(value)),

        Expression::Prefix {
            operator, right, ..
        } => {
            let right = eval_expression(right, env)?;
            eval_prefix_expression(operator, right)
        }
        Expression::Infix {
            left,
            operator,
            right,
            ..
        } => {
            let left = eval_expression(left, env)?;
            let right = eval_expression(right, env)?;
            eval_infix_expression(operator, left, right)
        }

        Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } => {
            let condition = eval_expression(condition, env)?;
            if condition.into() {
                eval_block_statement(consequence, env)
            } else if let Some(alternative) = alternative {
                eval_block_statement(alternative, env)
            } else {
                Ok(Object::Null)
            }
        }

        Expression::Identifier(Identifier { value: name, .. }) => eval_identifier(name, env),

        Expression::FunctionLiteral {
            parameters, body, ..
        } => Ok(Object::Function(Rc::new(Function {
            parameters: parameters.clone(),
            body: body.clone(),
            env: Rc::clone(env),
        }))),
        Expression::Call {
            function,
            arguments,
            ..
        } => {
            use crate::ast::Tokenizer;

            if "quote" == function.token_literal() {
                return Ok(quote(
                    Node::Expression(arguments.first().cloned().unwrap()),
                    env,
                ));
            }

            let function = eval_expression(function, env)?;
            let args = eval_expressions(arguments, env)?;
            apply_function(function, args)
        }

        Expression::StringLiteral { value, .. } => Ok(value.clone().into()),

        Expression::ArrayLiteral { elements, .. } => {
            let elements = eval_expressions(elements, env)?;
            Ok(elements.into())
        }

        Expression::Index { left, index, .. } => {
            let left = eval_expression(left, env)?;
            let index = eval_expression(index, env)?;
            eval_index_expression(left, index)
        }

        Expression::HashLiteral { pairs, .. } => eval_hash_literal(pairs, env),

        _ => unreachable!("{exp}"),
    }
}

fn eval_block_statement(block: &BlockStatement, env: &Environment) -> Result<Object> {
    let mut result = Object::Null;

    for statement in &block.statements {
        result = eval_statement(statement, env)?;
        if let Object::ReturnValue(_) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_hash_literal(pairs: &[(Expression, Expression)], env: &Environment) -> Result<Object> {
    let mut hash = HashMap::with_capacity(pairs.len());

    for (key_node, value_node) in pairs {
        let key = eval_expression(key_node, env)?;
        if !key.is_hashable() {
            return error!("unusable as hash key: {}", key.ty());
        }

        let value = eval_expression(value_node, env)?;

        hash.insert(key, value);
    }

    Ok(hash.into())
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
        Object::Function(f) => {
            let extend_env = extend_function_env(f.env.clone(), f.parameters.clone(), args);
            let evaluated = eval_block_statement(&f.body, &extend_env)?;
            unwrap_return_value(evaluated)
        }

        Object::Builtin(f) => f(args),

        _ => error!("not a function: {}", f.ty()),
    }
}

fn extend_function_env(
    env: Environment,
    parameters: Vec<Identifier>,
    mut args: Vec<Object>,
) -> Environment {
    use crate::object::environment::_Environment;
    let mut env = _Environment::new(Some(env));

    for (param_idx, param) in parameters.into_iter().enumerate() {
        env.set(
            param.value,
            std::mem::replace(&mut args[param_idx], Object::Null),
        );
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

fn eval_expressions(exps: &[Expression], env: &Environment) -> Result<Vec<Object>> {
    exps.iter().map(|e| eval_expression(e, env)).collect()
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
