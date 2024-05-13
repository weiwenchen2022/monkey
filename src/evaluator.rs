use std::cmp::Ordering;
use std::collections::HashMap;

use crate::ast::{Expression, Node, Program, Statement};
use crate::environment::Environment;
use crate::error;
use crate::object::Object;

mod builtins;

pub type Result<T> = std::result::Result<T, String>;

pub trait Evaluator {
    fn eval(self, env: Environment) -> Result<Object>;
}

impl Evaluator for Program {
    fn eval(self, env: Environment) -> Result<Object> {
        eval_program(self, env)
    }
}

fn eval_program(program: Program, env: Environment) -> Result<Object> {
    let mut result = Object::Null;

    for statement in program.statements {
        result = statement.eval(env.clone())?;

        if let Object::ReturnValue(v) = result {
            return Ok(*v);
        }
    }

    Ok(result)
}

// Statements
impl Evaluator for Statement {
    fn eval(self, env: Environment) -> Result<Object> {
        match self {
            Statement::Expression { expression, .. } => expression.eval(env),
            Statement::Block { .. } => eval_blockstatement(self, env),
            Statement::Return { return_value, .. } => {
                let val = return_value.eval(env)?;
                Ok(Object::ReturnValue(Box::new(val)))
            }
            Statement::Let { name, value, .. } => {
                let val = value.eval(env.clone())?;
                let Expression::Identifier { value: name, .. } = name else {
                    panic!("not identifier expression got {name:?}");
                };
                env.set(name, val.clone());
                Ok(Object::Null)
            }
        }
    }
}

fn eval_blockstatement(block: Statement, env: Environment) -> Result<Object> {
    match block {
        Statement::Block { statements, .. } => {
            let mut result = Object::Null;

            for statement in statements {
                result = statement.eval(env.clone())?;
                if let Object::ReturnValue(_) = result {
                    return Ok(result);
                }
            }

            Ok(result)
        }
        _ => panic!("not block statement got {block:?}"),
    }
}

// Expressions
impl Evaluator for Expression {
    fn eval(self, env: Environment) -> Result<Object> {
        match self {
            Expression::IntegerLiteral { value, .. } => Ok(Object::Integer(value)),
            Expression::Boolean { value, .. } => Ok(Object::Boolean(value)),

            Expression::Prefix {
                operator, right, ..
            } => {
                let right = right.eval(env)?;
                eval_prefix_expression(&operator, right)
            }

            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => {
                let left = left.eval(env.clone())?;
                let right = right.eval(env)?;
                eval_infix_expression(&operator, left, right)
            }

            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                let condition = condition.eval(env.clone())?;
                if condition.into() {
                    consequence.eval(env)
                } else if let Some(alternative) = alternative {
                    alternative.eval(env)
                } else {
                    Ok(Object::Null)
                }
            }

            Expression::Identifier { value: name, .. } => eval_identifier(&name, env),

            Expression::FunctionLiteral {
                parameters, body, ..
            } => Ok(Object::Function {
                parameters,
                body: *body,
                env,
            }),

            Expression::Call {
                function,
                mut arguments,
                ..
            } => {
                use crate::ast::Tokenizer;

                if "quote" == (*function).token_literal() {
                    return Ok(quote(Node::Expression(arguments.pop().unwrap()), env));
                }

                let function = function.eval(env.clone())?;
                let args = eval_expressions(arguments, env)?;

                apply_function(function, args)
            }

            Expression::StringLiteral { value, .. } => Ok(Object::String(value)),

            Expression::ArrayLiteral { elements, .. } => {
                let elements = eval_expressions(elements, env)?;
                Ok(Object::Array(elements))
            }

            Expression::Index { left, index, .. } => {
                let left = left.eval(env.clone())?;
                let index = index.eval(env.clone())?;
                eval_index_expression(left, index)
            }

            Expression::HashLiteral { pairs, .. } => eval_hash_literal(pairs, env),

            _ => Ok(Object::Null),
        }
    }
}

fn eval_hash_literal(nodes: HashMap<Expression, Expression>, env: Environment) -> Result<Object> {
    let mut pairs = HashMap::with_capacity(nodes.len());

    for (key_node, value_node) in nodes {
        let key = key_node.eval(env.clone())?;
        if !key.is_hashable() {
            return error!("unusable as hash key: {}", key.ty());
        }

        let value = value_node.eval(env.clone())?;

        pairs.insert(key, value);
    }

    Ok(Object::Hash(pairs))
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
    // if index < 0 || index >= elements.len() as i64 {
    //     Ok(Object::Null)
    // } else {
    //     Ok(elements[index as usize].clone())
    // }
}

fn apply_function(f: Object, args: Vec<Object>) -> Result<Object> {
    match f {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let extend_env = extend_function_env(env, parameters, args);
            let evaluated = body.eval(extend_env)?;
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
    let env = Environment::new(Some(Box::new(env)));

    for (param_idx, param) in parameters.into_iter().enumerate() {
        let Expression::Identifier { value: name, .. } = param else {
            panic!("not identifier expression got {:?}", param);
        };
        env.set(name, std::mem::replace(&mut args[param_idx], Object::Null));
    }

    env
}

fn unwrap_return_value(obj: Object) -> Result<Object> {
    if let Object::ReturnValue(return_value) = obj {
        Ok(*return_value)
    } else {
        Ok(obj)
    }
}

fn eval_expressions(exps: Vec<Expression>, env: Environment) -> Result<Vec<Object>> {
    exps.into_iter().map(|e| e.eval(env.clone())).collect()
}

fn eval_identifier(name: &str, env: Environment) -> Result<Object> {
    if let Some(val) = env.get(name) {
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
