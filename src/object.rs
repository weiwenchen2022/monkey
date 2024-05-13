use std::collections::HashMap;
use std::fmt::{self, Display};

#[macro_export]
macro_rules! error {
    ($format:tt, $($arg:expr),*) => {
        Err(format!($format, $($arg),*))
    };
}

pub type BuiltinFunction = fn(Vec<Object>) -> evaluator::Result<Object>;

#[derive(Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Environment,
    },
    String(String),
    Builtin(BuiltinFunction),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Quote(Node),

    Macro {
        parameters: Vec<Expression>,
        body: Statement,
        env: Environment,
    },
}

pub const NULL: &str = "null";
pub const ERROR: &str = "error";
pub const INTEGER: &str = "integer";
pub const BOOLEAN: &str = "boolean";
pub const RETURN_VALUE: &str = "return_value";
pub const FUNCTION: &str = "function";
pub const STRING: &str = "string";
pub const BUILTIN: &str = "builtin";
pub const ARRAY: &str = "array";
pub const HASH: &str = "hash";
pub const QUOTE: &str = "quote";
pub const MARCO: &str = "macro";

impl Object {
    pub fn ty(&self) -> &'static str {
        match self {
            Object::Null => NULL,
            Object::Integer(_) => INTEGER,
            Object::Boolean(_) => BOOLEAN,
            Object::ReturnValue(_) => RETURN_VALUE,
            Object::Error(_) => ERROR,
            Object::Function { .. } => FUNCTION,
            Object::String(_) => STRING,
            Object::Builtin(_) => BUILTIN,
            Object::Array(_) => ARRAY,
            Object::Hash(_) => HASH,
            Object::Quote(_) => QUOTE,
            Object::Macro { .. } => MARCO,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(_))
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            Object::Integer(_) | Object::String(_) | Object::Boolean(_)
        )
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Null => "null".fmt(f),
            Object::Integer(i) => i.fmt(f),
            Object::Boolean(b) => b.fmt(f),
            Object::ReturnValue(v) => v.fmt(f),
            Object::Error(e) => write!(f, "error: {e}"),
            Object::Function {
                parameters, body, ..
            } => {
                write!(f, "fn")?;
                write!(f, "(")?;
                parameters.iter().enumerate().try_for_each(|(i, p)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{p}")
                })?;
                writeln!(f, ") {{")?;
                write!(f, "{}", body)?;
                write!(f, "\n}}")
            }
            Object::String(s) => s.fmt(f),
            Object::Builtin(_) => "builtin function".fmt(f),
            Object::Array(elements) => {
                write!(f, "[")?;
                elements.iter().enumerate().try_for_each(|(i, el)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{el}")
                })?;
                write!(f, "]")
            }
            Object::Hash(pairs) => {
                write!(f, "{{")?;
                pairs.iter().enumerate().try_for_each(|(i, (key, value))| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)
                })?;
                write!(f, "}}")
            }

            Object::Quote(n) => n.fmt(f),

            Object::Macro {
                parameters, body, ..
            } => {
                write!(f, "marco")?;
                write!(f, "(")?;
                parameters.iter().enumerate().try_for_each(|(i, p)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{p}")
                })?;
                writeln!(f, ") {{")?;
                write!(f, "{}", body)?;
                write!(f, "\n}}")
            }
        }
    }
}

impl From<Object> for bool {
    fn from(obj: Object) -> Self {
        match obj {
            Object::Null => false,
            Object::Boolean(b) => b,
            _ => true,
        }
    }
}

use std::ops::{Neg, Not};

impl Not for Object {
    type Output = evaluator::Result<Object>;
    fn not(self) -> Self::Output {
        match self {
            Object::Boolean(b) => Ok(Object::Boolean(!b)),
            Object::Null => Ok(Object::Boolean(true)),
            _ => Ok(Object::Boolean(false)),
        }
    }
}

impl Neg for Object {
    type Output = evaluator::Result<Object>;
    fn neg(self) -> Self::Output {
        match self {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => error!("unknown operator: -{}", self.ty()),
        }
    }
}

use std::ops::{Add, Div, Mul, Sub};

impl Add for Object {
    type Output = evaluator::Result<Object>;
    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                Ok(Object::Integer(left_val + right_val))
            }
            (Object::String(left_val), Object::String(right_val)) => {
                Ok(Object::String(left_val.clone() + right_val))
            }
            _ => error!("unknown operator: {} + {}", self.ty(), rhs.ty()),
        }
    }
}

impl Sub for Object {
    type Output = evaluator::Result<Object>;
    fn sub(self, rhs: Self) -> Self::Output {
        let (left_val, right_val) = match (&self, &rhs) {
            (&Object::Integer(left_val), &Object::Integer(right_val)) => (left_val, right_val),
            _ => return error!("unknown operator: {} - {}", self.ty(), rhs.ty()),
        };
        Ok(Object::Integer(left_val - right_val))
    }
}

impl Mul for Object {
    type Output = evaluator::Result<Object>;
    fn mul(self, rhs: Self) -> Self::Output {
        let (left_val, right_val) = match (&self, &rhs) {
            (&Object::Integer(left_val), &Object::Integer(right_val)) => (left_val, right_val),
            _ => return error!("unknown operator: {} * {}", self.ty(), rhs.ty()),
        };
        Ok(Object::Integer(left_val * right_val))
    }
}

impl Div for Object {
    type Output = evaluator::Result<Object>;
    fn div(self, rhs: Self) -> Self::Output {
        let (left_val, right_val) = match (&self, &rhs) {
            (&Object::Integer(left_val), &Object::Integer(right_val)) => (left_val, right_val),
            _ => return error!("unknown operator: {} / {}", self.ty(), rhs.ty()),
        };
        Ok(Object::Integer(left_val / right_val))
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Null, Object::Null) => true,

            (Object::Integer(left_val), Object::Integer(right_val)) => left_val == right_val,
            (Object::Boolean(left_val), Object::Boolean(right_val)) => left_val == right_val,
            (Object::String(left_val), Object::String(right_val)) => left_val == right_val,

            _ => false,
        }
    }
}

impl Eq for Object {}

use std::cmp::Ordering;

use crate::ast::{Expression, Node, Statement};
use crate::environment::Environment;
use crate::evaluator;

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                left_val.partial_cmp(right_val)
            }
            _ => None,
        }
    }
}

use std::hash::{Hash, Hasher};

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Boolean(b) => b.hash(state),
            Object::Integer(i) => i.hash(state),
            Object::String(s) => s.hash(state),
            _ => panic!("unusable as hash key {}", self.ty()),
        }
    }
}

use crate::token::Token;

impl From<Object> for Node {
    fn from(obj: Object) -> Self {
        match obj {
            Object::Integer(value) => Node::Expression(Expression::IntegerLiteral {
                token: Token::Int(format!("{value}")),
                value,
            }),
            Object::Boolean(value) => Node::Expression(Expression::Boolean {
                token: if value { Token::True } else { Token::False },
                value,
            }),
            Object::Quote(node) => node,
            _ => todo!("{}", obj),
        }
    }
}

#[cfg(test)]
mod tests;
