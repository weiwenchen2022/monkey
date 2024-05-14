use crate::token::Token;
use std::{
    collections::HashMap,
    fmt::{self, Display},
};

#[derive(Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl From<Program> for Node {
    fn from(program: Program) -> Self {
        Node::Program(program)
    }
}

impl From<Statement> for Node {
    fn from(statement: Statement) -> Self {
        Node::Statement(statement)
    }
}

impl From<Expression> for Node {
    fn from(expression: Expression) -> Self {
        Node::Expression(expression)
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Program(program) => program.fmt(f),
            Node::Statement(stmt) => stmt.fmt(f),
            Node::Expression(exp) => exp.fmt(f),
        }
    }
}

impl Tokenizer for Node {
    fn token_literal(&self) -> String {
        match self {
            Node::Program(program) => program.token_literal(),
            Node::Statement(statement) => statement.token_literal(),
            Node::Expression(expression) => expression.token_literal(),
        }
    }
}

// The base Node interface
pub trait Tokenizer: Display {
    fn token_literal(&self) -> String;
}

#[derive(Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Tokenizer for Program {
    fn token_literal(&self) -> String {
        if let Some(first) = self.statements.first() {
            first.token_literal()
        } else {
            "".to_string()
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            s.fmt(f)?;
        }
        Ok(())
    }
}

// Statements
#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        token: Token, // the Token::Let token
        name: Expression,
        value: Expression,
    },

    Return {
        token: Token, // the 'return' token
        return_value: Expression,
    },

    Expression {
        token: Token, // the first token of the expression
        expression: Expression,
    },

    Block {
        token: Token, // the { token
        statements: Vec<Statement>,
    },
}

impl Tokenizer for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { token, .. }
            | Statement::Return { token, .. }
            | Statement::Expression { token, .. }
            | Statement::Block { token, .. } => format!("{}", token),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { token, name, value } => {
                write!(f, "{} ", token)?;
                write!(f, "{}", name)?;
                write!(f, " = ")?;
                write!(f, "{}", value)?;
                write!(f, ";")
            }

            Statement::Return {
                token,
                return_value,
            } => {
                write!(f, "{} ", token)?;
                write!(f, "{}", return_value)?;
                write!(f, ";")
            }

            Statement::Expression { expression, .. } => {
                write!(f, "{}", expression)
            }

            Statement::Block { statements, .. } => {
                for s in statements {
                    write!(f, "{}", s)?;
                }
                Ok(())
            }
        }
    }
}

// Expressions
#[derive(Clone, Debug)]
pub enum Expression {
    Null,

    Identifier {
        token: Token, // the Token::IDENT token
        value: String,
    },

    Boolean {
        token: Token,
        value: bool,
    },

    IntegerLiteral {
        token: Token,
        value: i64,
    },

    Prefix {
        token: Token, // The prefix token, e.g. !
        operator: String,
        right: Box<Expression>,
    },

    Infix {
        token: Token, // The operator token, e.g. +
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },

    If {
        token: Token, // The 'if' token
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },

    FunctionLiteral {
        token: Token, // The 'fn' token
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },

    Call {
        token: Token,              // The '(' token
        function: Box<Expression>, // Identifier or FunctionLiteral
        arguments: Vec<Expression>,
    },

    StringLiteral {
        token: Token,
        value: String,
    },

    ArrayLiteral {
        token: Token, // the '[' token
        elements: Vec<Expression>,
    },

    Index {
        token: Token, // The [ token,
        left: Box<Expression>,
        index: Box<Expression>,
    },

    HashLiteral {
        token: Token, // the '{' token
        pairs: HashMap<Expression, Expression>,
    },

    MacroLiteral {
        token: Token, // The 'macro' token
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Eq for Expression {}

use std::hash::{Hash, Hasher};

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

impl Tokenizer for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Null => "null".to_string(),

            Expression::Identifier { token, .. }
            | Expression::Boolean { token, .. }
            | Expression::IntegerLiteral { token, .. }
            | Expression::Prefix { token, .. }
            | Expression::Infix { token, .. }
            | Expression::If { token, .. }
            | Expression::FunctionLiteral { token, .. }
            | Expression::Call { token, .. }
            | Expression::StringLiteral { token, .. }
            | Expression::ArrayLiteral { token, .. }
            | Expression::Index { token, .. }
            | Expression::HashLiteral { token, .. }
            | Expression::MacroLiteral { token, .. } => token.to_string(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier { value, .. } => value.fmt(f),
            Expression::Boolean { value, .. } => value.fmt(f),
            Expression::IntegerLiteral { value, .. } => value.fmt(f),

            Expression::Prefix {
                operator, right, ..
            } => {
                write!(f, "(")?;
                write!(f, "{}", operator)?;
                write!(f, "{}", right)?;
                write!(f, ")")
            }

            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => {
                write!(f, "(")?;
                write!(f, "{}", left)?;
                write!(f, " {} ", operator)?;
                write!(f, "{}", right)?;
                write!(f, ")")
            }

            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                write!(f, "if")?;
                write!(f, "{}", condition)?;
                write!(f, " ")?;
                write!(f, "{}", consequence)?;
                if let Some(alternative) = alternative {
                    write!(f, "else ")?;
                    write!(f, "{}", alternative)?;
                }

                Ok(())
            }

            Expression::FunctionLiteral {
                parameters, body, ..
            } => {
                write!(f, "{}", self.token_literal())?;
                write!(f, "(")?;
                parameters.iter().enumerate().try_for_each(|(i, p)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{p}")
                })?;
                write!(f, ") ")?;
                write!(f, "{}", body)
            }

            Expression::Call {
                function,
                arguments,
                ..
            } => {
                write!(f, "{}", function)?;
                write!(f, "(")?;
                arguments.iter().enumerate().try_for_each(|(i, a)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{a}")
                })?;
                write!(f, ")")
            }

            Expression::StringLiteral { value, .. } => value.fmt(f),

            Expression::ArrayLiteral { elements, .. } => {
                write!(f, "[")?;
                elements.iter().enumerate().try_for_each(|(i, e)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")
                })?;
                write!(f, "]")
            }

            Expression::Index { left, index, .. } => {
                write!(f, "(")?;
                write!(f, "{}", left)?;
                write!(f, "[")?;
                write!(f, "{}", index)?;
                write!(f, "])")
            }

            Expression::HashLiteral { pairs, .. } => {
                write!(f, "{{")?;
                pairs.iter().enumerate().try_for_each(|(i, (key, value))| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}:{value}")
                })?;
                write!(f, "}}")
            }

            Expression::MacroLiteral {
                parameters, body, ..
            } => {
                write!(f, "{}", self.token_literal())?;
                write!(f, "(")?;
                parameters.iter().enumerate().try_for_each(|(i, p)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{p}")
                })?;
                write!(f, ") ")?;
                write!(f, "{}", body)
            }

            Expression::Null => unreachable!("null"),
        }
    }
}

mod modify;
pub(crate) use modify::modify;

#[cfg(test)]
mod tests;
