use crate::token::Token;

use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

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
    fn token_literal(&self) -> &str {
        match self {
            Node::Program(program) => program.token_literal(),
            Node::Statement(statement) => statement.token_literal(),
            Node::Expression(expression) => expression.token_literal(),
        }
    }
}

// The base Node interface
pub trait Tokenizer: Display {
    fn token_literal(&self) -> &str;
}

#[derive(Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Tokenizer for Program {
    fn token_literal(&self) -> &str {
        if let Some(first) = self.statements.first() {
            first.token_literal()
        } else {
            ""
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.statements.iter().try_for_each(|s| s.fmt(f))
    }
}

// Statements
#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        token: Token, // the Token::Let token
        name: Identifier,
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

    Block(BlockStatement),
}

impl Tokenizer for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let { token, .. }
            | Statement::Return { token, .. }
            | Statement::Expression { token, .. }
            | Statement::Block(BlockStatement { token, .. }) => token.literal(),
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

            Statement::Block(bs) => bs.fmt(f),
        }
    }
}

// Expressions
#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),

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
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },

    FunctionLiteral {
        token: Token, // The 'fn' token
        parameters: Vec<Identifier>,
        body: BlockStatement,
        name: String,
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
        pairs: Vec<(Expression, Expression)>,
    },

    MacroLiteral {
        token: Token, // The 'macro' token
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub(crate) token: Token, // the { token
    pub(crate) statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.statements.iter().try_for_each(|s| s.fmt(f))
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub(crate) token: Token, // the Token::IDENT token
    pub(crate) value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Eq for Expression {}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

impl Tokenizer for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(Identifier { token, .. })
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
            | Expression::MacroLiteral { token, .. } => token.literal(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(Identifier { value, .. }) => value.fmt(f),
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
                parameters,
                body,
                name,
                ..
            } => {
                write!(f, "{}", self.token_literal())?;
                if !name.is_empty() {
                    write!(f, "<{name}>")?;
                }
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
        }
    }
}

mod modify;
pub(crate) use modify::modify;

#[cfg(test)]
mod tests;
