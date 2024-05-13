use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

use std::collections::HashMap;
use std::mem;

const LOWEST: usize = 1;
const EQAULS: usize = 2; // ==
const LESSGRATER: usize = 3; // > or <
const SUM: usize = 4; // +
const PRODUCT: usize = 5; // *
const PREFIX: usize = 6; // -X or !X
const CALL: usize = 7; // myFunction(X)
const INDEX: usize = 8; // array[index]

use lazy_static::lazy_static;

lazy_static! {
    static ref PRECEDENCES: HashMap<Token, usize> = {
        let mut m = HashMap::new();
        m.extend([
            (Token::EQ, EQAULS),
            (Token::NotEq, EQAULS),
            (Token::LT, LESSGRATER),
            (Token::GT, LESSGRATER),
            (Token::Plus, SUM),
            (Token::Minus, SUM),
            (Token::Slash, PRODUCT),
            (Token::Asterisk, PRODUCT),
            (Token::LParen, CALL),
            (Token::LBracket, INDEX),
        ]);
        m
    };
}

pub struct Parser<'a> {
    l: Lexer<'a>,
    errors: Vec<String>,

    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Self {
        let mut p = Self {
            l,
            errors: Vec::new(),

            cur_token: Token::Eof,
            peek_token: Token::Eof,
        };

        // Read two tokens, so curToken and peekToken are both set
        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.cur_token_is(Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_letstatement(),
            Token::Return => self.parse_returnstatement(),
            _ => self.parse_expressionstatement(),
        }
    }

    fn parse_letstatement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::Ident("".to_string())) {
            return None;
        }

        let name = Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.to_string(),
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(LOWEST).unwrap_or(Expression::Null);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let { token, name, value })
    }

    fn parse_returnstatement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression(LOWEST).unwrap_or(Expression::Null);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return {
            token,
            return_value,
        })
    }

    fn parse_expressionstatement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(LOWEST).unwrap_or(Expression::Null);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Expression { token, expression })
    }

    fn parse_expression(&mut self, precedence: usize) -> Option<Expression> {
        let prefix = self.parse_prefix();
        if prefix.is_none() {
            self.no_prefix_parsefn_error(self.cur_token.clone());
            return None;
        }

        let mut left_exp = prefix.unwrap();

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            let infix = self.parse_infix(left_exp.clone());
            if infix.is_none() {
                return Some(left_exp);
            }

            left_exp = infix?;
        }

        Some(left_exp)
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::IF => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::String(_) => self.parse_string_literal(),
            Token::LBracket => self.parse_array_literal(),
            Token::LBrace => self.parse_hash_literal(),
            Token::Marco => self.parse_macro_literal(),

            _ => None,
        }
    }

    fn parse_macro_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Token::LBrace) {
            return None;
        }

        let body = self.parse_blockstatement();

        Some(Expression::MacroLiteral {
            token,
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let mut pairs: HashMap<Expression, Expression> = HashMap::new();

        while !self.peek_token_is(&Token::RBrace) {
            self.next_token();

            let key = self.parse_expression(LOWEST)?;
            if !self.expect_peek(Token::Colon) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(LOWEST)?;

            pairs.insert(key, value);

            if !self.peek_token_is(&Token::RBrace) && !self.expect_peek(Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(Token::RBrace) {
            return None;
        }

        Some(Expression::HashLiteral { token, pairs })
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(Token::RBracket)?;
        Some(Expression::ArrayLiteral { token, elements })
    }

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Expression>> {
        let mut list = vec![];
        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(LOWEST)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(LOWEST)?);
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        Some(Expression::StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.to_string(),
        })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters().unwrap_or_default();

        if !self.expect_peek(Token::LBrace) {
            return None;
        }

        let body = self.parse_blockstatement();

        Some(Expression::FunctionLiteral {
            token,
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut identifiers = vec![];

        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.to_string(),
        };
        identifiers.push(ident);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            let ident = Expression::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.to_string(),
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(Token::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::LParen) {
            return None;
        }

        self.next_token();

        let codition = self.parse_expression(LOWEST);

        if !self.expect_peek(Token::RParen) {
            return None;
        }

        if !self.expect_peek(Token::LBrace) {
            return None;
        }

        let consequence = self.parse_blockstatement();

        let alternative = if self.peek_token_is(&Token::Else) {
            self.next_token();

            if !self.expect_peek(Token::LBrace) {
                return None;
            }

            Some(Box::new(self.parse_blockstatement()))
        } else {
            None
        };

        Some(Expression::If {
            token,
            condition: Box::new(codition?),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_blockstatement(&mut self) -> Statement {
        let token = self.cur_token.clone();

        let mut statements = vec![];
        self.next_token();

        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.next_token();
        }

        Statement::Block { token, statements }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(LOWEST);

        if !self.expect_peek(Token::RParen) {
            return None;
        }

        exp
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.to_string();

        self.next_token();

        let right = self.parse_expression(PREFIX).unwrap_or(Expression::Null);
        Some(Expression::Prefix {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix(&mut self, left: Expression) -> Option<Expression> {
        match &self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::EQ
            | Token::NotEq
            | Token::LT
            | Token::GT => {
                self.next_token();
                self.parse_infix_expression(left)
            }
            Token::LParen => {
                self.next_token();
                self.parse_call_expression(left)
            }
            Token::LBracket => {
                self.next_token();
                self.parse_index_expression(left)
            }

            _ => None,
        }
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();

        self.next_token();
        let index = self.parse_expression(LOWEST)?;

        if !self.expect_peek(Token::RBracket) {
            return None;
        }

        Some(Expression::Index {
            token,
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let arguments = self.parse_expression_list(Token::RParen)?;
        Some(Expression::Call {
            token,
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.to_string();
        let precedence = self.cur_precedence();
        self.next_token();

        let right = self
            .parse_expression(precedence)
            .unwrap_or(Expression::Null);

        Some(Expression::Infix {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn peek_precedence(&self) -> usize {
        PRECEDENCES.get(&self.peek_token).copied().unwrap_or(LOWEST)
    }

    fn cur_precedence(&self) -> usize {
        PRECEDENCES.get(&self.cur_token).copied().unwrap_or(LOWEST)
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.to_string(),
        })
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(Token::True),
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        use std::str::FromStr;
        let literal = self.cur_token.to_string();
        let Ok(value) = i64::from_str(&literal) else {
            let msg = format!("could not parse \"{}\" as integer", literal);
            self.errors.push(msg);
            return None;
        };

        Some(Expression::IntegerLiteral {
            token: self.cur_token.clone(),
            value,
        })
    }

    fn no_prefix_parsefn_error(&mut self, t: Token) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(msg);
    }

    pub fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.l.next_token());
    }

    fn cur_token_is(&self, t: Token) -> bool {
        t == self.cur_token
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        match (&self.peek_token, t) {
            (Token::Ident(_), Token::Ident(_)) => true,
            _ => t == &self.peek_token,
        }
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(&t) {
            self.next_token();
            true
        } else {
            self.peek_error(&t);
            false
        }
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn peek_error(&mut self, t: &Token) {
        let msg = format!(
            "expected next token to be `{:?}`, got `{:?}` instead",
            t, self.peek_token,
        );
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests;
