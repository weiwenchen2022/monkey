use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub enum Token {
    Illegal(String),
    Eof,

    // Identifiers + literals
    Ident(String),  // add, foobar, x, y, ...
    Int(String),    // 1343456
    String(String), // "foobar"

    LineComment(String), // // line comment

    // Operators
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Asterisk, // *
    Slash,    // /

    LT, // <
    GT, // >

    EQ,    // ==
    NotEq, // !=

    // Delimiters
    Comma,     // ","
    Semicolon, // ";"
    Colon,     // ":"

    LParen,   // "("
    RParen,   // ")"
    LBrace,   // "{"
    RBrace,   // "}"
    LBracket, // "["
    RBracket, // "]"

    // Keywords
    Function, // fn
    Let,      // let
    True,     // true
    False,    // false
    IF,       // if
    Else,     // else
    Return,   // return
    Marco,    // macro
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{s}"),
            Token::Int(i) => write!(f, "{i}"),
            Token::String(s) => write!(f, "{s}"),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Eof => write!(f, ""),

            Token::Ident(_) => write!(f, "Ident"),
            Token::Int(_) => write!(f, "Int"),
            Token::String(_) => write!(f, "String"),
            Token::LineComment(_) => write!(f, "LineComment"),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),

            Token::EQ => write!(f, "=="),
            Token::NotEq => write!(f, "!="),

            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),

            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),

            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::IF => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Marco => write!(f, "marco"),

            Token::Illegal(t) => write!(f, "{t}"),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Eq for Token {}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

use lazy_static::lazy_static;

use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.extend([
            ("fn", Token::Function),
            ("let", Token::Let),
            ("true", Token::True),
            ("false", Token::False),
            ("if", Token::IF),
            ("else", Token::Else),
            ("return", Token::Return),
            ("macro", Token::Marco),
        ]);
        m
    };
}

impl Token {
    pub fn new_ident(ident: String) -> Self {
        if let Some(tok) = KEYWORDS.get(ident.as_str()).cloned() {
            tok
        } else {
            Token::Ident(ident)
        }
    }
}
