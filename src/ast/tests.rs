use super::*;

#[test]
fn display() {
    let program = Program {
        statements: vec![Statement::Let {
            token: Token::Let,
            name: Identifier {
                token: Token::Ident("myVar".to_string()),
                value: "myVar".to_string(),
            },
            value: Expression::Identifier(Identifier {
                token: Token::Ident("anotherVar".to_string()),
                value: "anotherVar".to_string(),
            }),
        }],
    };

    assert_eq!("let myVar = anotherVar;", program.to_string());
}
