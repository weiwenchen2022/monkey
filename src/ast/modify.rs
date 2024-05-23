use super::{Expression, Node, Program, Statement};

pub(crate) fn modify<M>(node: Node, mut modifier: M) -> Node
where
    M: FnMut(Node) -> Node + Clone,
{
    let node = match node {
        // program
        Node::Program(program) => {
            let statements = program
                .statements
                .into_iter()
                .map(|statement| {
                    let Node::Statement(statement) =
                        modify(Node::Statement(statement), modifier.clone())
                    else {
                        panic!();
                    };
                    statement
                })
                .collect();

            Node::Program(Program { statements })
        }

        // statements
        Node::Statement(Statement::Expression { token, expression }) => {
            let Node::Expression(expression) =
                modify(Node::Expression(expression), modifier.clone())
            else {
                panic!();
            };

            Node::Statement(Statement::Expression { token, expression })
        }

        // expressions
        Node::Expression(Expression::Infix {
            token,
            left,
            operator,
            right,
        }) => {
            let Node::Expression(left) = modify(Node::Expression(*left), modifier.clone()) else {
                panic!();
            };
            let Node::Expression(right) = modify(Node::Expression(*right), modifier.clone()) else {
                panic!();
            };

            Node::Expression(Expression::Infix {
                token,
                left: Box::new(left),
                operator,
                right: Box::new(right),
            })
        }

        Node::Expression(Expression::Prefix {
            token,
            operator,
            right,
        }) => {
            let Node::Expression(right) = modify(Node::Expression(*right), modifier.clone()) else {
                panic!()
            };

            Node::Expression(Expression::Prefix {
                token,
                operator,
                right: Box::new(right),
            })
        }

        Node::Expression(Expression::Index { token, left, index }) => {
            let Node::Expression(left) = modify(Node::Expression(*left), modifier.clone()) else {
                panic!()
            };
            let Node::Expression(index) = modify(Node::Expression(*index), modifier.clone()) else {
                panic!();
            };

            Node::Expression(Expression::Index {
                token,
                left: Box::new(left),
                index: Box::new(index),
            })
        }

        Node::Expression(Expression::If {
            token,
            condition,
            consequence,
            alternative,
        }) => {
            let Node::Expression(condition) =
                modify(Node::Expression(*condition), modifier.clone())
            else {
                panic!();
            };

            let Node::Statement(consequence) =
                modify(Node::Statement(*consequence), modifier.clone())
            else {
                panic!()
            };

            let alternative = alternative.map(|alternative| {
                let Node::Statement(alternative) =
                    modify(Node::Statement(*alternative), modifier.clone())
                else {
                    panic!()
                };
                Box::new(alternative)
            });

            Node::Expression(Expression::If {
                token,
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative,
            })
        }
        Node::Statement(Statement::Block { token, statements }) => {
            let statements = statements
                .into_iter()
                .map(|statement| {
                    let Node::Statement(statement) =
                        modify(Node::Statement(statement), modifier.clone())
                    else {
                        panic!();
                    };
                    statement
                })
                .collect();
            Node::Statement(Statement::Block { token, statements })
        }
        Node::Statement(Statement::Return {
            token,
            return_value,
        }) => {
            let Node::Expression(return_value) =
                modify(Node::Expression(return_value), modifier.clone())
            else {
                panic!();
            };

            Node::Statement(Statement::Return {
                token,
                return_value,
            })
        }
        Node::Statement(Statement::Let { token, name, value }) => {
            let Node::Expression(value) = modify(Node::Expression(value), modifier.clone()) else {
                panic!();
            };

            Node::Statement(Statement::Let { token, name, value })
        }

        Node::Expression(Expression::FunctionLiteral {
            token,
            parameters,
            body,
            name,
        }) => {
            let parameters = parameters
                .into_iter()
                .map(|parameter| {
                    let Node::Expression(parameter) =
                        modify(Node::Expression(parameter), modifier.clone())
                    else {
                        panic!();
                    };
                    parameter
                })
                .collect();

            let Node::Statement(body) = modify(Node::Statement(*body), modifier.clone()) else {
                panic!();
            };

            Node::Expression(Expression::FunctionLiteral {
                token,
                parameters,
                body: Box::new(body),
                name,
            })
        }

        Node::Expression(Expression::ArrayLiteral { token, elements }) => {
            let elements = elements
                .into_iter()
                .map(|elem| {
                    let Node::Expression(elem) = modify(Node::Expression(elem), modifier.clone())
                    else {
                        panic!();
                    };
                    elem
                })
                .collect();

            Node::Expression(Expression::ArrayLiteral { token, elements })
        }

        Node::Expression(Expression::HashLiteral { token, pairs }) => {
            let pairs = pairs
                .into_iter()
                .map(|(key, val)| {
                    let Node::Expression(key) = modify(Node::Expression(key), modifier.clone())
                    else {
                        panic!();
                    };
                    let Node::Expression(val) = modify(Node::Expression(val), modifier.clone())
                    else {
                        panic!();
                    };
                    (key, val)
                })
                .collect();
            Node::Expression(Expression::HashLiteral { token, pairs })
        }

        _ => node,
    };

    modifier(node)
}

#[cfg(test)]
mod tests {
    use super::modify;
    use crate::ast::{Expression, Node, Program, Statement};
    use crate::token::Token;

    #[test]
    fn test_modify() {
        let one = || Expression::IntegerLiteral {
            token: crate::token::Token::Int("1".to_string()),
            value: 1,
        };
        let two = || Expression::IntegerLiteral {
            token: crate::token::Token::Int("2".to_string()),
            value: 2,
        };

        let turn_one_into_two = |mut node: Node| -> Node {
            let Node::Expression(exp) = &mut node else {
                return node;
            };
            let Expression::IntegerLiteral { token, value } = exp else {
                return node;
            };

            if *value == 1 {
                *value = 2;
                *token = Token::Int("2".to_string());
            }
            node
        };

        struct Test {
            input: Node,
            expected: Node,
        }

        let tests = [
            Test {
                input: Node::Expression(one()),
                expected: Node::Expression(two()),
            },
            // program
            Test {
                input: Node::Program(Program {
                    statements: vec![Statement::Expression {
                        token: Token::Int("1".to_string()),
                        expression: one(),
                    }],
                }),
                expected: Node::Program(Program {
                    statements: vec![Statement::Expression {
                        token: Token::Int("2".to_string()),
                        expression: two(),
                    }],
                }),
            },
            // infix
            Test {
                input: Node::Expression(Expression::Infix {
                    token: Token::Plus,
                    left: Box::new(one()),
                    operator: "+".to_string(),
                    right: Box::new(two()),
                }),
                expected: Node::Expression(Expression::Infix {
                    token: Token::Plus,
                    left: Box::new(two()),
                    operator: "+".to_string(),
                    right: Box::new(two()),
                }),
            },
            Test {
                input: Node::Expression(Expression::Infix {
                    token: Token::Plus,
                    left: Box::new(two()),
                    operator: "+".to_string(),
                    right: Box::new(one()),
                }),
                expected: Node::Expression(Expression::Infix {
                    token: Token::Plus,
                    left: Box::new(two()),
                    operator: "+".to_string(),
                    right: Box::new(two()),
                }),
            },
            // prefix
            Test {
                input: Node::Expression(Expression::Prefix {
                    token: Token::Minus,
                    operator: "-".to_string(),
                    right: Box::new(one()),
                }),
                expected: Node::Expression(Expression::Prefix {
                    token: Token::Minus,
                    operator: "-".to_string(),
                    right: Box::new(two()),
                }),
            },
            // index
            Test {
                input: Node::Expression(Expression::Index {
                    token: Token::LBracket,
                    left: Box::new(one()),
                    index: Box::new(one()),
                }),
                expected: Node::Expression(Expression::Index {
                    token: Token::LBracket,
                    left: Box::new(two()),
                    index: Box::new(two()),
                }),
            },
            // if-expression
            Test {
                input: Node::Expression(Expression::If {
                    token: Token::IF,
                    condition: Box::new(one()),
                    consequence: Box::new(Statement::Block {
                        token: Token::LBrace,
                        statements: vec![Statement::Expression {
                            token: Token::Int("1".to_string()),
                            expression: one(),
                        }],
                    }),
                    alternative: Some(Box::new(Statement::Block {
                        token: Token::LBrace,
                        statements: vec![Statement::Expression {
                            token: Token::Int("1".to_string()),
                            expression: one(),
                        }],
                    })),
                }),
                expected: Node::Expression(Expression::If {
                    token: Token::IF,
                    condition: Box::new(two()),
                    consequence: Box::new(Statement::Block {
                        token: Token::LBrace,
                        statements: vec![Statement::Expression {
                            token: Token::Int("2".to_string()),
                            expression: two(),
                        }],
                    }),
                    alternative: Some(Box::new(Statement::Block {
                        token: Token::LBrace,
                        statements: vec![Statement::Expression {
                            token: Token::Int("2".to_string()),
                            expression: two(),
                        }],
                    })),
                }),
            },
            // return statement
            Test {
                input: Node::Statement(Statement::Return {
                    token: Token::Return,
                    return_value: one(),
                }),
                expected: Node::Statement(Statement::Return {
                    token: Token::Return,
                    return_value: two(),
                }),
            },
            // let statement
            Test {
                input: Node::Statement(Statement::Let {
                    token: Token::Let,
                    name: Expression::Identifier {
                        token: Token::Ident("a".to_string()),
                        value: "a".to_string(),
                    },
                    value: one(),
                }),
                expected: Node::Statement(Statement::Let {
                    token: Token::Let,
                    name: Expression::Identifier {
                        token: Token::Ident("a".to_string()),
                        value: "a".to_string(),
                    },
                    value: two(),
                }),
            },
            // function literal
            Test {
                input: Node::Expression(Expression::FunctionLiteral {
                    token: Token::Function,
                    parameters: vec![],
                    body: Box::new(Statement::Block {
                        token: Token::LBrace,
                        statements: vec![Statement::Expression {
                            token: Token::Int("1".to_string()),
                            expression: one(),
                        }],
                    }),
                    name: String::new(),
                }),
                expected: Node::Expression(Expression::FunctionLiteral {
                    token: Token::Function,
                    parameters: vec![],
                    body: Box::new(Statement::Block {
                        token: Token::LBrace,
                        statements: vec![Statement::Expression {
                            token: Token::Int("2".to_string()),
                            expression: two(),
                        }],
                    }),
                    name: String::new(),
                }),
            },
            // array literal
            Test {
                input: Node::Expression(Expression::ArrayLiteral {
                    token: Token::LBracket,
                    elements: vec![one(), one()],
                }),
                expected: Node::Expression(Expression::ArrayLiteral {
                    token: Token::LBracket,
                    elements: vec![two(), two()],
                }),
            },
            // hash literal
            Test {
                input: Node::Expression(Expression::HashLiteral {
                    token: Token::LBrace,
                    pairs: [(one(), one()), (one(), one())].into_iter().collect(),
                }),
                expected: Node::Expression(Expression::HashLiteral {
                    token: Token::LBrace,
                    pairs: [(two(), two()), (two(), two())].into_iter().collect(),
                }),
            },
        ];

        for tt in tests {
            let modified = modify(tt.input, turn_one_into_two);
            assert_eq!(tt.expected.to_string(), modified.to_string());
        }
    }
}
