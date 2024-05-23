use crate::ast::{Expression, Tokenizer};
use crate::{ast::Statement, lexer::Lexer};

use super::*;

type Any = Box<dyn std::any::Any>;

#[test]
fn let_statements() {
    struct Test<'a> {
        input: &'a str,
        expected_identifier: &'a str,
        expected_value: Any,
    }
    let tests = &[
        Test {
            input: "let x = 5;",
            expected_identifier: "x",
            expected_value: Box::new(5),
        },
        Test {
            input: "let y = true;",
            expected_identifier: "y",
            expected_value: Box::new(true),
        },
        Test {
            input: "let foobar = y;",
            expected_identifier: "foobar",
            expected_value: Box::new("y"),
        },
    ];

    for tt in tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        test_let_statement(stmt, tt.expected_identifier);

        let Statement::Let { value, .. } = stmt else {
            panic!("not let statement got {stmt:?}");
        };
        test_literal_expression(value, &tt.expected_value);
    }
}

#[test]
fn return_statements() {
    struct Test<'a> {
        input: &'a str,
        expected_value: Any,
    }

    let tests = &[
        Test {
            input: "return 5;",
            expected_value: Box::new(5),
        },
        Test {
            input: "return true;",
            expected_value: Box::new(true),
        },
        Test {
            input: "return foobar;",
            expected_value: Box::new("foobar"),
        },
    ];

    for tt in tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        assert_eq!("return", stmt.token_literal());

        let Statement::Return { return_value, .. } = stmt else {
            panic!("not return statement got {stmt:?}");
        };

        test_literal_expression(return_value, &tt.expected_value);
    }
}

#[test]
fn identifier_expression() {
    let input = "foobar;";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: ident, ..
    } = stmt
    else {
        panic!("not expression statement got {stmt:?}");
    };

    assert_eq!("foobar", ident.token_literal());
    let Expression::Identifier { value, .. } = ident else {
        panic!("not identifier expression got {ident:?}");
    };
    assert_eq!("foobar", value);
}

#[test]
fn integer_literal_expression() {
    let input = "5;";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: literal,
        ..
    } = stmt
    else {
        panic!("not expression statement got {stmt:?}");
    };

    let Expression::IntegerLiteral { value, .. } = literal else {
        panic!("not integer literal got {literal:?}");
    };
    assert_eq!(5, *value);

    assert_eq!("5", literal.token_literal());
}

#[test]
fn parsing_prefix_expression() {
    struct Test<'a> {
        input: &'a str,
        operator: &'a str,
        value: Any,
    }
    let prefix_tests = &[
        Test {
            input: "!5",
            operator: "!",
            value: Box::new(5),
        },
        Test {
            input: "-15",
            operator: "-",
            value: Box::new(15),
        },
        Test {
            input: "!foobar",
            operator: "!",
            value: Box::new("foobar"),
        },
        Test {
            input: "-foobar",
            operator: "-",
            value: Box::new("foobar"),
        },
        Test {
            input: "!true",
            operator: "!",
            value: Box::new(true),
        },
        Test {
            input: "!false",
            operator: "!",
            value: Box::new(false),
        },
    ];

    for tt in prefix_tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        let Statement::Expression { expression, .. } = stmt else {
            panic!("not expression statement got {stmt:?}");
        };

        let Expression::Prefix {
            operator, right, ..
        } = expression
        else {
            panic!("not prefix expression got {expression:?}");
        };

        assert_eq!(tt.operator, operator);
        test_literal_expression(right, &tt.value)
    }
}

#[test]
fn parsing_infix_expression() {
    struct Test {
        input: &'static str,
        left_value: Any,
        operator: &'static str,
        right_value: Any,
    }
    let infix_tests = &[
        Test {
            input: "5 + 5;",
            left_value: Box::new(5),
            operator: "+",
            right_value: Box::new(5),
        },
        Test {
            input: "5 - 5;",
            left_value: Box::new(5),
            operator: "-",
            right_value: Box::new(5),
        },
        Test {
            input: "5 * 5;",
            left_value: Box::new(5),
            operator: "*",
            right_value: Box::new(5),
        },
        Test {
            input: "5 / 5;",
            left_value: Box::new(5),
            operator: "/",
            right_value: Box::new(5),
        },
        Test {
            input: "5 > 5;",
            left_value: Box::new(5),
            operator: ">",
            right_value: Box::new(5),
        },
        Test {
            input: "5 < 5;",
            left_value: Box::new(5),
            operator: "<",
            right_value: Box::new(5),
        },
        Test {
            input: "5 == 5;",
            left_value: Box::new(5),
            operator: "==",
            right_value: Box::new(5),
        },
        Test {
            input: "5 != 5;",
            left_value: Box::new(5),
            operator: "!=",
            right_value: Box::new(5),
        },
        Test {
            input: "foobar + barfoo;",
            left_value: Box::new("foobar"),
            operator: "+",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar + barfoo;",
            left_value: Box::new("foobar"),
            operator: "+",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar + barfoo;",
            left_value: Box::new("foobar"),
            operator: "+",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar - barfoo;",
            left_value: Box::new("foobar"),
            operator: "-",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar * barfoo;",
            left_value: Box::new("foobar"),
            operator: "*",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar / barfoo;",
            left_value: Box::new("foobar"),
            operator: "/",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar > barfoo;",
            left_value: Box::new("foobar"),
            operator: ">",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar < barfoo;",
            left_value: Box::new("foobar"),
            operator: "<",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar == barfoo;",
            left_value: Box::new("foobar"),
            operator: "==",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "foobar != barfoo;",
            left_value: Box::new("foobar"),
            operator: "!=",
            right_value: Box::new("barfoo"),
        },
        Test {
            input: "true == true",
            left_value: Box::new(true),
            operator: "==",
            right_value: Box::new(true),
        },
        Test {
            input: "true != false",
            left_value: Box::new(true),
            operator: "!=",
            right_value: Box::new(false),
        },
        Test {
            input: "false == false",
            left_value: Box::new(false),
            operator: "==",
            right_value: Box::new(false),
        },
    ];

    for tt in infix_tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        let Statement::Expression { expression, .. } = stmt else {
            panic!("not expression statement got {stmt:?}");
        };

        test_infix_expression(expression, &tt.left_value, tt.operator, &tt.right_value);
    }
}

#[test]
fn operator_precedence_parsing() {
    struct Test {
        input: &'static str,
        expected: &'static str,
    }

    let tests = &[
        Test {
            input: "-a * b",
            expected: "((-a) * b)",
        },
        Test {
            input: "!-a",
            expected: "(!(-a))",
        },
        Test {
            input: "a + b + c",
            expected: "((a + b) + c)",
        },
        Test {
            input: "a + b - c",
            expected: "((a + b) - c)",
        },
        Test {
            input: "a * b * c",
            expected: "((a * b) * c)",
        },
        Test {
            input: "a * b / c",
            expected: "((a * b) / c)",
        },
        Test {
            input: "a + b / c",
            expected: "(a + (b / c))",
        },
        Test {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        Test {
            input: "3 + 4; -5 * 5",
            expected: "(3 + 4)((-5) * 5)",
        },
        Test {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4))",
        },
        Test {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4))",
        },
        Test {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        Test {
            input: "true",
            expected: "true",
        },
        Test {
            input: "false",
            expected: "false",
        },
        Test {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)",
        },
        Test {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)",
        },
        Test {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4)",
        },
        Test {
            input: "(5 + 5) * 2",
            expected: "((5 + 5) * 2)",
        },
        Test {
            input: "2 / (5 + 5)",
            expected: "(2 / (5 + 5))",
        },
        Test {
            input: "(5 + 5) * 2 * (5 + 5)",
            expected: "(((5 + 5) * 2) * (5 + 5))",
        },
        Test {
            input: "-(5 + 5)",
            expected: "(-(5 + 5))",
        },
        Test {
            input: "!(true == true)",
            expected: "(!(true == true))",
        },
        Test {
            input: "a + add(b * c) + d",
            expected: "((a + add((b * c))) + d)",
        },
        Test {
            input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        Test {
            input: "add(a + b + c * d / f + g)",
            expected: "add((((a + b) + ((c * d) / f)) + g))",
        },
        Test {
            input: "a * [1, 2, 3, 4][b * c] * d",
            expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        Test {
            input: "add(a * b[2], b[1], 2 * [1, 2][1])",
            expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    ];

    for tt in tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(tt.expected, program.to_string());
    }
}

#[test]
fn boolean_expression() {
    struct Test<'a> {
        input: &'a str,
        expected_boolean: bool,
    }

    let tests = &[
        Test {
            input: "true;",
            expected_boolean: true,
        },
        Test {
            input: "false;",
            expected_boolean: false,
        },
    ];

    for tt in tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        let Statement::Expression {
            expression: boolean,
            ..
        } = stmt
        else {
            panic!("not expression statement got {stmt:?}");
        };

        let Expression::Boolean { value, .. } = boolean else {
            panic!("not boolean expression got {boolean:?}");
        };
        assert_eq!(tt.expected_boolean, *value);
    }
}

#[test]
fn if_expression() {
    let input = "if (x < y) { x }";
    let l = Lexer::new(input.as_bytes());

    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression { expression, .. } = stmt else {
        panic!("not expression statement got {stmt:?}");
    };

    let Expression::If {
        condition,
        consequence,
        alternative,
        ..
    } = expression
    else {
        panic!("not if expression got {expression:?}");
    };

    test_infix_expression(
        condition,
        &(Box::new("x") as Any),
        "<",
        &(Box::new("y") as Any),
    );

    let Statement::Block {
        statements: consequence,
        ..
    } = consequence.as_ref()
    else {
        panic!("not block statement got {consequence:?}");
    };

    assert_eq!(1, consequence.len());
    let Statement::Expression { expression, .. } = &consequence[0] else {
        panic!("not expression statement got {:?}", consequence[0]);
    };

    test_identifier(expression, "x");

    assert!(alternative.is_none());
}

#[test]
fn if_else_expression() {
    let input = "if (x < y) { x} else { y }";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression { expression, .. } = stmt else {
        panic!("not expression statment got {stmt:?}");
    };

    let Expression::If {
        condition,
        consequence,
        alternative,
        ..
    } = expression
    else {
        panic!("not if expression got {expression:?}");
    };

    test_infix_expression(
        condition,
        &(Box::new("x") as Any),
        "<",
        &(Box::new("y") as Any),
    );

    let Statement::Block {
        statements: consequence,
        ..
    } = consequence.as_ref()
    else {
        panic!("not block statement got  {consequence:?}");
    };
    assert_eq!(1, consequence.len());

    let Statement::Expression {
        expression: consequence,
        ..
    } = &consequence[0]
    else {
        panic!("not expression statement, got {:?}", consequence[0]);
    };

    test_identifier(consequence, "x");

    assert!(alternative.is_some());

    let Statement::Block {
        statements: alternative,
        ..
    } = alternative.as_ref().unwrap().as_ref()
    else {
        panic!("not block statemnt got {:?}", alternative);
    };

    assert_eq!(1, alternative.len());
    let Statement::Expression {
        expression: alternative,
        ..
    } = &alternative[0]
    else {
        panic!("not expression statement");
    };

    test_identifier(alternative, "y");
}

#[test]
fn function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: function,
        ..
    } = stmt
    else {
        panic!("not expression statement got {stmt:?}");
    };

    let Expression::FunctionLiteral {
        parameters, body, ..
    } = function
    else {
        panic!("not function literal got {function:?}");
    };

    assert_eq!(2, parameters.len());
    test_literal_expression(&parameters[0], &(Box::new("x") as Any));
    test_literal_expression(&parameters[1], &(Box::new("y") as Any));

    let Statement::Block { statements, .. } = body.as_ref() else {
        panic!("not block statement got {body:?}");
    };
    assert_eq!(1, statements.len());

    let body_stmt = &statements[0];
    let Statement::Expression { expression, .. } = body_stmt else {
        panic!("not expression statement got {body_stmt:?}");
    };
    test_infix_expression(
        expression,
        &(Box::new("x") as Any),
        "+",
        &(Box::new("y") as Any),
    );
}

#[test]
fn function_parameter_parsing() {
    struct Test<'a> {
        input: &'a str,
        expected_params: Vec<&'a str>,
    }
    let tests = &[
        Test {
            input: "fn () {};",
            expected_params: vec![],
        },
        Test {
            input: "fn (x) {};",
            expected_params: vec!["x"],
        },
        Test {
            input: "fn (x, y, z) {};",
            expected_params: vec!["x", "y", "z"],
        },
    ];

    for tt in tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        let stmt = &program.statements[0];
        let Statement::Expression {
            expression: function,
            ..
        } = stmt
        else {
            panic!("not expression statement got {stmt:?}");
        };

        let Expression::FunctionLiteral { parameters, .. } = function else {
            panic!("not function literal got {function:?}");
        };

        assert_eq!(tt.expected_params.len(), parameters.len());

        for (i, &ident) in tt.expected_params.iter().enumerate() {
            test_literal_expression(&parameters[i], &(Box::new(ident) as Any));
        }
    }
}

#[test]
fn call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("not expression statement got {stmt:?}");
    };

    let Expression::Call {
        function,
        arguments,
        ..
    } = exp
    else {
        panic!("not call expression got {exp:?}");
    };

    test_identifier(function, "add");

    assert_eq!(3, arguments.len());
    test_literal_expression(&arguments[0], &(Box::new(1) as Any));
    test_infix_expression(
        &arguments[1],
        &(Box::new(2) as Any),
        "*",
        &(Box::new(3) as Any),
    );
    test_infix_expression(
        &arguments[2],
        &(Box::new(4) as Any),
        "+",
        &(Box::new(5) as Any),
    );
}

#[test]
fn call_expression_parameter_parsing() {
    struct Test<'a> {
        input: &'a str,
        expected_ident: &'a str,
        expected_args: &'a [&'a str],
    }

    let tests = &[
        Test {
            input: "add();",
            expected_ident: "add",
            expected_args: &[],
        },
        Test {
            input: "add(1);",
            expected_ident: "add",
            expected_args: &["1"],
        },
        Test {
            input: "add(1, 2 * 3, 4 + 5);",
            expected_ident: "add",
            expected_args: &["1", "(2 * 3)", "(4 + 5)"],
        },
    ];

    for tt in tests {
        let l = Lexer::new(tt.input.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        let stmt = &program.statements[0];
        let Statement::Expression {
            expression: exp, ..
        } = stmt
        else {
            panic!("not expression statement got {stmt:?}");
        };

        let Expression::Call {
            function,
            arguments,
            ..
        } = exp
        else {
            panic!("not call expression got {exp:?}");
        };

        test_identifier(function, tt.expected_ident);

        assert_eq!(tt.expected_args.len(), arguments.len());

        for (i, &arg) in tt.expected_args.iter().enumerate() {
            assert_eq!(arg, arguments[i].to_string());
        }
    }
}

#[test]
fn string_literal_expression() {
    let input = "\"hello world\"";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp not *ast.StringLiteral. got= {stmt:?}");
    };
    let Expression::StringLiteral { value, .. } = exp else {
        panic!("exp not *ast.StringLiteral. got= {exp:?}");
    };
    assert_eq!("hello world", value);
}

#[test]
fn parsing_empty_array_literals() {
    let input = "[]";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp not ast.ArrayLiteral. got= {stmt:?}");
    };

    let Expression::ArrayLiteral { elements, .. } = exp else {
        panic!("exp not ast.ArrayLiteral. got= {exp:?}");
    };
    assert_eq!(0, elements.len());
}

#[test]
fn parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp not ast.ArrayLiteral. got= {stmt:?}");
    };

    let Expression::ArrayLiteral { elements, .. } = exp else {
        panic!("exp not ast.ArrayLiteral. got= {exp:?}");
    };

    assert_eq!(3, elements.len());

    test_integer_literal(&elements[0], 1);
    test_infix_expression(
        &elements[1],
        &(Box::new(2) as Any),
        "*",
        &(Box::new(2) as Any),
    );
    test_infix_expression(
        &elements[2],
        &(Box::new(3) as Any),
        "+",
        &(Box::new(3) as Any),
    );
}

#[test]
fn parsing_index_expression() {
    let input = "myArray[1 + 1]";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp not *ast.IndexExpression. got {stmt:?}");
    };

    let Expression::Index { left, index, .. } = exp else {
        panic!("exp not *ast.IndexExpression. got {exp:?}");
    };

    test_identifier(left, "myArray");
    test_infix_expression(index, &(Box::new(1) as Any), "+", &(Box::new(1) as Any))
}

#[test]
fn parsing_empty_hash_literal() {
    let input = "{}";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp is not ast.HashLiteral. got={stmt:?}");
    };
    let Expression::HashLiteral { pairs, .. } = exp else {
        panic!("exp is not ast.HashLiteral. got={exp:?}");
    };

    assert_eq!(0, pairs.len());
}

#[test]
fn parsing_hash_literal_string_keys() {
    let input = r#"{"one": 1, "two": 2, "three": 3}"#;

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp is not ast.HashLiteral. got={stmt:?}");
    };
    let Expression::HashLiteral { pairs, .. } = exp else {
        panic!("exp is not ast.HashLiteral. got={exp:?}");
    };

    let expected = {
        let mut m: HashMap<String, i64> = HashMap::new();
        m.extend([
            ("one".to_string(), 1),
            ("two".to_string(), 2),
            ("three".to_string(), 3),
        ]);
        m
    };

    assert_eq!(expected.len(), pairs.len());

    for (key, value) in pairs {
        let Expression::StringLiteral { value: key, .. } = key else {
            panic!("key is not ast.StringLiteral. got={key:?}");
        };

        let expected_value = expected.get(key).copied().unwrap();
        test_integer_literal(value, expected_value);
    }
}

#[test]
fn parsing_hash_literal_boolean_keys() {
    let input = "{true: 1, false: 0}";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp is not ast.HashLiteral. got={stmt:?}");
    };
    let Expression::HashLiteral { pairs, .. } = exp else {
        panic!("exp is not ast.HashLiteral. got={exp:?}");
    };

    let expected = {
        let mut m: HashMap<bool, i64> = HashMap::new();
        m.extend([(true, 1), (false, 0)]);
        m
    };

    assert_eq!(expected.len(), pairs.len());

    for (key, value) in pairs {
        let Expression::Boolean { value: key, .. } = key else {
            panic!("key is not ast.BooleanLiteral. got={key:?}");
        };

        let expected_value = expected.get(key).copied().unwrap();
        test_integer_literal(value, expected_value);
    }
}

#[test]
fn parsing_hash_literal_integer_keys() {
    let input = "{1: 1, 2: 2, 3: 3}";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp is not ast.HashLiteral. got={stmt:?}");
    };
    let Expression::HashLiteral { pairs, .. } = exp else {
        panic!("exp is not ast.HashLiteral. got={exp:?}");
    };

    let expected = {
        let mut m: HashMap<i64, i64> = HashMap::new();
        m.extend([(1, 1), (2, 2), (3, 3)]);
        m
    };

    assert_eq!(expected.len(), pairs.len());

    for (key, value) in pairs {
        let Expression::IntegerLiteral { value: key, .. } = key else {
            panic!("key is not ast.IntegerLiteral. got={key:?}");
        };

        let expected_value = expected.get(key).copied().unwrap();
        test_integer_literal(value, expected_value);
    }
}

#[test]
fn parsing_hash_literal_with_expressons() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    let stmt = &program.statements[0];
    let Statement::Expression {
        expression: exp, ..
    } = stmt
    else {
        panic!("exp is not ast.HashLiteral. got={stmt:?}");
    };
    let Expression::HashLiteral { pairs, .. } = exp else {
        panic!("exp is not ast.HashLiteral. got={exp:?}");
    };

    let tests = {
        type TestFn = dyn Fn(&Expression);
        let mut m: HashMap<String, Box<TestFn>> = HashMap::new();
        m.insert(
            "one".to_string(),
            Box::new(|e| {
                test_infix_expression(e, &(Box::new(0) as Any), "+", &(Box::new(1) as Any))
            }),
        );
        m.insert(
            "two".to_string(),
            Box::new(|e| {
                test_infix_expression(e, &(Box::new(10) as Any), "-", &(Box::new(8) as Any))
            }),
        );
        m.insert(
            "three".to_string(),
            Box::new(|e| {
                test_infix_expression(e, &(Box::new(15) as Any), "/", &(Box::new(5) as Any))
            }),
        );
        m
    };

    assert_eq!(tests.len(), pairs.len());

    for (key, value) in pairs {
        let Expression::StringLiteral { value: key, .. } = key else {
            panic!("key is not ast.StringLiteral. got={key:?}");
        };

        let test_func = tests.get(key).unwrap();
        test_func(value);
    }
}

#[test]
fn parsing_marco_literal() {
    let input = "macro(x, y) { x + y; }";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Expression { expression, .. } = stmt else {
        panic!("statement is not ast.ExpressionStatement. got={stmt:?}")
    };

    let Expression::MacroLiteral {
        parameters, body, ..
    } = expression
    else {
        panic!("expression is not ast.MacroLiteral. got= {expression:?}")
    };

    assert_eq!(2, parameters.len());

    test_literal_expression(&parameters[0], &(Box::new("x") as Any));
    test_literal_expression(&parameters[1], &(Box::new("y") as Any));

    let Statement::Block { statements, .. } = body.as_ref() else {
        panic!("body not block statement {body:?}")
    };
    assert_eq!(1, statements.len());

    let Statement::Expression { expression, .. } = &statements[0] else {
        panic!(
            "macro body stmt is not ast.ExpressionStatement. got={:?}",
            &statements[0]
        );
    };
    test_infix_expression(
        expression,
        &(Box::new("x") as Any),
        "+",
        &(Box::new("y") as Any),
    );
}

#[test]
fn function_literal_with_name() {
    let input = "let myFunction = fn() { };";

    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parse_errors(&p);

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    let Statement::Let { value, .. } = stmt else {
        panic!("program.Statements[0] is not ast.LetStatement. got={stmt:?}");
    };

    let Expression::FunctionLiteral { name, .. } = value else {
        panic!("stmt.Value is not ast.FunctionLiteral. got={value:?}");
    };

    assert_eq!("myFunction", name);
}

fn test_infix_expression(
    exp: &Expression,
    expected_left: &Any,
    expected_operator: &str,
    expected_right: &Any,
) {
    let Expression::Infix {
        left,
        operator,
        right,
        ..
    } = exp
    else {
        panic!("exp not infix expression, got {exp:?}");
    };

    test_literal_expression(left, expected_left);
    assert_eq!(expected_operator, operator);
    test_literal_expression(right, expected_right);
}

fn test_literal_expression(exp: &Expression, expected: &Any) {
    if let Some(expected) = expected.downcast_ref::<i32>() {
        test_integer_literal(exp, *expected as i64);
    } else if let Some(expected) = expected.downcast_ref::<i64>() {
        test_integer_literal(exp, *expected);
    } else if let Some(expected) = expected.downcast_ref::<&'static str>() {
        test_identifier(exp, expected);
    } else if let Some(expected) = expected.downcast_ref::<String>() {
        test_identifier(exp, expected)
    } else if let Some(expected) = expected.downcast_ref::<bool>() {
        test_boolean_literal(exp, *expected)
    } else {
        panic!("type_id {:?}", expected.type_id());
    }
}

fn test_boolean_literal(exp: &Expression, expected: bool) {
    let Expression::Boolean { value, .. } = exp else {
        panic!("not boolean expression got {exp:?}");
    };

    assert_eq!(expected, *value);
    assert_eq!(format!("{}", expected), exp.token_literal());
}

fn test_integer_literal(il: &Expression, value: i64) {
    let Expression::IntegerLiteral { value: integ, .. } = il else {
        panic!("not integer literal, got {il:?}");
    };

    assert_eq!(value, *integ);
    assert_eq!(value.to_string(), il.token_literal())
}

fn test_identifier(exp: &Expression, value: &str) {
    let Expression::Identifier { value: ident, .. } = exp else {
        panic!("exp not identifier, got {exp:?}");
    };

    assert_eq!(value, ident);
    assert_eq!(value, exp.token_literal());
}

fn test_let_statement(s: &Statement, expected: &str) {
    assert_eq!("let", s.token_literal());

    let Statement::Let { name, .. } = s else {
        panic!("not let statement, got {s:?}");
    };

    let Expression::Identifier { value, .. } = name else {
        panic!("not identifier expression got {name:?}");
    };
    assert_eq!(expected, value);

    assert_eq!(expected, name.token_literal());
}

fn check_parse_errors(p: &Parser) {
    let errors = p.errors();
    if errors.is_empty() {
        return;
    }

    eprintln!("parser has {} errors", errors.len());
    for msg in errors {
        eprintln!("parser error: \"{}\"", msg);
    }
    panic!();
}
