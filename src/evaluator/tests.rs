use std::{collections::HashMap, ops::Deref};

use super::Evaluator;
use crate::{environment::Environment, lexer::Lexer, object::Object, parser::Parser};

pub(crate) struct Test<'a, T> {
    pub(crate) input: &'a str,
    pub(crate) expected: T,
}

#[test]
fn eval_integer_expression() {
    let tests: &[Test<'_, i64>] = &[
        Test {
            input: "5",
            expected: 5,
        },
        Test {
            input: "10",
            expected: 10,
        },
        Test {
            input: "-5",
            expected: -5,
        },
        Test {
            input: "-10",
            expected: -10,
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10",
            expected: 10,
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2",
            expected: 32,
        },
        Test {
            input: "-50 + 100 + -50",
            expected: 0,
        },
        Test {
            input: "5 * 2 + 10",
            expected: 20,
        },
        Test {
            input: "5 + 2 * 10",
            expected: 25,
        },
        Test {
            input: "20 + 2 * -10",
            expected: 0,
        },
        Test {
            input: "50 / 2 * 2 + 10",
            expected: 60,
        },
        Test {
            input: "2 * (5 + 10)",
            expected: 30,
        },
        Test {
            input: "3 * 3 * 3 + 10",
            expected: 37,
        },
        Test {
            input: "3 * (3 * 3) + 10",
            expected: 37,
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: 50,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_integer_object(&evaluated, tt.expected);
    }
}

#[test]
fn eval_boolean_expression() {
    let tests: &[Test<'_, bool>] = &[
        Test {
            input: "true",
            expected: true,
        },
        Test {
            input: "false",
            expected: false,
        },
        Test {
            input: "1 < 2",
            expected: true,
        },
        Test {
            input: "1 > 2",
            expected: false,
        },
        Test {
            input: "1 < 1",
            expected: false,
        },
        Test {
            input: "1 > 1",
            expected: false,
        },
        Test {
            input: "1 == 1",
            expected: true,
        },
        Test {
            input: "1 != 1",
            expected: false,
        },
        Test {
            input: "1 == 2",
            expected: false,
        },
        Test {
            input: "1 != 2",
            expected: true,
        },
        Test {
            input: "true == true",
            expected: true,
        },
        Test {
            input: "false == false",
            expected: true,
        },
        Test {
            input: "true == false",
            expected: false,
        },
        Test {
            input: "true != false",
            expected: true,
        },
        Test {
            input: "false != true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == false",
            expected: false,
        },
        Test {
            input: "(1 > 2) == true",
            expected: false,
        },
        Test {
            input: "(1 > 2) == false",
            expected: true,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}

#[test]
fn bang_operator() {
    let tests: &[Test<'_, bool>] = &[
        Test {
            input: "!true",
            expected: false,
        },
        Test {
            input: "!false",
            expected: true,
        },
        Test {
            input: "!5",
            expected: false,
        },
        Test {
            input: "!!true",
            expected: true,
        },
        Test {
            input: "!!false",
            expected: false,
        },
        Test {
            input: "!!5",
            expected: true,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}

#[test]
fn if_else_expression() {
    let tests: &[Test<'_, Option<i64>>] = &[
        Test {
            input: "if (true) { 10 }",
            expected: Some(10),
        },
        Test {
            input: "if (false) { 10 }",
            expected: None,
        },
        Test {
            input: "if (1) { 10 }",
            expected: Some(10),
        },
        Test {
            input: "if (1 < 2) { 10 }",
            expected: Some(10),
        },
        Test {
            input: "if (1 > 2) { 10 }",
            expected: None,
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Some(20),
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Some(10),
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        if let Some(integer) = tt.expected {
            test_integer_object(&evaluated, integer);
        } else {
            test_null_object(evaluated);
        }
    }
}

#[test]
fn return_statements() {
    let tests: &[Test<'_, i64>] = &[
        Test {
            input: "return 10;",
            expected: 10,
        },
        Test {
            input: "return 10; 9;",
            expected: 10,
        },
        Test {
            input: "return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: "9; return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: "
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
}
",
            expected: 10,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_integer_object(&evaluated, tt.expected);
    }
}

#[test]
fn error_handling() {
    let tests: &[Test<'_, &'_ str>] = &[
        Test {
            input: "5 + true;",
            expected: "type mismatch: integer + boolean",
        },
        Test {
            input: "5 + true; 5;",
            expected: "type mismatch: integer + boolean",
        },
        Test {
            input: "-true",
            expected: "unknown operator: -boolean",
        },
        Test {
            input: "true + false;",
            expected: "unknown operator: boolean + boolean",
        },
        Test {
            input: "true + false + true + false;",
            expected: "unknown operator: boolean + boolean",
        },
        Test {
            input: "5; true + false; 5",
            expected: "unknown operator: boolean + boolean",
        },
        Test {
            input: r#""Hello" - "World""#,
            expected: "unknown operator: string - string",
        },
        Test {
            input: "if (10 > 1) { true + false; }",
            expected: "unknown operator: boolean + boolean",
        },
        Test {
            input: "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}
",
            expected: "unknown operator: boolean + boolean",
        },
        Test {
            input: "foobar",
            expected: "identifier not found: foobar",
        },
        Test {
            input: r#"{"name": "Monkey"}[fn(x) { x }];"#,
            expected: "unusable as hash key: function",
        },
        Test {
            input: "999[1]",
            expected: "index operator not supported: integer",
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        match evaluated {
            Object::Error(e) => assert_eq!(tt.expected, e),
            _ => panic!("no error object returned. got {}", evaluated.ty()),
        }
    }
}

#[test]
fn let_statements() {
    let tests: &[Test<'_, i64>] = &[
        Test {
            input: "let a = 5; a;",
            expected: 5,
        },
        Test {
            input: "let a = 5 * 5; a;",
            expected: 25,
        },
        Test {
            input: "let a = 5; let b = a; b;",
            expected: 5,
        },
        Test {
            input: "let a = 5; let b = a; let c = a + b + 5; c;",
            expected: 15,
        },
    ];

    for tt in tests {
        test_integer_object(&test_eval(tt.input), tt.expected);
    }
}

#[test]
fn function_object() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input);
    let Object::Function {
        parameters, body, ..
    } = evaluated
    else {
        panic!("object is not Function. got {}", evaluated.ty());
    };

    assert_eq!(1, parameters.len());
    assert_eq!("x", parameters[0].to_string());

    let expected_body = "(x + 2)";
    assert_eq!(expected_body, body.to_string());
}

#[test]
fn function_application() {
    let tests: &[Test<'_, i64>] = &[
        Test {
            input: "let identity = fn(x) { x; }; identity(5);",
            expected: 5,
        },
        Test {
            input: "let identity = fn(x) { return x; }; identity(5);",
            expected: 5,
        },
        Test {
            input: "let double = fn(x) { x * 2; }; double(5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5, 5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            expected: 20,
        },
        Test {
            input: "fn(x) { x; }(5)",
            expected: 5,
        },
    ];

    for tt in tests {
        test_integer_object(&test_eval(tt.input), tt.expected);
    }
}

#[test]
fn enclosing_environments() {
    let input = "
let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;

  first + second + third;
};

ourFunction(20) + first + second;";

    test_integer_object(&test_eval(input), 70);
}

#[test]
fn closures() {
    let input = "
let new_adder = fn(x) {
    fn(y) { x + y };
}

let add_two = new_adder(2);
add_two(2);
";

    test_integer_object(&test_eval(input), 4);
}

#[test]
fn string_literal() {
    let input = r#""Hello World!"#;
    let evaluated = test_eval(input);
    let Object::String(s) = evaluated else {
        panic!("object is not String. got {}", evaluated.ty());
    };
    assert_eq!("Hello World!", s.deref());
}

#[test]
fn string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;
    let evaluated = test_eval(input);
    let Object::String(s) = evaluated else {
        panic!("object is not String. got {}", evaluated.ty());
    };
    assert_eq!("Hello World!", s.deref());
}

#[test]
fn builtin_functions() {
    use std::any::Any;

    let tests: &[Test<'_, Option<Box<dyn Any>>>] = &[
        Test {
            input: r#"len("")"#,
            expected: Some(Box::new(0)),
        },
        Test {
            input: r#"len("four")"#,
            expected: Some(Box::new(4)),
        },
        Test {
            input: r#"len("hello world")"#,
            expected: Some(Box::new(11)),
        },
        Test {
            input: "len(1)",
            expected: Some(Box::new("argument to `len` not supported, got integer")),
        },
        Test {
            input: r#"len("one", "two")"#,
            expected: Some(Box::new("wrong number of arguments. got=2, want=1")),
        },
        Test {
            input: "len([1, 2, 3])",
            expected: Some(Box::new(3)),
        },
        Test {
            input: "len([])",
            expected: Some(Box::new(0)),
        },
        Test {
            input: "first([1, 2, 3])",
            expected: Some(Box::new(1)),
        },
        Test {
            input: "first([])",
            expected: None,
        },
        Test {
            input: "first(1)",
            expected: Some(Box::new("argument to `first` must be array, got integer")),
        },
        Test {
            input: "last([1, 2, 3])",
            expected: Some(Box::new(3)),
        },
        Test {
            input: "last([])",
            expected: None,
        },
        Test {
            input: "last(1)",
            expected: Some(Box::new("argument to `last` must be array, got integer")),
        },
        Test {
            input: "rest([1, 2, 3])",
            expected: Some(Box::new(vec![2, 3])),
        },
        Test {
            input: "rest([])",
            expected: None,
        },
        Test {
            input: "push([], 1)",
            expected: Some(Box::new(vec![1])),
        },
        Test {
            input: "push(1, 1)",
            expected: Some(Box::new("argument to `push` must be array, got integer")),
        },
        Test {
            input: r#"puts("hello", "world!")"#,
            expected: None,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);

        if let Some(expected) = &tt.expected {
            if let Some(expected) = expected.downcast_ref::<i32>() {
                test_integer_object(&evaluated, *expected as i64);
            } else if let Some(expected) = expected.downcast_ref::<i64>() {
                test_integer_object(&evaluated, *expected);
            } else if let Some(expected) = expected.downcast_ref::<&str>() {
                let Object::Error(err) = evaluated else {
                    panic!("object is not Error. got= {}", evaluated.ty());
                };
                assert_eq!(*expected, &err);
            } else if let Some(expected) = expected.downcast_ref::<String>() {
                let Object::Error(err) = evaluated else {
                    panic!("object is not Error. got= {}", evaluated.ty());
                };
                assert_eq!(expected, &err);
            } else if let Some(expected) = expected.downcast_ref::<Vec<i32>>() {
                let Object::Array(elements) = evaluated else {
                    panic!("obj not Array. got={}", evaluated.ty());
                };

                assert_eq!(expected.len(), elements.len());
                for (i, expected_elem) in expected.iter().enumerate() {
                    test_integer_object(&elements[i], *expected_elem as i64);
                }
            } else {
                panic!("typeid {:?}", expected.as_ref().type_id());
            }
        } else {
            test_null_object(evaluated);
        }
    }
}

#[test]
fn array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let evaluated = test_eval(input);
    let Object::Array(elements) = evaluated else {
        panic!("object is not Array. got {}", evaluated.ty());
    };
    assert_eq!(3, elements.len());

    use std::mem;
    test_integer_object(&elements[0], 1);
    test_integer_object(&elements[1], 4);
    test_integer_object(&elements[2], 6);
}

#[test]
fn array_index_expression() {
    let tests: &[Test<'_, Option<i64>>] = &[
        Test {
            input: "[1, 2, 3][0]",
            expected: Some(1),
        },
        Test {
            input: "[1, 2, 3][1]",
            expected: Some(2),
        },
        Test {
            input: "[1, 2, 3][2]",
            expected: Some(3),
        },
        Test {
            input: "let i = 0; [1][i];",
            expected: Some(1),
        },
        Test {
            input: "[1, 2, 3][1 + 1];",
            expected: Some(3),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[2];",
            expected: Some(3),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            expected: Some(6),
        },
        Test {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            expected: Some(2),
        },
        Test {
            input: "[1, 2, 3][3]",
            expected: None,
        },
        Test {
            input: "[1, 2, 3][-1]",
            expected: None,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        if let Some(integer) = tt.expected {
            test_integer_object(&evaluated, integer);
        } else {
            test_null_object(evaluated);
        }
    }
}

#[test]
fn hash_literals() {
    let input = r#"let two = "two";
	{
		"one": 10 - 9,
		two: 1 + 1,
		"thr" + "ee": 6 / 2,
		4: 4,
		true: 5,
		false: 6
	}"#;

    let evaluated = test_eval(input);
    let Object::Hash(pairs) = evaluated else {
        panic!("Eval didn't return Hash. got={}", evaluated.ty());
    };

    let expected = {
        let mut m: HashMap<Object, i64> = HashMap::new();
        m.extend([
            ("one".into(), 1),
            ("two".into(), 2),
            ("three".into(), 3),
            (Object::Integer(4), 4),
            (Object::Boolean(true), 5),
            (Object::Boolean(false), 6),
        ]);
        m
    };

    assert_eq!(expected.len(), pairs.len());

    for (expected_key, expeced_value) in expected {
        let value = pairs.get(&expected_key).cloned().unwrap();
        test_integer_object(&value, expeced_value)
    }
}

#[test]
fn hash_index_expression() {
    let tests: &[Test<'_, Option<i64>>] = &[
        Test {
            input: r#"{"foo": 5}["foo"]"#,
            expected: Some(5),
        },
        Test {
            input: r#"{"foo": 5}["bar"]"#,
            expected: None,
        },
        Test {
            input: r#"let key = "foo"; {"foo": 5}[key]"#,
            expected: Some(5),
        },
        Test {
            input: r#"{}["foo"]"#,
            expected: None,
        },
        Test {
            input: "{5: 5}[5]",
            expected: Some(5),
        },
        Test {
            input: "{true: 5}[true]",
            expected: Some(5),
        },
        Test {
            input: "{false: 5}[false]",
            expected: Some(5),
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        if let Some(integer) = tt.expected {
            test_integer_object(&evaluated, integer);
        } else {
            test_null_object(evaluated);
        }
    }
}

pub(crate) fn test_eval(input: &str) -> Object {
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    let env = Environment::new(None);

    program.eval(env).unwrap_or_else(Object::Error)
}

fn test_integer_object(obj: &Object, expected: i64) {
    match obj {
        &Object::Integer(i) => assert_eq!(expected, i),
        _ => panic!("object is not Integer. got {}", obj.ty()),
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(b) => assert_eq!(expected, b),
        _ => panic!("object is not Boolean. got {}", obj.ty()),
    }
}

fn test_null_object(obj: Object) {
    match obj {
        Object::Null => (),
        _ => panic!("object is not NULL. got {}", obj.ty()),
    }
}
