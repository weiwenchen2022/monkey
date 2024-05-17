use crate::ast::Program;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

use super::VM;

use std::any::Any;
use std::collections::HashMap;

struct VmTestCase<'a> {
    input: &'a str,
    expected: Box<dyn Any>,
}

macro_rules! run_vm_tests {
    ($tests:tt) => {
        for tt in $tests {
            let program = parse(tt.input);

            let mut comp = Compiler::new();
            comp.compile(program).expect("compiler error: ");

            let mut vm = VM::new(comp.bytecode());
            vm.run().expect("vm error: ");

            let stack_elem = vm.last_popped_stack_elem();
            test_expected_object(&tt.expected, &stack_elem)
        }
    };
}

#[test]
fn integer_arithmetic() {
    let tests = &[
        VmTestCase {
            input: "1",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "2",
            expected: Box::new(2),
        },
        VmTestCase {
            input: "1 + 2",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "1 - 2",
            expected: Box::new(-1),
        },
        VmTestCase {
            input: "1 * 2",
            expected: Box::new(2),
        },
        VmTestCase {
            input: "4 / 2",
            expected: Box::new(2),
        },
        VmTestCase {
            input: "50 / 2 * 2 + 10 - 5",
            expected: Box::new(55),
        },
        VmTestCase {
            input: "5 * (2 + 10)",
            expected: Box::new(60),
        },
        VmTestCase {
            input: "5 + 5 + 5 + 5 - 10",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "2 * 2 * 2 * 2 * 2",
            expected: Box::new(32),
        },
        VmTestCase {
            input: "5 * 2 + 10",
            expected: Box::new(20),
        },
        VmTestCase {
            input: "5 + 2 * 10",
            expected: Box::new(25),
        },
        VmTestCase {
            input: "5 * (2 + 10)",
            expected: Box::new(60),
        },
        VmTestCase {
            input: "-5",
            expected: Box::new(-5),
        },
        VmTestCase {
            input: "-10",
            expected: Box::new(-10),
        },
        VmTestCase {
            input: "-50 + 100 + -50",
            expected: Box::new(0),
        },
        VmTestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: Box::new(50),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn boolean_expression() {
    let tests = &[
        VmTestCase {
            input: "true",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "false",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "1 < 2",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "1 > 2",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "1 < 1",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "1 > 1",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "1 == 1",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "1 != 1",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "1 == 2",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "1 != 2",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "true == true",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "false == false",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "true == false",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "true != false",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "false != true",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "(1 < 2) == true",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "(1 < 2) == false",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "(1 > 2) == true",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "(1 > 2) == false",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "!true",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "!false",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "!5",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "!!true",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "!!false",
            expected: Box::new(false),
        },
        VmTestCase {
            input: "!!5",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "!(if (false) { 5; })",
            expected: Box::new(true),
        },
        VmTestCase {
            input: "if ((if (false) { 10 })) { 10 } else { 20 }",
            expected: Box::new(20),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn conditionals() {
    let tests = &[
        VmTestCase {
            input: "if (true) { 10 }",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "if (true) { 10 } else { 20 }",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "if (false) { 10 } else { 20 } ",
            expected: Box::new(20),
        },
        VmTestCase {
            input: "if (1) { 10 }",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "if (1 < 2) { 10 }",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Box::new(20),
        },
        VmTestCase {
            input: "if (1 > 2) { 10 }",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "if (false) { 10 }",
            expected: Box::new(()),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn global_let_statements() {
    let tests = &[
        VmTestCase {
            input: "let one = 1; one",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "let one = 1; let two = 2; one + two",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "let one = 1; let two = one + one; one + two",
            expected: Box::new(3),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn string_expressions() {
    let tests = &[
        VmTestCase {
            input: r#""monkey""#,
            expected: Box::new("monkey"),
        },
        VmTestCase {
            input: r#""mon" + "key""#,
            expected: Box::new("monkey"),
        },
        VmTestCase {
            input: r#""mon" + "key" + "banana""#,
            expected: Box::new("monkeybanana"),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn array_literals() {
    let tests = &[
        VmTestCase {
            input: "[]",
            expected: Box::new(&[] as &[i32]),
        },
        VmTestCase {
            input: "[1, 2, 3]",
            expected: Box::new(&[1, 2, 3] as &[i32]),
        },
        VmTestCase {
            input: "[1 + 2, 3 * 4, 5 + 6]",
            expected: Box::new(&[3, 12, 11] as &[i32]),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn hash_literals() {
    let tests = &[
        VmTestCase {
            input: "{}",
            expected: Box::new([].into_iter().collect::<HashMap<i64, i64>>()),
        },
        VmTestCase {
            input: "{1: 2, 2: 3}",
            expected: Box::new([(1, 2), (2, 3)].into_iter().collect::<HashMap<i64, i64>>()),
        },
        VmTestCase {
            input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
            expected: Box::new([(2, 4), (6, 16)].into_iter().collect::<HashMap<i64, i64>>()),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn index_expressions() {
    let tests = &[
        VmTestCase {
            input: "[1, 2, 3][1]",
            expected: Box::new(2),
        },
        VmTestCase {
            input: "[1, 2, 3][0 + 2]",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "[[1, 1, 1]][0][0]",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "[][0]",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "[1, 2, 3][99]",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "[1][-1]",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "{1: 1, 2: 2}[1]",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "{1: 1, 2: 2}[2]",
            expected: Box::new(2),
        },
        VmTestCase {
            input: "{1: 1}[0]",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "{}[0]",
            expected: Box::new(()),
        },
    ];

    run_vm_tests!(tests);
}

fn parse(input: &str) -> Program {
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    p.parse_program()
}

fn test_expected_object(expected: &Box<dyn Any>, actual: &Object) {
    if let Some(expected) = expected.downcast_ref::<i32>().copied() {
        test_integer_object(expected as i64, actual)
    } else if let Some(expected) = expected.downcast_ref::<bool>().copied() {
        test_boolean_object(expected, actual);
    } else if let Some(()) = expected.downcast_ref::<()>() {
        assert!(matches!(actual, Object::Null));
    } else if let Some(expected) = expected.downcast_ref::<&str>() {
        test_string_object(expected, actual);
    } else if let Some(expected) = expected.downcast_ref::<&[i32]>() {
        let Object::Array(array) = actual else {
            panic!("object not Array: {}", actual.ty());
        };

        assert_eq!(expected.len(), array.len());

        for (i, epected_elem) in expected.iter().copied().enumerate() {
            test_integer_object(epected_elem as i64, &array[i]);
        }
    } else if let Some(expected) = expected.downcast_ref::<HashMap<i64, i64>>() {
        let Object::Hash(pairs) = actual else {
            panic!("object is not Hash. got={}", actual.ty());
        };

        assert_eq!(expected.len(), pairs.len());

        for (&expected_key, &expected_value) in expected {
            let Some(actual_value) = pairs.get(&expected_key.into()) else {
                panic!("no pair for given key in Pairs");
            };

            test_integer_object(expected_value, actual_value);
        }
    } else {
        panic!("type_id {:?}", expected.as_ref().type_id());
    }
}

fn test_integer_object(expected: i64, actual: &Object) {
    let &Object::Integer(actual) = actual else {
        panic!("object is not Integer. got={}", actual.ty());
    };
    assert_eq!(expected, actual);
}

fn test_boolean_object(expected: bool, actual: &Object) {
    let &Object::Boolean(actual) = actual else {
        panic!("object is not Boolean. got={}", actual.ty());
    };
    assert_eq!(expected, actual);
}

fn test_string_object(expected: &str, actual: &Object) {
    let Object::String(actual) = actual else {
        panic!("object is not String. got={}", actual.ty());
    };
    assert_eq!(expected, actual);
}
