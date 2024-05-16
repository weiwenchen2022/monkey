use std::any::Any;

use crate::ast::Program;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

use super::VM;

struct VmTestCase<'a> {
    input: &'a str,
    expected: Box<dyn Any>,
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

    run_vm_tests(tests);
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
    ];

    run_vm_tests(tests);
}

fn run_vm_tests(tests: &[VmTestCase]) {
    for tt in tests {
        let program = parse(tt.input);

        let mut comp = Compiler::new();
        comp.compile(program).expect("compiler error: ");

        let mut vm = VM::new(comp.bytecode());
        vm.run().expect("vm error: ");

        let stack_elem = vm.last_popped_stack_elem();
        test_expected_object(&tt.expected, &stack_elem)
    }
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
