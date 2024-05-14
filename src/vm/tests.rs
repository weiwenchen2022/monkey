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

        let stack_elem = vm.stack_top();
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
