use std::any::Any;

use crate::{
    ast::Program,
    code::{Instructions, Opcode},
    lexer::Lexer,
    object::Object,
    parser::Parser,
};

use crate::make;

use super::Compiler;

struct CompilerTestCase<'a> {
    input: &'a str,
    expected_constants: Vec<Box<dyn Any>>,
    expected_instructions: Vec<Instructions>,
}

#[test]
fn integer_arithmetic() {
    let tests: &[CompilerTestCase] = &[
        CompilerTestCase {
            input: "1 + 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Add),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "1; 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Pop),
                make!(Opcode::Constant, 1),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "1 - 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Sub),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "1 * 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Mul),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "2 / 1",
            expected_constants: vec![Box::new(2), Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Div),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "-1",
            expected_constants: vec![Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Minus),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn boolean_expression() {
    let tests = &[
        CompilerTestCase {
            input: "true",
            expected_constants: vec![],
            expected_instructions: vec![make!(Opcode::True), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "false",
            expected_constants: vec![],
            expected_instructions: vec![make!(Opcode::False), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "1 > 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::GreaterThan),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "1 < 2",
            expected_constants: vec![Box::new(2), Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::GreaterThan),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "1 == 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Equal),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "1 != 2",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::NotEqual),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "true == false",
            expected_constants: vec![],
            expected_instructions: vec![
                make!(Opcode::True),
                make!(Opcode::False),
                make!(Opcode::Equal),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "true != false",
            expected_constants: vec![],
            expected_instructions: vec![
                make!(Opcode::True),
                make!(Opcode::False),
                make!(Opcode::NotEqual),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "!true",
            expected_constants: vec![],
            expected_instructions: vec![
                make!(Opcode::True),
                make!(Opcode::Bang),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests(tests);
}

fn run_compiler_tests(tests: &[CompilerTestCase]) {
    for tt in tests {
        let program = parse(tt.input);

        let mut compiler = Compiler::new();
        compiler.compile(program).expect("compiler error: ");

        let bytecode = compiler.bytecode();

        test_instructions(&tt.expected_instructions, &bytecode.instructions);
        test_constants(&tt.expected_constants, &bytecode.constants);
    }
}

fn parse(input: &str) -> Program {
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    p.parse_program()
}

fn test_instructions(expected: &[Instructions], actual: &Instructions) {
    let concatted = concat_instructions(expected);
    assert_eq!(&concatted, actual);
}

fn concat_instructions(s: &[Instructions]) -> Instructions {
    s.iter()
        .fold(Vec::new(), |mut out, ins| {
            out.extend_from_slice(ins);
            out
        })
        .into()
}

fn test_constants(expected: &[Box<dyn Any>], actual: &[Object]) {
    assert_eq!(expected.len(), actual.len());

    for (i, constant) in expected.iter().enumerate() {
        if let Some(constant) = constant.downcast_ref::<i32>().copied() {
            test_integer_object(constant as i64, &actual[i])
        } else {
            panic!("type_id {:?}", constant.as_ref().type_id());
        }
    }
}

fn test_integer_object(expected: i64, actual: &Object) {
    let &Object::Integer(actual) = actual else {
        panic!("object is not Integer. got={}", actual.ty());
    };
    assert_eq!(expected, actual);
}
