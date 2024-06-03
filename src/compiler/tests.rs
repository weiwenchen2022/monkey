use crate::ast::Program;
use crate::code::{Instructions, Opcode};
use crate::compiler::SymbolTable;
use crate::lexer::Lexer;
use crate::make;
use crate::object::Object;
use crate::parser::Parser;

use super::Compiler;

use std::any::Any;
use std::ops::Deref;
use std::vec;

struct CompilerTestCase<'a> {
    input: &'a str,
    expected_constants: Vec<Box<dyn Any>>,
    expected_instructions: Vec<Instructions>,
}

macro_rules! run_compiler_tests {
    ($tests:tt) => {
        for tt in $tests {
            let program = parse(tt.input);

            let mut compiler = Compiler::new();
            compiler.compile(program).expect("compiler error: ");

            let bytecode = compiler.bytecode();

            test_instructions(&tt.expected_instructions, &bytecode.instructions);
            test_constants(&tt.expected_constants, &bytecode.constants);
        }
    };
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

    run_compiler_tests!(tests);
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

    run_compiler_tests!(tests);
}

#[test]
fn conditionals() {
    let tests = &[
        CompilerTestCase {
            input: "
			if (true) { 10 }; 3333;
			",
            expected_constants: vec![Box::new(10), Box::new(3333)],
            expected_instructions: vec![
                // 0000
                make!(Opcode::True),
                // 0001
                make!(Opcode::JumpNotTruthy, 10),
                // 0004
                make!(Opcode::Constant, 0),
                // 0007
                make!(Opcode::Jump, 11),
                // 0010
                make!(Opcode::Null),
                // 0011
                make!(Opcode::Pop),
                // 0012
                make!(Opcode::Constant, 1),
                // 0015
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
			if (true) { 10 } else { 20 }; 3333;
			",
            expected_constants: vec![Box::new(10), Box::new(20), Box::new(3333)],
            expected_instructions: vec![
                // 0000
                make!(Opcode::True),
                // 0001
                make!(Opcode::JumpNotTruthy, 10),
                // 0004
                make!(Opcode::Constant, 0),
                // 0007
                make!(Opcode::Jump, 13),
                // 0010
                make!(Opcode::Constant, 1),
                // 0013
                make!(Opcode::Pop),
                // 0014
                make!(Opcode::Constant, 2),
                // 0017
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests)
}

#[test]
fn global_let_statements() {
    let tests = &[
        CompilerTestCase {
            input: "
			let one = 1;
			let two = 2;
			",
            expected_constants: vec![Box::new(1), Box::new(2)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::SetGlobal, 1),
            ],
        },
        CompilerTestCase {
            input: "
			let one = 1;
			one;
			",
            expected_constants: vec![Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
			let one = 1;
			let two = one;
			two;
			",
            expected_constants: vec![Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::SetGlobal, 1),
                make!(Opcode::GetGlobal, 1),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn string_expressions() {
    let tests = &[
        CompilerTestCase {
            input: r#""monkey""#,
            expected_constants: vec![Box::new("monkey")],
            expected_instructions: vec![make!(Opcode::Constant, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: r#""mon" + "key""#,
            expected_constants: vec![Box::new("mon"), Box::new("key")],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Add),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn array_literals() {
    let tests = &[
        CompilerTestCase {
            input: "[]",
            expected_constants: vec![],
            expected_instructions: vec![make!(Opcode::Array, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "[1, 2, 3]",
            expected_constants: vec![Box::new(1), Box::new(2), Box::new(3)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Constant, 2),
                make!(Opcode::Array, 3),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "[1 + 2, 3 - 4, 5 * 6]",
            expected_constants: vec![
                Box::new(1),
                Box::new(2),
                Box::new(3),
                Box::new(4),
                Box::new(5),
                Box::new(6),
            ],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Add),
                make!(Opcode::Constant, 2),
                make!(Opcode::Constant, 3),
                make!(Opcode::Sub),
                make!(Opcode::Constant, 4),
                make!(Opcode::Constant, 5),
                make!(Opcode::Mul),
                make!(Opcode::Array, 3),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn hash_literals() {
    let tests = &[
        CompilerTestCase {
            input: "{}",
            expected_constants: vec![],
            expected_instructions: vec![make!(Opcode::Hash, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "{1: 2, 3: 4, 5: 6}",
            expected_constants: vec![
                Box::new(1),
                Box::new(2),
                Box::new(3),
                Box::new(4),
                Box::new(5),
                Box::new(6),
            ],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Constant, 2),
                make!(Opcode::Constant, 3),
                make!(Opcode::Constant, 4),
                make!(Opcode::Constant, 5),
                make!(Opcode::Hash, 6),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "{1: 2 + 3, 4: 5 * 6}",
            expected_constants: vec![
                Box::new(1),
                Box::new(2),
                Box::new(3),
                Box::new(4),
                Box::new(5),
                Box::new(6),
            ],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Constant, 2),
                make!(Opcode::Add),
                make!(Opcode::Constant, 3),
                make!(Opcode::Constant, 4),
                make!(Opcode::Constant, 5),
                make!(Opcode::Mul),
                make!(Opcode::Hash, 4),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn index_expressions() {
    let tests = &[
        CompilerTestCase {
            input: "[1, 2, 3][1 + 1]",
            expected_constants: vec![
                Box::new(1),
                Box::new(2),
                Box::new(3),
                Box::new(1),
                Box::new(1),
            ],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Constant, 2),
                make!(Opcode::Array, 3),
                make!(Opcode::Constant, 3),
                make!(Opcode::Constant, 4),
                make!(Opcode::Add),
                make!(Opcode::Index),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "{1: 2}[2 - 1]",
            expected_constants: vec![Box::new(1), Box::new(2), Box::new(2), Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Hash, 2),
                make!(Opcode::Constant, 2),
                make!(Opcode::Constant, 3),
                make!(Opcode::Sub),
                make!(Opcode::Index),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn functions() {
    let tests = &[
        CompilerTestCase {
            input: "fn() { return 5 + 10 }",
            expected_constants: vec![
                Box::new(5),
                Box::new(10),
                Box::new(vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Add),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 2, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "fn() { 5 + 10 }",
            expected_constants: vec![
                Box::new(5),
                Box::new(10),
                Box::new(vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Add),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 2, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "fn() { 1; 2 }",
            expected_constants: vec![
                Box::new(1),
                Box::new(2),
                Box::new(vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Pop),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 2, 0), make!(Opcode::Pop)],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn compiler_scopes() {
    let mut comipler = Compiler::new();
    assert_eq!(0, comipler.scope_index);

    let global_symbol_table = comipler.symbol_table.as_ptr() as *const SymbolTable;

    comipler.emit(Opcode::Mul, &[]);

    comipler.enter_scope();

    assert_eq!(1, comipler.scope_index);

    comipler.emit(Opcode::Sub, &[]);

    assert_eq!(1, comipler.scopes[comipler.scope_index].instructions.len());

    let last = &comipler.scopes[comipler.scope_index].last_instruction;
    assert_eq!(Opcode::Sub, last.opcode);

    assert!(std::ptr::eq(
        global_symbol_table,
        comipler
            .symbol_table
            .borrow()
            .outer
            .as_ref()
            .unwrap()
            .as_ptr() as *const SymbolTable,
    ));

    comipler.leave_scope();

    assert_eq!(0, comipler.scope_index);

    assert!(std::ptr::eq(
        global_symbol_table,
        comipler.symbol_table.as_ptr() as *const SymbolTable
    ));
    assert!(comipler.symbol_table.borrow().outer.is_none());

    comipler.emit(Opcode::Add, &[]);

    assert_eq!(2, comipler.scopes[comipler.scope_index].instructions.len());

    let last = &comipler.scopes[comipler.scope_index].last_instruction;
    assert_eq!(Opcode::Add, last.opcode);

    let previous = &comipler.scopes[comipler.scope_index].previous_instruction;
    assert_eq!(Opcode::Mul, previous.opcode);
}

#[test]
fn functions_without_return_value() {
    let tests = &[CompilerTestCase {
        input: "fn() { }",
        expected_constants: vec![Box::new(vec![make!(Opcode::Return)])],
        expected_instructions: vec![make!(Opcode::Closure, 0, 0), make!(Opcode::Pop)],
    }];
    run_compiler_tests!(tests);
}

#[test]
fn function_calls() {
    let tests = &[
        CompilerTestCase {
            input: "fn() { 24 }();",
            expected_constants: vec![
                Box::new(24),
                Box::new(vec![make!(Opcode::Constant, 0), make!(Opcode::ReturnValue)]),
            ],
            expected_instructions: vec![
                make!(Opcode::Closure, 1, 0),
                make!(Opcode::Call, 0),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
			let noArg = fn() { 24 };
			noArg();
			",
            expected_constants: vec![
                Box::new(24),
                Box::new(vec![make!(Opcode::Constant, 0), make!(Opcode::ReturnValue)]),
            ],
            expected_instructions: vec![
                make!(Opcode::Closure, 1, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::Call, 0),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
			let oneArg = fn(a) { a };
			oneArg(24);
			",
            expected_constants: vec![
                Box::new(vec![make!(Opcode::GetLocal, 0), make!(Opcode::ReturnValue)]),
                Box::new(24),
            ],
            expected_instructions: vec![
                make!(Opcode::Closure, 0, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Call, 1),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
        	let manyArg = fn(a, b, c) { a; b; c };
        	manyArg(24, 25, 26);
        	",
            expected_constants: vec![
                Box::new(vec![
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Pop),
                    make!(Opcode::GetLocal, 1),
                    make!(Opcode::Pop),
                    make!(Opcode::GetLocal, 2),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(24),
                Box::new(25),
                Box::new(26),
            ],
            expected_instructions: vec![
                make!(Opcode::Closure, 0, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::Constant, 1),
                make!(Opcode::Constant, 2),
                make!(Opcode::Constant, 3),
                make!(Opcode::Call, 3),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn let_statement_scopes() {
    let tests = &[
        CompilerTestCase {
            input: "
			let num = 55;
			fn() { num }
			",
            expected_constants: vec![
                Box::new(55),
                Box::new(vec![
                    make!(Opcode::GetGlobal, 0),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::Closure, 1, 0),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
			fn() {
				let num = 55;
				num
			}
			",
            expected_constants: vec![
                Box::new(55),
                Box::new(vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::SetLocal, 0),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 1, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "
			fn() {
				let a = 55;
				let b = 77;
				a + b
			}
			",
            expected_constants: vec![
                Box::new(55),
                Box::new(77),
                Box::new(vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::SetLocal, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::SetLocal, 1),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::GetLocal, 1),
                    make!(Opcode::Add),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 2, 0), make!(Opcode::Pop)],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn builtins() {
    let tests = &[
        CompilerTestCase {
            input: "
			len([]);
			push([], 1);
			",
            expected_constants: vec![Box::new(1)],
            expected_instructions: vec![
                make!(Opcode::GetBuiltin, 0),
                make!(Opcode::Array, 0),
                make!(Opcode::Call, 1),
                make!(Opcode::Pop),
                make!(Opcode::GetBuiltin, 5),
                make!(Opcode::Array, 0),
                make!(Opcode::Constant, 0),
                make!(Opcode::Call, 2),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "fn() { len([]) }",
            expected_constants: vec![Box::new(vec![
                make!(Opcode::GetBuiltin, 0),
                make!(Opcode::Array, 0),
                make!(Opcode::Call, 1),
                make!(Opcode::ReturnValue),
            ])],
            expected_instructions: vec![make!(Opcode::Closure, 0, 0), make!(Opcode::Pop)],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn closures() {
    let tests = &[
        CompilerTestCase {
            input: "
			fn(a) {
				fn(b) {
					a + b
				}
			}
			",
            expected_constants: vec![
                Box::new(vec![
                    make!(Opcode::GetFree, 0),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Add),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(vec![
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Closure, 0, 1),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 1, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "
			fn(a) {
				fn(b) {
					fn(c) {
						a + b + c
					}
				}
			};
			",
            expected_constants: vec![
                Box::new(vec![
                    make!(Opcode::GetFree, 0),
                    make!(Opcode::GetFree, 1),
                    make!(Opcode::Add),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Add),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(vec![
                    make!(Opcode::GetFree, 0),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Closure, 0, 2),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(vec![
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Closure, 1, 1),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![make!(Opcode::Closure, 2, 0), make!(Opcode::Pop)],
        },
        CompilerTestCase {
            input: "
			let global = 55;

			fn() {
				let a = 66;

				fn() {
					let b = 77;

					fn() {
						let c = 88;

						global + a + b + c;
					}
				}
			}
			",
            expected_constants: vec![
                Box::new(55),
                Box::new(66),
                Box::new(77),
                Box::new(88),
                Box::new(vec![
                    make!(Opcode::Constant, 3),
                    make!(Opcode::SetLocal, 0),
                    make!(Opcode::GetGlobal, 0),
                    make!(Opcode::GetFree, 0),
                    make!(Opcode::Add),
                    make!(Opcode::GetFree, 1),
                    make!(Opcode::Add),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Add),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(vec![
                    make!(Opcode::Constant, 2),
                    make!(Opcode::SetLocal, 0),
                    make!(Opcode::GetFree, 0),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Closure, 4, 2),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(vec![
                    make!(Opcode::Constant, 1),
                    make!(Opcode::SetLocal, 0),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Closure, 5, 1),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![
                make!(Opcode::Constant, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::Closure, 6, 0),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
}

#[test]
fn recursive_functions() {
    let tests = &[
        CompilerTestCase {
            input: "
			let countDown = fn(x) { countDown(x - 1); };
			countDown(1);
			",
            expected_constants: vec![
                Box::new(1),
                Box::new(vec![
                    make!(Opcode::CurrentClosure),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Sub),
                    make!(Opcode::Call, 1),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(1),
            ],
            expected_instructions: vec![
                make!(Opcode::Closure, 1, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::Constant, 2),
                make!(Opcode::Call, 1),
                make!(Opcode::Pop),
            ],
        },
        CompilerTestCase {
            input: "
			let wrapper = fn() {
				let countDown = fn(x) { countDown(x - 1); };
				countDown(1);
			};
			wrapper();
			",
            expected_constants: vec![
                Box::new(1),
                Box::new(vec![
                    make!(Opcode::CurrentClosure),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Sub),
                    make!(Opcode::Call, 1),
                    make!(Opcode::ReturnValue),
                ]),
                Box::new(1),
                Box::new(vec![
                    make!(Opcode::Closure, 1, 0),
                    make!(Opcode::SetLocal, 0),
                    make!(Opcode::GetLocal, 0),
                    make!(Opcode::Constant, 2),
                    make!(Opcode::Call, 1),
                    make!(Opcode::ReturnValue),
                ]),
            ],
            expected_instructions: vec![
                make!(Opcode::Closure, 3, 0),
                make!(Opcode::SetGlobal, 0),
                make!(Opcode::GetGlobal, 0),
                make!(Opcode::Call, 0),
                make!(Opcode::Pop),
            ],
        },
    ];

    run_compiler_tests!(tests);
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
            test_integer_object(constant as i64, &actual[i]);
        } else if let Some(constant) = constant.downcast_ref::<&str>() {
            test_string_object(constant, &actual[i]);
        } else if let Some(constant) = constant.downcast_ref::<Vec<Instructions>>() {
            let Object::CompiledFunction(cf) = &actual[i] else {
                panic!("constant %d - not a function: {}", &actual[i].ty())
            };

            test_instructions(constant, &cf.instructions);
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

fn test_string_object(expected: &str, actual: &Object) {
    let Object::String(actual) = actual else {
        panic!("object is not String. got={}", actual.ty());
    };
    assert_eq!(expected, actual.deref());
}
