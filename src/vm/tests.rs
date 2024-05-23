use crate::ast::Program;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

use super::VM;

use std::any::Any;
use std::collections::HashMap;
use std::ops::Deref;

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

#[test]
fn calling_functions_without_argument() {
    let tests = &[
        VmTestCase {
            input: "
		let fivePlusTen = fn() { 5 + 10; };
		fivePlusTen();
		",
            expected: Box::new(15),
        },
        VmTestCase {
            input: "
		let one = fn() { 1; };
		let two = fn() { 2; };
		one() + two()
		",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "
		let a = fn() { 1 };
		let b = fn() { a() + 1 };
		let c = fn() { b() + 1 };
		c();
		",
            expected: Box::new(3),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn functions_with_return_statement() {
    let tests = &[
        VmTestCase {
            input: "
		let earlyExit = fn() { return 99; 100; };
		earlyExit();
		",
            expected: Box::new(99),
        },
        VmTestCase {
            input: "
		let earlyExit = fn() { return 99; return 100; };
		earlyExit();
		",
            expected: Box::new(99),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn functions_without_return_value() {
    let tests = &[
        VmTestCase {
            input: "
		let noReturn = fn() { };
		noReturn();
		",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "
		let noReturn = fn() { };
		let noReturnTwo = fn() { noReturn(); };
		noReturn();
		noReturnTwo();
		",
            expected: Box::new(()),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn first_calss_functions() {
    let tests = &[
        VmTestCase {
            input: "
		let returnsOne = fn() { 1; };
		let returnsOneReturner = fn() { returnsOne; };
		returnsOneReturner()();
		",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "
		let returnsOneReturner = fn() {
			let returnsOne = fn() { 1; };
			returnsOne;
		};
		returnsOneReturner()();
		",
            expected: Box::new(1),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn calling_functions_with_bindings() {
    let tests = &[
        VmTestCase {
            input: "
		let one = fn() { let one = 1; one };
		one();
		",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "
		let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
		oneAndTwo();
		",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "
		let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
		let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
		oneAndTwo() + threeAndFour();
		",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "
		let firstFoobar = fn() { let foobar = 50; foobar; };
		let secondFoobar = fn() { let foobar = 100; foobar; };
		firstFoobar() + secondFoobar();
		",
            expected: Box::new(150),
        },
        VmTestCase {
            input: "
		let globalSeed = 50;
		let minusOne = fn() {
			let num = 1;
			globalSeed - num;
		}
		let minusTwo = fn() {
			let num = 2;
			globalSeed - num;
		}
		minusOne() + minusTwo();
		",
            expected: Box::new(97),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn calling_functions_with_arguments_and_bindings() {
    let tests = &[
        VmTestCase {
            input: "
		let identity = fn(a) { a; };
		identity(4);
		",
            expected: Box::new(4),
        },
        VmTestCase {
            input: "
        let sum = fn(a, b) { a + b; };
        sum(1, 2);
        ",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "
        let sum = fn(a, b) {
        	let c = a + b;
        	c;
        };
        sum(1, 2);
        ",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "
        let sum = fn(a, b) {
        	let c = a + b;
        	c;
        };
        sum(1, 2) + sum(3, 4);
        ",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "
        let sum = fn(a, b) {
        	let c = a + b;
        	c;
        };
        let outer = fn() {
        	sum(1, 2) + sum(3, 4);
        };
        outer();
        ",
            expected: Box::new(10),
        },
        VmTestCase {
            input: "
        let globalNum = 10;

        let sum = fn(a, b) {
        	let c = a + b;
        	c + globalNum;
        };

        let outer = fn() {
        	sum(1, 2) + sum(3, 4) + globalNum;
        };

        outer() + globalNum;
        ",
            expected: Box::new(50),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn calling_functions_with_wrong_arguments() {
    let tests = &[
        VmTestCase {
            input: "fn() { 1; }(1);",
            expected: Box::new("wrong number of arguments: want=0, got=1"),
        },
        VmTestCase {
            input: "fn(a) { a; }();",
            expected: Box::new("wrong number of arguments: want=1, got=0"),
        },
        VmTestCase {
            input: "fn(a, b) { a + b; }(1);",
            expected: Box::new("wrong number of arguments: want=2, got=1"),
        },
    ];

    for tt in tests {
        let program = parse(tt.input);

        let mut comp = Compiler::new();
        comp.compile(program).expect("compiler error: ");

        let mut vm = VM::new(comp.bytecode());
        let err = vm
            .run()
            .expect_err("expected VM error but resulted in none.");
        assert_eq!(
            tt.expected.downcast_ref::<&'static str>().copied().unwrap(),
            err
        );
    }
}

#[test]
fn builtin_functions() {
    let tests = &[
        VmTestCase {
            input: r#"len("")"#,
            expected: Box::new(0),
        },
        VmTestCase {
            input: r#"len("four")"#,
            expected: Box::new(4),
        },
        VmTestCase {
            input: r#"len("hello world")"#,
            expected: Box::new(11),
        },
        VmTestCase {
            input: "len(1)",
            expected: Box::new(Object::Error(
                "argument to `len` not supported, got integer".to_string(),
            )),
        },
        VmTestCase {
            input: r#"len("one", "two")"#,
            expected: Box::new(Object::Error(
                "wrong number of arguments. got=2, want=1".to_string(),
            )),
        },
        VmTestCase {
            input: "len([1, 2, 3])",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "len([])",
            expected: Box::new(0),
        },
        VmTestCase {
            input: r#"puts("hello", "world!")"#,
            expected: Box::new(()),
        },
        VmTestCase {
            input: "first([1, 2, 3])",
            expected: Box::new(1),
        },
        VmTestCase {
            input: "first([])",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "first(1)",
            expected: Box::new(Object::Error(
                "argument to `first` must be array, got integer".to_string(),
            )),
        },
        VmTestCase {
            input: "last([1, 2, 3])",
            expected: Box::new(3),
        },
        VmTestCase {
            input: "last([])",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "last(1)",
            expected: Box::new(Object::Error(
                "argument to `last` must be array, got integer".to_string(),
            )),
        },
        VmTestCase {
            input: "rest([1, 2, 3])",
            expected: Box::new(&[2, 3] as &[i32]),
        },
        VmTestCase {
            input: "rest([])",
            expected: Box::new(()),
        },
        VmTestCase {
            input: "push([], 1)",
            expected: Box::new(&[1] as &[i32]),
        },
        VmTestCase {
            input: "push(1, 1)",
            expected: Box::new(Object::Error(
                "argument to `push` must be array, got integer".to_string(),
            )),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn closures() {
    let tests = &[
        VmTestCase {
            input: "
		let newClosure = fn(a) {
			fn() { a; };
		};
		let closure = newClosure(99);
		closure();
		",
            expected: Box::new(99),
        },
        VmTestCase {
            input: "
		let newAdder = fn(a, b) {
			fn(c) { a + b + c };
		};
		let adder = newAdder(1, 2);
		adder(8);
		",
            expected: Box::new(11),
        },
        VmTestCase {
            input: "
		let newAdder = fn(a, b) {
			let c = a + b;
			fn(d) { c + d };
		};
		let adder = newAdder(1, 2);
		adder(8);
		",
            expected: Box::new(11),
        },
        VmTestCase {
            input: "
		let newAdderOuter = fn(a, b) {
			let c = a + b;
			fn(d) {
				let e = d + c;
				fn(f) { e + f; };
			};
		};
		let newAdderInner = newAdderOuter(1, 2)
		let adder = newAdderInner(3);
		adder(8);
		",
            expected: Box::new(14),
        },
        VmTestCase {
            input: "
		let a = 1;
		let newAdderOuter = fn(b) {
			fn(c) {
				fn(d) { a + b + c + d };
			};
		};
		let newAdderInner = newAdderOuter(2)
		let adder = newAdderInner(3);
		adder(8);
		",
            expected: Box::new(14),
        },
        VmTestCase {
            input: "
		let newClosure = fn(a, b) {
			let one = fn() { a; };
			let two = fn() { b; };
			fn() { one() + two(); };
		};
		let closure = newClosure(9, 90);
		closure();
		",
            expected: Box::new(99),
        },
    ];

    run_vm_tests!(tests);
}

#[test]
fn recursive_functions() {
    let tests = &[
        VmTestCase {
            input: "
		let countDown = fn(x) {
			if (x == 0) {
				return 0;
			} else {
				countDown(x - 1);
			}
		};
		countDown(1);
		",
            expected: Box::new(0),
        },
        VmTestCase {
            input: "
		let countDown = fn(x) {
			if (x == 0) {
				return 0;
			} else {
				countDown(x - 1);
			}
		};
		let wrapper = fn() {
			countDown(1);
		};
		wrapper();
		",
            expected: Box::new(0),
        },
        VmTestCase {
            input: "
		let wrapper = fn() {
			let countDown = fn(x) {
				if (x == 0) {
					return 0;
				} else {
					countDown(x - 1);
				}
			};
			countDown(1);
		};
		wrapper();
		",
            expected: Box::new(0),
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
    } else if let Some(Object::Error(expected)) = expected.downcast_ref::<Object>() {
        let Object::Error(actual) = actual else {
            panic!("object is not Error: {}", actual.ty());
        };

        assert_eq!(expected, actual);
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
    assert_eq!(expected, actual.deref());
}
