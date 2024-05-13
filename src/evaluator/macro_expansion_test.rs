use super::{define_macros, expand_macros, tests::Test};
use crate::{ast::Program, environment::Environment, lexer::Lexer, object::Object, parser::Parser};

#[test]
fn define_marco() {
    let input = "
    let number = 1;
    let function = fn(x, y) { x + y };
    let mymacro = macro(x, y) { x + y; };
";

    let mut env = Environment::new(None);
    let mut program = test_parse_program(input);

    define_macros(&mut program, &mut env);

    assert_eq!(2, program.statements.len());

    assert!(env.get("number").is_none());
    assert!(env.get("function").is_none());
    let obj = env.get("mymacro").expect("macro not in environment.");
    let Object::Macro {
        parameters, body, ..
    } = obj
    else {
        panic!("object is not Macro. got={}", obj.ty());
    };

    assert_eq!(2, parameters.len());
    assert_eq!("x", parameters[0].to_string());
    assert_eq!("y", parameters[1].to_string());

    let expected_body = "(x + y)";
    assert_eq!(expected_body, body.to_string());
}

#[test]
fn test_expand_macros() {
    let tests: &[Test<'_, &'static str>] = &[
        Test {
            input: "
            let infixExpression = macro() { quote(1 + 2); };

            infixExpression();
            ",
            expected: "(1 + 2)",
        },
        Test {
            input: "
            let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };

            reverse(2 + 2, 10 - 5);
            ",
            expected: "(10 - 5) - (2 + 2)",
        },
        Test {
            input: r#"
            let unless = macro(condition, consequence, alternative) {
                quote(if (!(unquote(condition))) {
                    unquote(consequence);
                } else {
                    unquote(alternative);
                });
            };

            unless(10 > 5, puts("not greater"), puts("greater"));
            "#,
            expected: r#"if (!(10 > 5)) { puts("not greater") } else { puts("greater") }"#,
        },
    ];

    for tt in tests {
        let expected = test_parse_program(tt.expected);
        let mut program = test_parse_program(tt.input);

        let mut env = Environment::new(None);

        define_macros(&mut program, &mut env);

        let expanded = expand_macros(program, &env);

        assert_eq!(expected.to_string(), expanded.to_string());
    }
}

fn test_parse_program(input: &str) -> Program {
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    p.parse_program()
}
