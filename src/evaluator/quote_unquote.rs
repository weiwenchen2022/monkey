use std::cell::RefCell;
use std::rc::Rc;

use super::eval;
use crate::ast::{self, Expression, Node, Tokenizer};
use crate::object::Environment;
use crate::object::Object;

pub(crate) fn quote(node: Node, env: &Environment) -> Object {
    let node = eval_unquote_calls(node, env);
    Object::Quote(node)
}

fn eval_unquote_calls(node: Node, env: &Environment) -> Node {
    ast::modify(node, move |mut node| {
        if !is_unquote_call(&node) {
            return node;
        }

        let Node::Expression(Expression::Call {
            ref mut arguments, ..
        }) = &mut node
        else {
            return node;
        };

        if arguments.len() != 1 {
            return node;
        }

        let arg = arguments.pop().unwrap();
        let unquoted = eval(&arg.into(), env).unwrap();
        unquoted.into()
    })
}

fn is_unquote_call(node: &Node) -> bool {
    matches!(node,  Node::Expression(Expression::Call { function, ..}) if "unquote" == function.token_literal())
}

#[cfg(test)]
mod tests {
    use crate::evaluator::tests::{test_eval, Test};
    use crate::object::Object;

    #[test]
    fn quote() {
        let tests: &[Test<'_, &'static str>] = &[
            Test {
                input: "quote(5)",
                expected: "5",
            },
            Test {
                input: "quote(5 + 8)",
                expected: "(5 + 8)",
            },
            Test {
                input: "quote(foobar)",
                expected: "foobar",
            },
            Test {
                input: "quote(foobar + barfoo)",
                expected: "(foobar + barfoo)",
            },
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input);
            let Object::Quote(quoted) = evaluated else {
                panic!("expected *object.Quote. got={}", evaluated.ty());
            };

            assert_eq!(tt.expected, quoted.to_string());
        }
    }

    #[test]
    fn quote_unquote() {
        let tests: &[Test<'_, &'static str>] = &[
            Test {
                input: "quote(unquote(4))",
                expected: "4",
            },
            Test {
                input: "quote(unquote(4 + 4))",
                expected: "8",
            },
            Test {
                input: "quote(8 + unquote(4 + 4))",
                expected: "(8 + 8)",
            },
            Test {
                input: "quote(unquote(4 + 4) + 8)",
                expected: "(8 + 8)",
            },
            Test {
                input: "let foobar = 8;
                quote(foobar)",
                expected: "foobar",
            },
            Test {
                input: "let foobar = 8;
                quote(unquote(foobar))",
                expected: "8",
            },
            Test {
                input: "quote(unquote(true))",
                expected: "true",
            },
            Test {
                input: "quote(unquote(true == false))",
                expected: "false",
            },
            Test {
                input: "quote(unquote(quote(4 + 4)))",
                expected: "(4 + 4)",
            },
            Test {
                input: "let quotedInfixExpression = quote(4 + 4);
                quote(unquote(4 + 4) + unquote(quotedInfixExpression))",
                expected: "(8 + (4 + 4))",
            },
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input);
            let Object::Quote(quoted) = evaluated else {
                panic!("expected *object.Quote. got={}", evaluated.ty());
            };

            assert_eq!(tt.expected, quoted.to_string());
        }
    }
}
