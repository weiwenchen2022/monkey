#![allow(dead_code)]

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{self, Expression, Identifier, Node, Program, Statement},
    object::{Environment, Object},
};

use super::eval;

pub(crate) fn define_macros(program: &mut Program, env: &Environment) {
    let mut definitions = vec![];

    for (i, statement) in program.statements.iter().enumerate() {
        if is_macro_definition(statement) {
            definitions.push(i);
        }
    }

    for i in definitions.iter().rev().copied() {
        add_macro(program.statements.drain(i..i + 1).next().unwrap(), env);
    }
}

pub(crate) fn expand_macros(program: Program, env: &Environment) -> Node {
    ast::modify(Node::Program(program), |mut node| {
        let Node::Expression(Expression::Call {
            function,
            arguments,
            ..
        }) = &mut node
        else {
            return node;
        };

        let (obj, ok) = is_macro_call(function, env);
        if !ok {
            return node;
        }

        let args = quote_args(std::mem::take(arguments));
        let eval_env = extend_macro_env(obj.clone(), args);

        let Object::Macro { body, .. } = obj else {
            panic!();
        };
        let evaluated = eval(&Statement::Block(body).into(), &eval_env).unwrap();

        let Object::Quote(node) = evaluated else {
            panic!("we only support returning AST-nodes from macros");
        };

        node
    })
}

fn is_macro_call(function: &Expression, env: &Environment) -> (Object, bool) {
    let Expression::Identifier(Identifier { value, .. }) = function else {
        return (Object::Null, false);
    };

    let Some(obj) = env.borrow().get(value) else {
        return (Object::Null, false);
    };

    if !matches!(&obj, Object::Macro { .. }) {
        return (Object::Null, false);
    }

    (obj, true)
}

fn quote_args(arguments: Vec<Expression>) -> Vec<Object> {
    arguments
        .into_iter()
        .map(|a| Object::Quote(Node::Expression(a)))
        .collect()
}

fn extend_macro_env(obj: Object, mut args: Vec<Object>) -> Environment {
    let Object::Macro {
        parameters, env, ..
    } = obj
    else {
        panic!();
    };

    use crate::object::environment::_Environment;
    let mut extended = _Environment::new(Some(env));

    let mut args = args.drain(..);

    for param in parameters {
        extended.set(param.value, args.next().unwrap());
    }
    Rc::new(RefCell::new(extended))
}

fn is_macro_definition(node: &Statement) -> bool {
    let Statement::Let { value, .. } = node else {
        return false;
    };
    matches!(value, Expression::MacroLiteral { .. })
}

fn add_macro(stmt: Statement, env: &Environment) {
    let Statement::Let { name, value, .. } = stmt else {
        panic!();
    };
    let Expression::MacroLiteral {
        parameters, body, ..
    } = value
    else {
        panic!();
    };

    env.borrow_mut().set(
        name.value,
        Object::Macro {
            parameters,
            body,
            env: Rc::clone(env),
        },
    );
}
