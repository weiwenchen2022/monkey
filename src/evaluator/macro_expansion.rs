use crate::{
    ast::{self, Expression, Node, Program, Statement},
    environment::Environment,
    object::Object,
};

use super::Evaluator;

pub(crate) fn define_macros(program: &mut Program, env: &mut Environment) {
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
        let evaluated = body.eval(eval_env).unwrap();

        let Object::Quote(node) = evaluated else {
            panic!("we only support returning AST-nodes from macros");
        };

        node
    })
}

fn is_macro_call(function: &Expression, env: &Environment) -> (Object, bool) {
    let Expression::Identifier { value, .. } = function else {
        return (Object::Null, false);
    };

    let Some(obj) = env.get(value) else {
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
    let extended = Environment::new(Some(Box::new(env)));

    let mut args = args.drain(..);

    for param in parameters {
        let Expression::Identifier { value: name, .. } = param else {
            panic!();
        };
        extended.set(name, args.next().unwrap());
    }
    extended
}

fn is_macro_definition(node: &Statement) -> bool {
    let Statement::Let { value, .. } = node else {
        return false;
    };
    matches!(value, Expression::MacroLiteral { .. })
}

fn add_macro(stmt: Statement, env: &mut Environment) {
    let Statement::Let { name, value, .. } = stmt else {
        panic!();
    };
    let Expression::MacroLiteral {
        parameters, body, ..
    } = value
    else {
        panic!();
    };

    let Expression::Identifier { value: name, .. } = name else {
        panic!();
    };

    env.set(
        name,
        Object::Macro {
            parameters,
            body: *body,
            env: env.clone(),
        },
    );
}
