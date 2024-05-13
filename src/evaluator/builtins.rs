use lazy_static::lazy_static;

use std::collections::HashMap;

use crate::{
    evaluator,
    object::{BuiltinFunction, Object},
};

lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<String, BuiltinFunction> = {
        let mut m = HashMap::new();

        m.insert("len".to_string(), len as BuiltinFunction);
        m.insert("first".to_string(), first as BuiltinFunction);
        m.insert("last".to_string(), last as BuiltinFunction);
        m.insert("rest".to_string(), rest as BuiltinFunction);
        m.insert("push".to_string(), push as BuiltinFunction);
        m.insert("puts".to_string(), puts as BuiltinFunction);

        m
    };
}

fn len(args: Vec<Object>) -> evaluator::Result<Object> {
    if args.len() != 1 {
        return error!("wrong number of arguments. got={}, want=1", args.len());
    }

    match &args[0] {
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        Object::Array(elements) => Ok(Object::Integer(elements.len() as i64)),
        _ => error!("argument to `len` not supported, got {}", args[0].ty()),
    }
}

fn first(args: Vec<Object>) -> evaluator::Result<Object> {
    if args.len() != 1 {
        return error!("wrong number of arguments. got={}, want=1", args.len());
    }

    match &args[0] {
        Object::Array(elements) => Ok(elements.first().cloned().unwrap_or(Object::Null)),
        _ => error!("argument to `first` must be array, got {}", args[0].ty()),
    }
}

fn last(args: Vec<Object>) -> evaluator::Result<Object> {
    if args.len() != 1 {
        return error!("wrong number of arguments. got={}, want=1", args.len());
    }

    match &args[0] {
        Object::Array(elements) => Ok(elements.last().cloned().unwrap_or(Object::Null)),
        _ => error!("argument to `last` must be array, got {}", args[0].ty()),
    }
}

fn rest(args: Vec<Object>) -> evaluator::Result<Object> {
    if args.len() != 1 {
        return error!("wrong number of arguments. got={}, want=1", args.len());
    }

    match &args[0] {
        Object::Array(elements) => {
            if !elements.is_empty() {
                Ok(Object::Array(elements[1..].to_vec()))
            } else {
                Ok(Object::Null)
            }
        }
        _ => error!("argument to `last` must be array, got {}", args[0].ty()),
    }
}

fn push(mut args: Vec<Object>) -> evaluator::Result<Object> {
    if args.len() != 2 {
        return error!("wrong number of arguments. got={}, want=2", args.len());
    }

    let elem = args.pop().unwrap();

    match &args[0] {
        Object::Array(elements) => {
            let mut new_elements = Vec::with_capacity(elements.len() + 1);
            new_elements.extend_from_slice(&elements[..]);
            new_elements.push(elem);
            Ok(Object::Array(new_elements))
        }
        _ => error!("argument to `push` must be array, got {}", args[0].ty()),
    }
}

fn puts(args: Vec<Object>) -> evaluator::Result<Object> {
    use std::io::Write;
    let mut stdout = std::io::stdout().lock();

    args.iter()
        .try_for_each(|arg| writeln!(&mut stdout, "{}", arg).map_err(|e| format!("{e}")))?;

    Ok(Object::Null)
}
