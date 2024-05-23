use std::io::{self, Write};
use std::ops::Deref;

use crate::error;
use crate::evaluator;

use super::{BuiltinFunction, Object};

pub(crate) static BUILTINS: &[(&str, BuiltinFunction)] = &[
    ("len", |args| -> evaluator::Result<Object> {
        if args.len() != 1 {
            return error!("wrong number of arguments. got={}, want=1", args.len());
        }

        match &args[0] {
            Object::Array(elements) => Ok(Object::Integer(elements.len() as i64)),
            Object::String(s) => Ok(Object::Integer(s.len() as i64)),
            obj => error!("argument to `len` not supported, got {}", obj.ty()),
        }
    }),
    ("puts", |args| -> evaluator::Result<Object> {
        let mut stdout = io::stdout().lock();
        args.iter()
            .try_for_each(|arg| writeln!(&mut stdout, "{arg}",))
            .map_err(|err| format!("{err}"))?;
        Ok(Object::Null)
    }),
    ("first", |args| -> evaluator::Result<Object> {
        if args.len() != 1 {
            return error!("wrong number of arguments. got={}, want=1", args.len());
        }

        match &args[0] {
            Object::Array(elements) => Ok(elements.first().cloned().unwrap_or(Object::Null)),
            _ => error!("argument to `first` must be array, got {}", args[0].ty()),
        }
    }),
    ("last", |args| -> evaluator::Result<Object> {
        if args.len() != 1 {
            return error!("wrong number of arguments. got={}, want=1", args.len());
        }

        match &args[0] {
            Object::Array(elements) => Ok(elements.last().cloned().unwrap_or(Object::Null)),
            _ => error!("argument to `last` must be array, got {}", args[0].ty()),
        }
    }),
    ("rest", |args| -> evaluator::Result<Object> {
        if args.len() != 1 {
            return error!("wrong number of arguments. got={}, want=1", args.len());
        }

        match &args[0] {
            Object::Array(elements) => {
                if !elements.is_empty() {
                    let elements = elements[1..].to_vec();
                    Ok(elements.into())
                } else {
                    Ok(Object::Null)
                }
            }
            _ => error!("argument to `rest` must be array, got {}", args[0].ty()),
        }
    }),
    ("push", |mut args| -> evaluator::Result<Object> {
        if args.len() != 2 {
            return error!("wrong number of arguments. got={}, want=2", args.len());
        }

        let new_element = args.pop().unwrap();
        let arr = args.pop().unwrap();

        match arr {
            Object::Array(elements) => {
                let mut new_elements = Vec::with_capacity(elements.len() + 1);
                new_elements.clone_from_slice(elements.deref() as &[Object]);
                new_elements.push(new_element);
                Ok(new_elements.into())
            }
            _ => error!("argument to `push` must be array, got {}", arr.ty()),
        }
    }),
];

// pub(crate) fn get_builtin_by_name(name: &str) -> Option<Object> {
//     BUILTINS.iter().find_map(|def| {
//         if name == def.0 {
//             Some(Object::Builtin(def.1))
//         } else {
//             None
//         }
//     })
// }

pub(crate) fn get_builtin_function_by_name(name: &str) -> Option<BuiltinFunction> {
    BUILTINS
        .iter()
        .find_map(|def| if name == def.0 { Some(def.1) } else { None })
}
