use lazy_static::lazy_static;

use std::collections::HashMap;

use crate::evaluator;
use crate::object::{self, BuiltinFunction, Object};

lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        let mut m = HashMap::new();

        m.insert("len", object::get_builtin_function_by_name("len").unwrap());
        m.insert(
            "puts",
            object::get_builtin_function_by_name("puts").unwrap(),
        );
        m.insert(
            "first",
            object::get_builtin_function_by_name("first").unwrap(),
        );
        m.insert(
            "last",
            object::get_builtin_function_by_name("last").unwrap(),
        );
        m.insert(
            "rest",
            object::get_builtin_function_by_name("rest").unwrap(),
        );
        m.insert(
            "push",
            object::get_builtin_function_by_name("push").unwrap(),
        );

        m
    };
}
