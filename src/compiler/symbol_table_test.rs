use super::symbol_table::{Symbol, SymbolScope, SymbolTable};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[test]
fn define() {
    let expected: HashMap<String, Symbol> = [
        (
            "a".to_string(),
            Symbol {
                name: Rc::new("a".to_string()),
                scope: SymbolScope::Global,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: Rc::new("b".to_string()),
                scope: SymbolScope::Global,
                index: 1,
            },
        ),
        (
            "c".to_string(),
            Symbol {
                name: Rc::new("c".to_string()),
                scope: SymbolScope::Local,
                index: 0,
            },
        ),
        (
            "d".to_string(),
            Symbol {
                name: Rc::new("d".to_string()),
                scope: SymbolScope::Local,
                index: 1,
            },
        ),
        (
            "e".to_string(),
            Symbol {
                name: Rc::new("e".to_string()),
                scope: SymbolScope::Local,
                index: 0,
            },
        ),
        (
            "f".to_string(),
            Symbol {
                name: Rc::new("f".to_string()),
                scope: SymbolScope::Local,
                index: 1,
            },
        ),
    ]
    .into_iter()
    .collect();

    let gobal = Rc::new(RefCell::new(SymbolTable::new(None)));

    let a = gobal.borrow_mut().define("a".to_string());
    assert_eq!(expected["a"], a);

    let b = gobal.borrow_mut().define("b".to_string());
    assert_eq!(expected["b"], b);

    let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(gobal))));
    let c = first_local.borrow_mut().define("c".to_string());
    assert_eq!(expected["c"], c);
    let d = first_local.borrow_mut().define("d".to_string());
    assert_eq!(expected["d"], d);

    let mut second_local = SymbolTable::new(Some(first_local));
    let e = second_local.define("e".to_string());
    assert_eq!(expected["e"], e);
    let f = second_local.define("f".to_string());
    assert_eq!(expected["f"], f);
}

#[test]
fn resolve_global() {
    let mut global = SymbolTable::new(None);
    global.define("a".to_string());
    global.define("b".to_string());

    let expected: HashMap<String, Symbol> = [
        (
            "a".to_string(),
            Symbol {
                name: Rc::new("a".to_string()),
                scope: SymbolScope::Global,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: Rc::new("b".to_string()),
                scope: SymbolScope::Global,
                index: 1,
            },
        ),
    ]
    .into_iter()
    .collect();

    for sym in expected.values() {
        let result = global.resolve(&sym.name).unwrap();
        assert_eq!(sym, &result);
    }
}

#[test]
fn resolve_local() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    global.borrow_mut().define("a".to_string());
    global.borrow_mut().define("b".to_string());

    let mut local = SymbolTable::new(Some(global));
    local.define("c".to_string());
    local.define("d".to_string());

    let expected = &[
        Symbol {
            name: Rc::new("a".to_string()),
            scope: SymbolScope::Global,
            index: 0,
        },
        Symbol {
            name: Rc::new("b".to_string()),
            scope: SymbolScope::Global,
            index: 1,
        },
        Symbol {
            name: Rc::new("c".to_string()),
            scope: SymbolScope::Local,
            index: 0,
        },
        Symbol {
            name: Rc::new("d".to_string()),
            scope: SymbolScope::Local,
            index: 1,
        },
    ];

    for sym in expected {
        let result = local.resolve(&sym.name).unwrap();
        assert_eq!(sym, &result);
    }
}

#[test]
fn resolve_nested_local() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    global.borrow_mut().define("a".to_string());
    global.borrow_mut().define("b".to_string());

    let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(global))));
    first_local.borrow_mut().define("c".to_string());
    first_local.borrow_mut().define("d".to_string());

    let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(
        &first_local,
    )))));
    second_local.borrow_mut().define("e".to_string());
    second_local.borrow_mut().define("f".to_string());

    struct Test<'a> {
        table: Rc<RefCell<SymbolTable>>,
        expected_symbols: &'a [Symbol],
    }

    let tests = &[
        Test {
            table: first_local,
            expected_symbols: &[
                Symbol {
                    name: Rc::new("a".to_string()),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("b".to_string()),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: Rc::new("c".to_string()),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("d".to_string()),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        },
        Test {
            table: second_local,
            expected_symbols: &[
                Symbol {
                    name: Rc::new("a".to_string()),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("b".to_string()),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: Rc::new("e".to_string()),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("f".to_string()),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        },
    ];

    for tt in tests {
        for sym in tt.expected_symbols {
            let result = tt.table.borrow_mut().resolve(&sym.name).unwrap();
            assert_eq!(sym, &result);
        }
    }
}

#[test]
fn define_resolve_builtins() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(&global)))));
    let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(
        &first_local,
    )))));

    let expected = &[
        Symbol {
            name: Rc::new("a".to_string()),
            scope: SymbolScope::Builtin,
            index: 0,
        },
        Symbol {
            name: Rc::new("d".to_string()),
            scope: SymbolScope::Builtin,
            index: 1,
        },
        Symbol {
            name: Rc::new("e".to_string()),
            scope: SymbolScope::Builtin,
            index: 2,
        },
        Symbol {
            name: Rc::new("f".to_string()),
            scope: SymbolScope::Builtin,
            index: 3,
        },
    ];

    for (i, sym) in expected.iter().enumerate() {
        global
            .borrow_mut()
            .define_builtin(i, sym.name.as_ref().clone());
    }

    for table in [global, first_local, second_local] {
        for sym in expected {
            let result = table.borrow_mut().resolve(&sym.name).unwrap();
            assert_eq!(sym, &result);
        }
    }
}

#[test]
fn resolve_free() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    global.borrow_mut().define("a".to_string());
    global.borrow_mut().define("b".to_string());

    let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(global))));
    first_local.borrow_mut().define("c".to_string());
    first_local.borrow_mut().define("d".to_string());

    let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(
        &first_local,
    )))));
    second_local.borrow_mut().define("e".to_string());
    second_local.borrow_mut().define("f".to_string());

    struct Test {
        table: Rc<RefCell<SymbolTable>>,
        expected_symbols: Vec<Symbol>,
        expected_free_symbols: Vec<Symbol>,
    }
    let tests = &[
        Test {
            table: first_local,
            expected_symbols: vec![
                Symbol {
                    name: Rc::new("a".to_string()),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("b".to_string()),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: Rc::new("c".to_string()),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("d".to_string()),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
            expected_free_symbols: vec![],
        },
        Test {
            table: second_local,
            expected_symbols: vec![
                Symbol {
                    name: Rc::new("a".to_string()),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("b".to_string()),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: Rc::new("c".to_string()),
                    scope: SymbolScope::Free,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("d".to_string()),
                    scope: SymbolScope::Free,
                    index: 1,
                },
                Symbol {
                    name: Rc::new("e".to_string()),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("f".to_string()),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
            expected_free_symbols: vec![
                Symbol {
                    name: Rc::new("c".to_string()),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: Rc::new("d".to_string()),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        },
    ];

    for tt in tests {
        for sym in &tt.expected_symbols {
            let result = tt.table.borrow_mut().resolve(&sym.name).unwrap();
            assert_eq!(sym, &result);
        }

        assert_eq!(
            tt.expected_free_symbols.len(),
            tt.table.borrow().free_symbols.len()
        );

        for (i, sym) in tt.expected_free_symbols.iter().enumerate() {
            let result = &tt.table.borrow().free_symbols[i];
            assert_eq!(sym, result);
        }
    }
}

#[test]
fn resolve_unresolvable_free() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    global.borrow_mut().define("a".to_string());

    let first_local = Rc::new(RefCell::new(SymbolTable::new(Some(global))));
    first_local.borrow_mut().define("c".to_string());

    let second_local = Rc::new(RefCell::new(SymbolTable::new(Some(first_local))));
    second_local.borrow_mut().define("e".to_string());
    second_local.borrow_mut().define("f".to_string());

    let expected = &[
        Symbol {
            name: Rc::new("a".to_string()),
            scope: SymbolScope::Global,
            index: 0,
        },
        Symbol {
            name: Rc::new("c".to_string()),
            scope: SymbolScope::Free,
            index: 0,
        },
        Symbol {
            name: Rc::new("e".to_string()),
            scope: SymbolScope::Local,
            index: 0,
        },
        Symbol {
            name: Rc::new("f".to_string()),
            scope: SymbolScope::Local,
            index: 1,
        },
    ];

    for sym in expected {
        let result = second_local
            .borrow_mut()
            .resolve(&sym.name)
            .expect(&format!("name {} not resolvable", sym.name));
        assert_eq!(sym, &result);
    }

    let expected_unresolvable = ["b", "d"];
    for name in expected_unresolvable {
        if second_local.borrow_mut().resolve(name).is_some() {
            panic!("name {name} resolved, but was expected not to");
        }
    }
}

#[test]
fn define_and_resolve_function_name() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    global.borrow_mut().define_function_name("a".to_string());

    let expected = Symbol {
        name: Rc::new("a".to_string()),
        scope: SymbolScope::Function,
        index: 0,
    };

    let result = global.borrow_mut().resolve(&expected.name).unwrap();
    assert_eq!(expected, result);
}

#[test]
fn shadowing_function_name() {
    let global = Rc::new(RefCell::new(SymbolTable::new(None)));
    global.borrow_mut().define_function_name("a".to_string());
    global.borrow_mut().define("a".to_string());

    let expected = Symbol {
        name: Rc::new("a".to_string()),
        scope: SymbolScope::Global,
        index: 0,
    };

    let result = global.borrow_mut().resolve(&expected.name).unwrap();
    assert_eq!(expected, result);
}
