use super::symbol_table::{Symbol, SymbolScope, SymbolTable};
use std::collections::HashMap;

#[test]
fn define() {
    let expected: HashMap<String, Symbol> = [
        (
            "a".to_string(),
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ),
        (
            "c".to_string(),
            Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Local,
                index: 0,
            },
        ),
        (
            "d".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ),
        (
            "e".to_string(),
            Symbol {
                name: "e".to_string(),
                scope: SymbolScope::Local,
                index: 0,
            },
        ),
        (
            "f".to_string(),
            Symbol {
                name: "f".to_string(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ),
    ]
    .into_iter()
    .collect();

    let mut gobal = SymbolTable::new(None);

    let a = gobal.define("a".to_string());
    assert_eq!(expected["a"], a);

    let b = gobal.define("b".to_string());
    assert_eq!(expected["b"], b);

    let mut first_local = SymbolTable::new(Some(Box::new(gobal)));
    let c = first_local.define("c".to_string());
    assert_eq!(expected["c"], c);
    let d = first_local.define("d".to_string());
    assert_eq!(expected["d"], d);

    let mut second_local = SymbolTable::new(Some(Box::new(first_local)));
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
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ),
    ]
    .into_iter()
    .collect();

    for sym in expected.values() {
        let result = global.resolve(&sym.name).unwrap();
        assert_eq!(sym, result);
    }
}

#[test]
fn resolve_local() {
    let mut global = SymbolTable::new(None);
    global.define("a".to_string());
    global.define("b".to_string());

    let mut local = SymbolTable::new(Some(Box::new(global)));
    local.define("c".to_string());
    local.define("d".to_string());

    let expected = &[
        Symbol {
            name: "a".to_string(),
            scope: SymbolScope::Global,
            index: 0,
        },
        Symbol {
            name: "b".to_string(),
            scope: SymbolScope::Global,
            index: 1,
        },
        Symbol {
            name: "c".to_string(),
            scope: SymbolScope::Local,
            index: 0,
        },
        Symbol {
            name: "d".to_string(),
            scope: SymbolScope::Local,
            index: 1,
        },
    ];

    for sym in expected {
        let result = local.resolve(&sym.name).unwrap();
        assert_eq!(sym, result);
    }
}

#[test]
fn resolve_nested_local() {
    let mut global = SymbolTable::new(None);
    global.define("a".to_string());
    global.define("b".to_string());

    let mut first_local = SymbolTable::new(Some(Box::new(global)));
    first_local.define("c".to_string());
    first_local.define("d".to_string());

    let mut second_local = SymbolTable::new(Some(Box::new(first_local)));
    second_local.define("e".to_string());
    second_local.define("f".to_string());

    struct Test<'a> {
        table: &'a SymbolTable,
        expected_symbols: &'a [Symbol],
    }

    let tests = &[
        Test {
            table: second_local.outer.as_ref().unwrap(),
            expected_symbols: &[
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "c".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "d".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        },
        Test {
            table: &second_local,
            expected_symbols: &[
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "e".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "f".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        },
    ];

    for tt in tests {
        for sym in tt.expected_symbols {
            let result = tt.table.resolve(&sym.name).unwrap();
            assert_eq!(sym, result);
        }
    }
}
