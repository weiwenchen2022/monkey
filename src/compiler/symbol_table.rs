use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum SymbolScope {
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Symbol {
    pub(crate) name: String,
    pub(crate) scope: SymbolScope,
    pub(crate) index: usize,
}

pub(crate) struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub(crate) fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: SymbolScope::Global,
            index: self.num_definitions,
        };
        self.num_definitions += 1;
        self.store.insert(name, symbol.clone());

        symbol
    }

    pub(crate) fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::{Symbol, SymbolScope, SymbolTable};
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
        ]
        .into_iter()
        .collect();

        let mut gobal = SymbolTable::new();

        let a = gobal.define("a".to_string());
        assert_eq!(expected["a"], a);

        let b = gobal.define("b".to_string());
        assert_eq!(expected["b"], b);
    }

    #[test]
    fn resolve_global() {
        let mut global = SymbolTable::new();
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
}
