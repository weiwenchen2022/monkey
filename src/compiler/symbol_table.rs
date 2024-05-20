use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum SymbolScope {
    Local,
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Symbol {
    pub(crate) name: String,
    pub(crate) scope: SymbolScope,
    pub(crate) index: usize,
}

pub(crate) struct SymbolTable {
    pub(crate) outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
    pub(crate) num_definitions: usize,
}

impl SymbolTable {
    pub(crate) fn new(outer: Option<Box<Self>>) -> Self {
        Self {
            outer,

            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub(crate) fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: if self.outer.is_none() {
                SymbolScope::Global
            } else {
                SymbolScope::Local
            },
            index: self.num_definitions,
        };

        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub(crate) fn resolve(&self, name: &str) -> Option<&Symbol> {
        if let obj @ Some(_) = self.store.get(name) {
            obj
        } else if let Some(outer) = self.outer.as_ref() {
            outer.resolve(name)
        } else {
            None
        }
    }
}
