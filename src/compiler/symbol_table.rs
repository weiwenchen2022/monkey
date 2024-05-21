use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum SymbolScope {
    Local,
    Global,
    Builtin,
    // Free,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Symbol {
    pub(crate) name: Rc<String>,
    pub(crate) scope: SymbolScope,
    pub(crate) index: usize,
}

pub(crate) struct SymbolTable {
    pub(crate) outer: Option<Rc<RefCell<SymbolTable>>>,

    store: HashMap<String, Symbol>,
    pub(crate) num_definitions: usize,
}

impl SymbolTable {
    pub(crate) fn new(outer: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            outer,

            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub(crate) fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: Rc::new(name.clone()),
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

    pub(crate) fn resolve(&self, name: &str) -> Option<Symbol> {
        if let obj @ Some(_) = self.store.get(name).cloned() {
            obj
        } else if let Some(outer) = &self.outer {
            outer.borrow().resolve(name)
        } else {
            None
        }
    }

    pub(crate) fn define_builtin(&mut self, index: usize, name: String) -> Symbol {
        let symbol = Symbol {
            name: Rc::new(name.clone()),
            scope: SymbolScope::Builtin,
            index,
        };
        self.store.insert(name, symbol.clone());
        symbol
    }
}
