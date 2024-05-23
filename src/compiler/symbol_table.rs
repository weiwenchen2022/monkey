use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum SymbolScope {
    Local,
    Global,
    Builtin,
    Free,
    Function,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Symbol {
    pub(crate) name: Rc<String>,
    pub(crate) scope: SymbolScope,
    pub(crate) index: usize,
}

pub struct SymbolTable {
    pub(crate) outer: Option<Rc<RefCell<SymbolTable>>>,

    store: HashMap<String, Symbol>,
    pub(crate) num_definitions: usize,

    pub(crate) free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub(crate) fn new(outer: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            outer,

            store: HashMap::new(),
            num_definitions: 0,

            free_symbols: Vec::new(),
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

    pub(crate) fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if let obj @ Some(_) = self.store.get(name).cloned() {
            obj
        } else if let Some(outer) = &self.outer {
            let obj = outer.borrow_mut().resolve(name)?;
            if matches!(obj.scope, SymbolScope::Global | SymbolScope::Builtin) {
                return Some(obj);
            }

            let free = self.define_free(obj);
            Some(free)
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

    pub(crate) fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());

        let symbol = Symbol {
            name: original.name.clone(),
            index: self.free_symbols.len() - 1,
            scope: SymbolScope::Free,
        };
        self.store
            .insert(original.name.as_ref().clone(), symbol.clone());
        symbol
    }

    pub(crate) fn define_function_name(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: Rc::new(name.clone()),
            scope: SymbolScope::Function,
            index: 0,
        };
        self.store.insert(name, symbol.clone());
        symbol
    }
}
