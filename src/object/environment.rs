use crate::object::Object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Environment = Rc<RefCell<_Environment>>;

#[derive(Clone, Default)]
pub struct _Environment {
    outer: Option<Environment>,
    store: HashMap<String, Object>,
}

impl _Environment {
    pub fn new(outer: Option<Environment>) -> Self {
        Self {
            outer,
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(val) = self.store.get(name).cloned() {
            return Some(val);
        }

        if let Some(outer) = &self.outer {
            outer.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }
}
