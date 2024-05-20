use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::object::Object;

#[derive(Clone)]
pub struct Environment {
    outer: Option<Box<Environment>>,
    store: Rc<RefCell<HashMap<String, Object>>>,
}

impl Environment {
    pub fn new(outer: Option<Box<Environment>>) -> Self {
        Self {
            outer,
            store: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(val) = self.store.borrow().get(name).cloned() {
            return Some(val);
        }

        if let Some(outer) = &self.outer {
            outer.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: String, val: Object) {
        self.store.borrow_mut().insert(name, val);
    }
}
