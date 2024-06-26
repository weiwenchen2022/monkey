use std::rc::Rc;

use crate::{code::Instructions, object::Closure};

#[derive(Clone)]
pub(crate) struct Frame {
    pub(crate) cl: Rc<Closure>,
    pub(crate) ip: isize,
    pub(crate) base_pointer: usize,
}

impl Default for Frame {
    fn default() -> Self {
        Self {
            cl: Rc::new(Closure::default()),
            ip: -1,
            base_pointer: 0,
        }
    }
}

impl Frame {
    pub(crate) fn new(cl: Closure, base_pointer: usize) -> Self {
        Self {
            cl: Rc::new(cl),
            ip: -1,
            base_pointer,
        }
    }

    pub(crate) fn instructions(&self) -> &Instructions {
        &self.cl.f.instructions
    }
}
