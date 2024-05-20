use crate::{code::Instructions, object::Object};

#[derive(Clone)]
pub(crate) struct Frame {
    pub(crate) f: Object,
    pub(crate) ip: isize,
    pub(crate) base_pointer: usize,
}

impl Default for Frame {
    fn default() -> Self {
        Self {
            f: Object::Null,
            ip: -1,
            base_pointer: 0,
        }
    }
}

impl Frame {
    pub(crate) fn new(f: Object, base_pointer: usize) -> Self {
        Self {
            f,
            ip: -1,
            base_pointer,
        }
    }

    pub(crate) fn instructions(&self) -> &Instructions {
        if let Object::CompiledFunction { instructions, .. } = &self.f {
            instructions
        } else {
            unreachable!();
        }
    }
}
