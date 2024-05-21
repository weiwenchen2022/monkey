use crate::{code::Instructions, object::Object};

#[derive(Clone)]
pub(crate) struct Frame {
    pub(crate) cl: Object,
    pub(crate) ip: isize,
    pub(crate) base_pointer: usize,
}

impl Default for Frame {
    fn default() -> Self {
        Self {
            cl: Object::Null,
            ip: -1,
            base_pointer: 0,
        }
    }
}

impl Frame {
    pub(crate) fn new(cl: Object, base_pointer: usize) -> Self {
        Self {
            cl,
            ip: -1,
            base_pointer,
        }
    }

    pub(crate) fn instructions(&self) -> &Instructions {
        if let Object::Closure { f, .. } = &self.cl {
            if let Object::CompiledFunction { instructions, .. } = f.as_ref() {
                instructions
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
    }
}
