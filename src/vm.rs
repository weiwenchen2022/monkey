use std::borrow::Cow;
use std::mem;

use crate::code::{self, Instructions, Opcode};

use crate::compiler::Bytecode;
use crate::object::Object;

const STACK_SIZE: usize = 2048;

type Result<T> = std::result::Result<T, Cow<'static, str>>;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // Always points to the next value. Top of stack is stack[sp-1]
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Object {
        if 0 == self.sp {
            Object::Null
        } else {
            self.stack[self.sp - 1].clone()
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut ip = 0;
        let instructions = std::mem::take(&mut self.instructions);
        while ip < instructions.len() {
            let op = instructions[ip].try_into()?;

            match op {
                Opcode::Constant => {
                    let const_index = code::read_u16(&instructions[ip + 1..]);
                    ip += 2;

                    self.push(self.constants[const_index as usize].clone())?;
                }

                Opcode::Add => {
                    let right = self.pop();
                    let left = self.pop();
                    let Object::Integer(left_value) = left else {
                        panic!();
                    };
                    let Object::Integer(right_value) = right else {
                        panic!();
                    };

                    let result = left_value + right_value;
                    self.push(Object::Integer(result))?;
                }
            }
            ip += 1;
        }
        Ok(())
    }

    fn push(&mut self, o: Object) -> Result<()> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".into());
        }

        self.stack[self.sp] = o;
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Object {
        let o = mem::replace(&mut self.stack[self.sp - 1], Object::Null);
        self.sp -= 1;
        o
    }
}

#[cfg(test)]
mod tests;
