use crate::code::{self, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::Object;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;

const STACK_SIZE: usize = 2048;
pub(crate) const GLOBALS_SIZE: usize = 65536;

type Result<T> = std::result::Result<T, Cow<'static, str>>;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // Always points to the next value. Top of stack is stack[sp-1]

    pub(crate) globals: Vec<Object>,
}

impl VM {
    pub fn new_with_global_store(bytecode: Bytecode, s: Vec<Object>) -> Self {
        let mut vm = Self::new(bytecode);
        vm.globals = s;
        vm
    }

    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,

            globals: vec![Object::Null; GLOBALS_SIZE],
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

                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(op)?;
                }

                Opcode::Pop => {
                    self.pop();
                }

                Opcode::True => self.push(Object::Boolean(true))?,
                Opcode::False => self.push(Object::Boolean(false))?,

                Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => {
                    self.execute_comparison(op)?;
                }

                Opcode::Bang => {
                    self.execute_bang_operator()?;
                }

                Opcode::Minus => {
                    self.execute_minus_operator()?;
                }

                Opcode::Jump => {
                    let pos = code::read_u16(&instructions[ip + 1..]);
                    ip = pos as usize - 1;
                }
                Opcode::JumpNotTruthy => {
                    let pos = code::read_u16(&instructions[ip + 1..]);
                    ip += 2;

                    let condition = self.pop();
                    let is_truthy: bool = condition.into();
                    if !is_truthy {
                        ip = pos as usize - 1;
                    }
                }

                Opcode::Null => {
                    self.push(Object::Null)?;
                }

                Opcode::SetGlobal => {
                    let global_index = code::read_u16(&instructions[ip + 1..]);
                    ip += 2;
                    self.globals[global_index as usize] = self.pop();
                }

                Opcode::GetGlobal => {
                    let global_index = code::read_u16(&instructions[ip + 1..]);
                    ip += 2;

                    self.push(self.globals[global_index as usize].clone())?;
                }

                Opcode::Array => {
                    let num_elements = code::read_u16(&instructions[ip + 1..]);
                    ip += 2;

                    let array = self.build_array(self.sp - num_elements as usize, self.sp);
                    self.sp -= num_elements as usize;

                    self.push(array)?;
                }

                Opcode::Hash => {
                    let num_elements = code::read_u16(&instructions[ip + 1..]);
                    ip += 2;

                    let hash = self.build_hash(self.sp - num_elements as usize, self.sp)?;

                    self.sp -= num_elements as usize;

                    self.push(hash)?;
                }

                Opcode::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?;
                }
            }

            ip += 1;
        }
        Ok(())
    }

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<()> {
        match (&left, &index) {
            (Object::Array(_), Object::Integer(_)) => self.execute_array_index(left, index),
            (Object::Hash(_), _) => self.execute_hash_index(left, index),
            _ => Err(format!("index operator not supported: {}", left.ty()).into()),
        }
    }

    fn execute_array_index(&mut self, left: Object, index: Object) -> Result<()> {
        let Object::Array(elements) = left else {
            unreachable!()
        };
        let Object::Integer(i) = index else {
            unreachable!();
        };

        self.push(elements.get(i as usize).cloned().unwrap_or(Object::Null))
    }

    fn execute_hash_index(&mut self, left: Object, index: Object) -> Result<()> {
        let Object::Hash(pairs) = left else {
            unreachable!();
        };

        if !index.is_hashable() {
            return Err(format!("unusable as hash key: {}", index.ty()).into());
        }

        self.push(pairs.get(&index).cloned().unwrap_or(Object::Null))
    }

    fn build_hash(&mut self, start_index: usize, end_index: usize) -> Result<Object> {
        let mut hashed = HashMap::with_capacity((end_index - start_index) / 2);

        for i in (start_index..end_index).step_by(2) {
            let key = std::mem::replace(&mut self.stack[i], Object::Null);
            let value = std::mem::replace(&mut self.stack[i + 1], Object::Null);

            if !key.is_hashable() {
                return Err(format!("unusable as hash key: {}", key.ty()).into());
            }

            hashed.insert(key, value);
        }

        Ok(Object::Hash(hashed))
    }

    fn build_array(&mut self, start_index: usize, end_index: usize) -> Object {
        let mut elements = Vec::with_capacity(end_index - start_index);
        for i in start_index..end_index {
            elements.push(std::mem::replace(&mut self.stack[i], Object::Null));
        }
        Object::Array(elements)
    }

    fn execute_minus_operator(&mut self) -> Result<()> {
        let operand = self.pop();
        match operand {
            Object::Integer(_) => self.push((-operand).unwrap()),
            _ => Err(format!("unsupported type for negation: {}", operand.ty()).into()),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<()> {
        let operand = self.pop();
        self.push((!operand).unwrap())
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<()> {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Integer(_), Object::Integer(_)) => {
                self.execute_binary_integer_operation(op, left, right)
            }
            (Object::String(_), Object::String(_)) => {
                self.execute_binary_string_operation(op, left, right)
            }

            _ => Err(format!(
                "unsupported types for binary operation: {} {}",
                left.ty(),
                right.ty()
            )
            .into()),
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: Object,
        right: Object,
    ) -> Result<()> {
        let result = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => Err(format!("unknown integer operator: {op:?}")),
        };
        self.push(result?)
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: Object,
        right: Object,
    ) -> Result<()> {
        let result = match op {
            Opcode::Add => left + right,
            _ => Err(format!("unknown string operator: {op:?}")),
        };
        self.push(result?)
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<()> {
        let right = self.pop();
        let left = self.pop();

        match op {
            Opcode::Equal => self.push((left == right).into()),
            Opcode::NotEqual => self.push((left != right).into()),
            Opcode::GreaterThan => {
                if let Some(ord) = left.partial_cmp(&right) {
                    self.push(matches!(ord, Ordering::Greater).into())
                } else {
                    Err(format!(
                        "unsupported types for binary operation: {} {}",
                        left.ty(),
                        right.ty(),
                    )
                    .into())
                }
            }
            _ => Err(format!("unknown operator: {:?}", op).into()),
        }
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
        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        o
    }

    pub(crate) fn last_popped_stack_elem(&self) -> Object {
        self.stack[self.sp].clone()
    }
}

#[cfg(test)]
mod tests;
