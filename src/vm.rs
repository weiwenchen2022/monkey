use std::borrow::Cow;
use std::cmp::Ordering;

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
            }

            ip += 1;
        }
        Ok(())
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
