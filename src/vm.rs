use crate::code::{self, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::{self, Object};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::mem;

use self::frame::Frame;

const STACK_SIZE: usize = 2048;
pub(crate) const GLOBALS_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

mod frame;

type Result<T> = std::result::Result<T, Cow<'static, str>>;

pub struct VM {
    constants: Vec<Object>,

    stack: Vec<Object>,
    sp: usize, // Always points to the next value. Top of stack is stack[sp-1]

    pub(crate) globals: Vec<Object>,

    frames: Vec<Frame>,
    frames_index: usize,
}

impl VM {
    pub fn new_with_global_store(bytecode: Bytecode, s: Vec<Object>) -> Self {
        let mut vm = Self::new(bytecode);
        vm.globals = s;
        vm
    }

    pub fn new(bytecode: Bytecode) -> Self {
        let main_fn = Object::CompiledFunction {
            instructions: bytecode.instructions,
            num_locals: 0,
            num_parameters: 0,
        };
        let main_closure = Object::Closure {
            f: Box::new(main_fn),
            free: vec![],
        };
        let main_frame = Frame::new(main_closure, 0);

        let mut frames = vec![Frame::default(); MAX_FRAMES];
        frames[0] = main_frame;

        Self {
            constants: bytecode.constants,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,

            globals: vec![Object::Null; GLOBALS_SIZE],

            frames,
            frames_index: 1,
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, f: Frame) {
        self.frames[self.frames_index] = f;
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        std::mem::take(&mut self.frames[self.frames_index])
    }

    #[allow(dead_code)]
    pub fn stack_top(&self) -> Object {
        if 0 == self.sp {
            Object::Null
        } else {
            self.stack[self.sp - 1].clone()
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut ip: usize;
        let mut ins: &Instructions;
        let mut op: Opcode;

        while self.current_frame().ip < self.current_frame().instructions().len() as isize - 1 {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip as usize;
            ins = self.current_frame().instructions();
            op = ins[ip].try_into()?;

            match op {
                Opcode::Constant => {
                    let const_index = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;

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
                    let pos = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip = pos as isize - 1;
                }
                Opcode::JumpNotTruthy => {
                    let pos = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;

                    let condition = self.pop();
                    let is_truthy: bool = condition.into();
                    if !is_truthy {
                        self.current_frame().ip = pos as isize - 1;
                    }
                }

                Opcode::Null => {
                    self.push(Object::Null)?;
                }

                Opcode::SetGlobal => {
                    let global_index = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;
                    self.globals[global_index as usize] = self.pop();
                }

                Opcode::GetGlobal => {
                    let global_index = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;

                    self.push(self.globals[global_index as usize].clone())?;
                }

                Opcode::Array => {
                    let num_elements = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;

                    let array = self.build_array(self.sp - num_elements as usize, self.sp);
                    self.sp -= num_elements as usize;

                    self.push(array)?;
                }

                Opcode::Hash => {
                    let num_elements = code::read_u16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;

                    let hash = self.build_hash(self.sp - num_elements as usize, self.sp)?;

                    self.sp -= num_elements as usize;

                    self.push(hash)?;
                }

                Opcode::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?;
                }

                Opcode::Call => {
                    let num_args = code::read_u8(&ins[ip + 1..]);
                    self.current_frame().ip += 1;

                    self.execute_call(num_args as usize)?;
                }
                Opcode::ReturnValue => {
                    let return_value = self.pop();

                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(return_value)?;
                }
                Opcode::Return => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(Object::Null)?;
                }

                Opcode::SetLocal => {
                    let local_index = code::read_u8(&ins[ip + 1..]);
                    self.current_frame().ip += 1;

                    let base_pointer = self.current_frame().base_pointer;
                    self.stack[base_pointer + local_index as usize] = self.pop();
                }
                Opcode::GetLocal => {
                    let local_index = code::read_u8(&ins[ip + 1..]);
                    self.current_frame().ip += 1;

                    let base_pointer = self.current_frame().base_pointer;
                    self.push(self.stack[base_pointer + local_index as usize].clone())?;
                }

                Opcode::GetBuiltin => {
                    let builtin_index = code::read_u8(&ins[ip + 1..]);
                    self.current_frame().ip += 1;

                    let definition = object::BUILTINS[builtin_index as usize];
                    self.push(Object::Builtin(definition.1))?;
                }

                Opcode::Closure => {
                    let const_index = code::read_u16(&ins[ip + 1..]);
                    let num_free = code::read_u8(&ins[ip + 3..]);
                    self.current_frame().ip += 3;

                    self.push_closure(const_index as usize, num_free as usize)?;
                }
                Opcode::GetFree => {
                    let free_index = code::read_u8(&ins[ip + 1..]);
                    self.current_frame().ip += 1;

                    let current_closure = &self.current_frame().cl;
                    let Object::Closure { free, .. } = current_closure else {
                        unreachable!();
                    };
                    let obj = free[free_index as usize].clone();
                    self.push(obj)?;
                }

                Opcode::CurrentClosure => {
                    let current_closure = self.current_frame().cl.clone();
                    self.push(current_closure)?;
                }
            }
        }
        Ok(())
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) -> Result<()> {
        let constant = &self.constants[const_index];
        if !matches!(constant, Object::CompiledFunction { .. }) {
            return Err(format!("not a function: {}", constant.ty()).into());
        }

        let free = self.stack[self.sp - num_free..self.sp].iter_mut().fold(
            Vec::with_capacity(num_free),
            |mut free, obj| {
                free.push(mem::replace(obj, Object::Null));
                free
            },
        );
        self.sp -= num_free;

        let closure = Object::Closure {
            f: Box::new(constant.clone()),
            free,
        };
        self.push(closure)
    }

    fn execute_call(&mut self, num_args: usize) -> Result<()> {
        let callee = self.stack[self.sp - 1 - num_args].clone();
        match callee {
            Object::Closure { .. } => self.call_closure(callee, num_args),
            Object::Builtin(_) => self.call_builtin(&callee, num_args),
            _ => Err("calling non-closure and non-builtin".into()),
        }
    }

    fn call_closure(&mut self, cl: Object, num_args: usize) -> Result<()> {
        let Object::Closure { f, .. } = &cl else {
            return Err("calling non-function".into());
        };

        let &Object::CompiledFunction {
            num_locals,
            num_parameters,
            ..
        } = f.as_ref()
        else {
            return Err("calling non-function".into());
        };

        if num_parameters as usize != num_args {
            return Err(format!(
                "wrong number of arguments: want={}, got={}",
                num_parameters, num_args
            )
            .into());
        }

        let frame = Frame::new(cl.clone(), self.sp - num_args);
        self.sp = frame.base_pointer + num_locals as usize;
        self.push_frame(frame);

        Ok(())
    }

    fn call_builtin(&mut self, builtin: &Object, num_args: usize) -> Result<()> {
        let args = self.stack[self.sp - num_args..self.sp].iter_mut().fold(
            Vec::with_capacity(num_args),
            |mut args, arg| {
                args.push(std::mem::replace(arg, Object::Null));
                args
            },
        );

        let Object::Builtin(f) = builtin else {
            unreachable!();
        };
        let result = f(args).unwrap_or_else(Object::Error);
        self.sp = self.sp - num_args - 1;
        self.push(result)
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

        Ok(hashed.into())
    }

    fn build_array(&mut self, start_index: usize, end_index: usize) -> Object {
        let mut elements = Vec::with_capacity(end_index - start_index);
        for i in start_index..end_index {
            elements.push(std::mem::replace(&mut self.stack[i], Object::Null));
        }
        Object::Array(elements.into())
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
