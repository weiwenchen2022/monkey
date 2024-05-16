use crate::{
    ast::{Expression, Node, Statement},
    code::{self, Instructions, Opcode},
    object::Object,
};

use std::mem;

pub(crate) use self::symbol_table::SymbolTable;

mod symbol_table;

pub struct Bytecode {
    pub(crate) instructions: Instructions,
    pub(crate) constants: Vec<Object>,
}

struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

impl Default for EmittedInstruction {
    fn default() -> Self {
        Self {
            opcode: Opcode::Null,
            position: 0,
        }
    }
}

pub struct Compiler {
    instructions: Instructions,
    pub(crate) constants: Vec<Object>,

    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,

    pub(crate) symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new_with_state(s: SymbolTable, constants: Vec<Object>) -> Self {
        let mut compiler = Self::new();
        compiler.symbol_table = s;
        compiler.constants = constants;
        compiler
    }

    pub fn new() -> Self {
        Self {
            instructions: Instructions::default(),
            constants: Vec::new(),

            last_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),

            symbol_table: SymbolTable::new(),
        }
    }

    pub fn compile<N: Into<Node>>(&mut self, node: N) -> Result<(), String> {
        let node = node.into();

        match node {
            Node::Program(program) => {
                for s in program.statements {
                    self.compile(s)?;
                }
            }
            Node::Statement(Statement::Expression { expression, .. }) => {
                self.compile(expression)?;
                self.emit(Opcode::Pop, &[]);
            }
            Node::Expression(Expression::Infix {
                mut left,
                mut operator,
                mut right,
                ..
            }) => {
                if operator == "<" {
                    (left, right) = (right, left);
                    operator = ">".to_string();
                }

                self.compile(*left)?;
                self.compile(*right)?;

                match operator.as_str() {
                    "+" => self.emit(Opcode::Add, &[]),
                    "-" => self.emit(Opcode::Sub, &[]),
                    "*" => self.emit(Opcode::Mul, &[]),
                    "/" => self.emit(Opcode::Div, &[]),
                    ">" => self.emit(Opcode::GreaterThan, &[]),
                    "==" => self.emit(Opcode::Equal, &[]),
                    "!=" => self.emit(Opcode::NotEqual, &[]),
                    _ => return Err(format!("unknown operator {operator}")),
                };
            }

            Node::Expression(Expression::IntegerLiteral { value, .. }) => {
                let integer = Object::Integer(value);
                let const_index = self.add_constant(integer);
                self.emit(Opcode::Constant, &[const_index as i64]);
            }

            Node::Expression(Expression::Boolean { value, .. }) => {
                self.emit(if value { Opcode::True } else { Opcode::False }, &[]);
            }

            Node::Expression(Expression::Prefix {
                operator, right, ..
            }) => {
                self.compile(*right)?;

                match operator.as_str() {
                    "!" => self.emit(Opcode::Bang, &[]),
                    "-" => self.emit(Opcode::Minus, &[]),
                    _ => return Err(format!("unknown operator {}", operator)),
                };
            }

            Node::Expression(Expression::If {
                condition,
                consequence,
                alternative,
                ..
            }) => {
                self.compile(*condition)?;

                // Emit an `OpJumpNotTruthy` with a bogus value
                let jump_not_truthy_pos = self.emit(Opcode::JumpNotTruthy, &[i16::MAX as i64]);

                self.compile(*consequence)?;

                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                // Emit an `OpJump` with a bogus value
                let jump_pos = self.emit(Opcode::Jump, &[i16::MAX as i64]);

                let after_consequence_pos = self.instructions.len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos as i64);

                if alternative.is_none() {
                    self.emit(Opcode::Null, &[]);
                } else {
                    self.compile(*alternative.unwrap())?;

                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                }

                let after_alternative_pos = self.instructions.len();
                self.change_operand(jump_pos, after_alternative_pos as i64);
            }

            Node::Statement(Statement::Block { statements, .. }) => {
                for s in statements {
                    self.compile(s)?;
                }
            }

            Node::Statement(Statement::Let { name, value, .. }) => {
                self.compile(value)?;

                let Expression::Identifier { value: name, .. } = name else {
                    panic!("{name:?}");
                };

                let symbol = self.symbol_table.define(name);
                self.emit(Opcode::SetGlobal, &[symbol.index as i64]);
            }

            Node::Expression(Expression::Identifier { value: name, .. }) => {
                let Some(symbol) = self.symbol_table.resolve(&name) else {
                    return Err(format!("undefined variable {name}"));
                };

                self.emit(Opcode::GetGlobal, &[symbol.index as i64]);
            }

            _ => (),
        }

        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: Opcode, operands: &[i64]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);
        self.set_last_instruction(op, pos);

        pos
    }

    fn last_instruction_is_pop(&self) -> bool {
        matches!(self.last_instruction, EmittedInstruction { opcode , .. } if  Opcode::Pop == opcode)
    }

    fn remove_last_pop(&mut self) {
        self.instructions.truncate(self.last_instruction.position);
        self.last_instruction = mem::take(&mut self.previous_instruction);
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Vec<u8>) {
        self.instructions[pos..pos + new_instruction.len()].copy_from_slice(&new_instruction);
    }

    fn change_operand(&mut self, op_pos: usize, operand: i64) {
        let op = self.instructions[op_pos].try_into().unwrap();
        let new_instruction = make!(op, operand);
        self.replace_instruction(op_pos, new_instruction.into());
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        self.previous_instruction = mem::replace(
            &mut self.last_instruction,
            EmittedInstruction {
                opcode: op,
                position: pos,
            },
        );
    }

    fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend(&*ins);
        pos_new_instruction
    }
}

#[cfg(test)]
mod tests;
