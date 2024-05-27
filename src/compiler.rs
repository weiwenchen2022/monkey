use crate::ast::{BlockStatement, Expression, Identifier, Node, Statement};
use crate::code::{self, Instructions, Opcode};
use crate::object::{self, Object};

use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

mod symbol_table;
pub(crate) use self::symbol_table::SymbolTable;
use self::symbol_table::{Symbol, SymbolScope};

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

#[derive(Default)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

pub struct Compiler {
    pub(crate) constants: Vec<Object>,
    pub(crate) symbol_table: Rc<RefCell<SymbolTable>>,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new_with_state(s: Rc<RefCell<SymbolTable>>, constants: Vec<Object>) -> Self {
        let mut compiler = Self::new();
        compiler.symbol_table = s;
        compiler.constants = constants;
        compiler
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let main_scope = CompilationScope::default();
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new(None)));

        for (i, v) in object::BUILTINS.iter().enumerate() {
            symbol_table.borrow_mut().define_builtin(i, v.0.to_string());
        }

        Self {
            constants: Vec::new(),
            symbol_table,

            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn compile<N: Into<Node>>(&mut self, node: N) -> Result<(), String> {
        let node = node.into();

        match node {
            Node::Program(program) => {
                program
                    .statements
                    .into_iter()
                    .try_for_each(|s| self.compile(s))?;
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

                self.compile(Statement::Block(consequence))?;

                if self.last_instruction_is(Opcode::Pop) {
                    self.remove_last_pop();
                }

                // Emit an `OpJump` with a bogus value
                let jump_pos = self.emit(Opcode::Jump, &[i16::MAX as i64]);

                let after_consequence_pos = self.current_instructions().len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos as i64);

                if alternative.is_none() {
                    self.emit(Opcode::Null, &[]);
                } else {
                    self.compile(Statement::Block(alternative.unwrap()))?;

                    if self.last_instruction_is(Opcode::Pop) {
                        self.remove_last_pop();
                    }
                }

                let after_alternative_pos = self.current_instructions().len();
                self.change_operand(jump_pos, after_alternative_pos as i64);
            }

            Node::Statement(Statement::Block(BlockStatement { statements, .. })) => {
                for s in statements {
                    self.compile(s)?;
                }
            }

            Node::Statement(Statement::Let { name, value, .. }) => {
                let symbol = self.symbol_table.borrow_mut().define(name.value);

                self.compile(value)?;

                if SymbolScope::Global == symbol.scope {
                    self.emit(Opcode::SetGlobal, &[symbol.index as i64]);
                } else {
                    self.emit(Opcode::SetLocal, &[symbol.index as i64]);
                }
            }

            Node::Expression(Expression::Identifier(Identifier { value: name, .. })) => {
                let Some(symbol) = self.symbol_table.borrow_mut().resolve(&name) else {
                    return Err(format!("undefined variable {name}"));
                };

                self.load_symbol(&symbol);
            }

            Node::Expression(Expression::StringLiteral { value, .. }) => {
                let const_index = self.add_constant(value);
                self.emit(Opcode::Constant, &[const_index as i64]);
            }

            Node::Expression(Expression::ArrayLiteral { elements, .. }) => {
                let nelements = elements.len();
                elements.into_iter().try_for_each(|el| self.compile(el))?;

                self.emit(Opcode::Array, &[nelements as i64]);
            }

            Node::Expression(Expression::HashLiteral { pairs, .. }) => {
                let n = pairs.len();
                pairs.into_iter().try_for_each(|(k, v)| {
                    self.compile(k)?;
                    self.compile(v)
                })?;

                self.emit(Opcode::Hash, &[n as i64 * 2]);
            }

            Node::Expression(Expression::Index { left, index, .. }) => {
                self.compile(*left)?;
                self.compile(*index)?;
                self.emit(Opcode::Index, &[]);
            }

            Node::Expression(Expression::FunctionLiteral {
                parameters,
                body,
                name,
                ..
            }) => {
                self.enter_scope();

                if !name.is_empty() {
                    self.symbol_table.borrow_mut().define_function_name(name);
                }

                let num_parameters = parameters.len() as u8;
                if !parameters.is_empty() {
                    let mut symbol_table = self.symbol_table.borrow_mut();
                    parameters.into_iter().for_each(|p| {
                        symbol_table.define(p.value);
                    });
                }

                self.compile(Statement::Block(body))?;

                if self.last_instruction_is(Opcode::Pop) {
                    self.replace_last_pop_with_return();
                }

                if !self.last_instruction_is(Opcode::ReturnValue) {
                    self.emit(Opcode::Return, &[]);
                }

                let free_symbols = self.symbol_table.borrow().free_symbols.clone();
                let num_locals = self.symbol_table.borrow().num_definitions as u8;
                let instructions = self.leave_scope();

                for s in &free_symbols {
                    self.load_symbol(s);
                }

                let compiled_fn = Object::CompiledFunction {
                    instructions,
                    num_locals,
                    num_parameters,
                };
                let fn_index = self.add_constant(compiled_fn);
                self.emit(
                    Opcode::Closure,
                    &[fn_index as i64, free_symbols.len() as i64],
                );
            }

            Node::Statement(Statement::Return { return_value, .. }) => {
                self.compile(return_value)?;
                self.emit(Opcode::ReturnValue, &[]);
            }

            Node::Expression(Expression::Call {
                function,
                arguments,
                ..
            }) => {
                self.compile(*function)?;
                let nargs = arguments.len();
                arguments.into_iter().try_for_each(|a| self.compile(a))?;
                self.emit(Opcode::Call, &[nargs as i64]);
            }

            _ => (),
        }

        Ok(())
    }

    fn load_symbol(&mut self, s: &Symbol) {
        match s.scope {
            SymbolScope::Global => self.emit(Opcode::GetGlobal, &[s.index as i64]),
            SymbolScope::Local => self.emit(Opcode::GetLocal, &[s.index as i64]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, &[s.index as i64]),
            SymbolScope::Free => self.emit(Opcode::GetFree, &[s.index as i64]),
            SymbolScope::Function => self.emit(Opcode::CurrentClosure, &[]),
        };
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.scopes[self.scope_index].last_instruction.position;
        self.replace_instruction(last_pos, make!(Opcode::ReturnValue).into());
        self.scopes[self.scope_index].last_instruction.opcode = Opcode::ReturnValue;
    }

    pub fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.clone(),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::default());
        self.scope_index += 1;

        self.symbol_table = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(
            &self.symbol_table,
        )))));
    }

    fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop().unwrap();
        self.scope_index -= 1;
        let outer = self.symbol_table.borrow_mut().outer.take().unwrap();
        self.symbol_table = outer;
        scope.instructions
    }

    fn add_constant<O: Into<Object>>(&mut self, obj: O) -> usize {
        self.constants.push(obj.into());
        self.constants.len() - 1
    }

    fn emit(&mut self, op: Opcode, operands: &[i64]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);
        self.set_last_instruction(op, pos);

        pos
    }

    fn last_instruction_is(&mut self, op: Opcode) -> bool {
        if self.current_instructions().is_empty() {
            return false;
        }
        matches!(&self.scopes[self.scope_index].last_instruction, &EmittedInstruction { opcode , .. } if op == opcode)
    }

    fn remove_last_pop(&mut self) {
        let previous = mem::take(&mut self.scopes[self.scope_index].previous_instruction);
        let last = mem::replace(
            &mut self.scopes[self.scope_index].last_instruction,
            previous,
        );

        self.scopes[self.scope_index]
            .instructions
            .truncate(last.position);
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Vec<u8>) {
        self.current_instructions()[pos..pos + new_instruction.len()]
            .copy_from_slice(&new_instruction);
    }

    fn change_operand(&mut self, op_pos: usize, operand: i64) {
        let op = self.current_instructions()[op_pos].try_into().unwrap();
        let new_instruction = make!(op, operand);
        self.replace_instruction(op_pos, new_instruction.into());
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        self.scopes[self.scope_index].previous_instruction = mem::replace(
            &mut self.scopes[self.scope_index].last_instruction,
            EmittedInstruction {
                opcode: op,
                position: pos,
            },
        );
    }

    fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        self.current_instructions().extend(&*ins);
        pos_new_instruction
    }

    fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod symbol_table_test;
