use crate::{
    ast::{Expression, Node, Statement},
    code::{self, Instructions, Opcode},
    object::Object,
};

pub struct Bytecode {
    pub(crate) instructions: Instructions,
    pub(crate) constants: Vec<Object>,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::default(),
            constants: Vec::new(),
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
        self.add_instruction(ins)
    }

    fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend(&*ins);
        pos_new_instruction
    }
}

#[cfg(test)]
mod tests;
