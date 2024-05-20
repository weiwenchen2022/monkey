use lazy_static::lazy_static;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::ops::{Deref, DerefMut};

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Instructions(Vec<u8>);

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl From<Vec<u8>> for Instructions {
    fn from(vec: Vec<u8>) -> Self {
        Self(vec)
    }
}

impl From<Instructions> for Vec<u8> {
    fn from(ins: Instructions) -> Self {
        ins.0
    }
}

impl Debug for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ins: &[u8] = &self.0;
        let mut i = 0;

        while i < ins.len() {
            let def = lookup(ins[i]).unwrap();

            let (operands, read) = read_operands(def, &ins[i + 1..]);
            writeln!(f, "{:04} {}", i, fmt_instruction(def, &operands))?;

            i += 1 + read;
        }

        Ok(())
    }
}

fn fmt_instruction<'a>(def: &'a Definition, operands: &[i64]) -> Cow<'a, str> {
    let operand_count = operands.len();
    if def.operand_widths.len() != operand_count {
        return format!(
            "ERROR: operand len {} does not match defined {}\n",
            operand_count,
            def.operand_widths.len(),
        )
        .into();
    }

    match operand_count {
        0 => def.name.into(),
        1 => format!("{} {}", def.name, operands[0]).into(),
        _ => format!("ERROR: unhandled operand_count for {}\n", def.name).into(),
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum Opcode {
    Constant,

    Add,

    Pop,

    Sub,
    Mul,
    Div,

    True,
    False,

    Equal,
    NotEqual,
    GreaterThan,

    Minus,
    Bang,

    JumpNotTruthy,
    Jump,

    Null,

    GetGlobal,
    SetGlobal,

    Array,
    Hash,
    Index,

    Call,

    ReturnValue,
    Return,

    GetLocal,
    SetLocal,
}

impl TryFrom<u8> for Opcode {
    type Error = String;
    fn try_from(op: u8) -> Result<Self, String> {
        for key in DEFINITIONS.keys() {
            if *key as u8 == op {
                return Ok(*key);
            }
        }
        Err(format!("opcode {op} undefined"))
    }
}

pub(crate) struct Definition {
    name: &'static str,
    operand_widths: &'static [u8],
}

lazy_static! {
    static ref DEFINITIONS: HashMap<Opcode, Definition> = {
        let mut definitions = HashMap::new();

        definitions.extend([
            (
                Opcode::Constant,
                Definition {
                    name: "OpConstant",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::Add,
                Definition {
                    name: "OpAdd",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Pop,
                Definition {
                    name: "OpPop",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Sub,
                Definition {
                    name: "OpSub",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Mul,
                Definition {
                    name: "OpMul",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Div,
                Definition {
                    name: "OpDiv",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::True,
                Definition {
                    name: "OpTrue",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::False,
                Definition {
                    name: "OpFalse",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Equal,
                Definition {
                    name: "OpEqual",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::NotEqual,
                Definition {
                    name: "OpNotEqual",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::GreaterThan,
                Definition {
                    name: "OpGreaterThan",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Minus,
                Definition {
                    name: "OpMinus",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Bang,
                Definition {
                    name: "OpBang",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::JumpNotTruthy,
                Definition {
                    name: "OpJumpNotTruthy",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::Jump,
                Definition {
                    name: "OpJump",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::Null,
                Definition {
                    name: "OpNull",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::GetGlobal,
                Definition {
                    name: "OpGetGlobal",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::SetGlobal,
                Definition {
                    name: "OpSetGlobal",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::Array,
                Definition {
                    name: "OpArray",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::Hash,
                Definition {
                    name: "OpHash",
                    operand_widths: &[2],
                },
            ),
            (
                Opcode::Index,
                Definition {
                    name: "OpIndex",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Call,
                Definition {
                    name: "OpCall",
                    operand_widths: &[1],
                },
            ),
            (
                Opcode::ReturnValue,
                Definition {
                    name: "OpReturnValue",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::Return,
                Definition {
                    name: "OpReturn",
                    operand_widths: &[],
                },
            ),
            (
                Opcode::GetLocal,
                Definition {
                    name: "OpGetLocal",
                    operand_widths: &[1],
                },
            ),
            (
                Opcode::SetLocal,
                Definition {
                    name: "OpSetLocal",
                    operand_widths: &[1],
                },
            ),
        ]);

        definitions
    };
}

pub fn lookup(op: u8) -> Result<&'static Definition, String> {
    DEFINITIONS
        .get(&op.try_into()?)
        .ok_or_else(|| format!("opcode {op} undefined"))
}

#[macro_export]
macro_rules! make {
    ($op:expr $(,)?) => {
        $crate::code::make($op, &[])
    };
    ($op:expr, $($operand:expr),+ $(,)?) => {
        $crate::code::make($op, &[$($operand),+])
    };
}

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

pub(crate) fn make(op: Opcode, operands: &[i64]) -> Instructions {
    let Some(def) = DEFINITIONS.get(&op) else {
        return Instructions(vec![]);
    };

    let instruction_len = 1 + def.operand_widths.iter().sum::<u8>();
    let mut instruction = Vec::with_capacity(instruction_len as usize);
    instruction.write_u8(op as u8).unwrap();

    for (i, o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => instruction.write_u16::<BigEndian>(*o as u16).unwrap(),
            1 => instruction.write_u8(*o as u8).unwrap(),
            _ => todo!("{width}"),
        }
    }

    Instructions(instruction)
}

pub(crate) fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<i64>, usize) {
    let mut operands = Vec::with_capacity(def.operand_widths.len());
    let mut offset = 0;

    for width in def.operand_widths {
        match width {
            2 => operands.push(read_u16(&ins[offset..]) as i64),
            1 => operands.push(read_u8(&ins[offset..]) as i64),
            _ => unreachable!("{width}"),
        }
        offset += *width as usize;
    }

    (operands, offset)
}

pub(crate) fn read_u16(mut ins: &[u8]) -> u16 {
    ins.read_u16::<BigEndian>().unwrap()
}

pub(crate) fn read_u8(ins: &[u8]) -> u8 {
    ins[0]
}

#[cfg(test)]
mod tests;
