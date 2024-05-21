use crate::code::{self, Opcode};

use super::Instructions;

use crate::make;

#[test]
fn test_make() {
    struct Test {
        op: Opcode,
        operands: Vec<i64>,
        expected: Vec<u8>,
    }

    let tests = &[
        Test {
            op: Opcode::Constant,
            operands: vec![65534],
            expected: vec![Opcode::Constant as u8, 255, 254],
        },
        Test {
            op: Opcode::Add,
            operands: vec![],
            expected: vec![Opcode::Add as u8],
        },
        Test {
            op: Opcode::GetLocal,
            operands: vec![255],
            expected: vec![Opcode::GetLocal as u8, 255],
        },
        Test {
            op: Opcode::Closure,
            operands: vec![65534, 255],
            expected: vec![Opcode::Closure as u8, 255, 254, 255],
        },
    ];

    for tt in tests {
        let instruction = code::make(tt.op, &tt.operands);

        assert_eq!(tt.expected.len(), instruction.len());
        assert_eq!(&tt.expected, &*instruction);
    }
}

#[test]
fn instructions_display() {
    let instructions = &[
        make!(Opcode::Add,),
        make!(Opcode::GetLocal, 1),
        make!(Opcode::Constant, 2,),
        make!(Opcode::Constant, 65535,),
        make!(Opcode::Closure, 65535, 255),
    ];

    let expected = "0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpClosure 65535 255
";

    let concatted: Instructions = instructions
        .iter()
        .fold(vec![], |mut concatted, ins| {
            concatted.extend_from_slice(ins);
            concatted
        })
        .into();

    assert_eq!(expected, concatted.to_string());
}

#[test]
fn read_operands() {
    struct Test<'a> {
        op: Opcode,
        operands: &'a [i64],
        bytes_read: usize,
    }
    let tests: &[Test] = &[
        Test {
            op: Opcode::Constant,
            operands: &[65535],
            bytes_read: 2,
        },
        Test {
            op: Opcode::GetLocal,
            operands: &[255],
            bytes_read: 1,
        },
        Test {
            op: Opcode::Closure,
            operands: &[65535, 255],
            bytes_read: 3,
        },
    ];

    for tt in tests {
        let instruction = code::make(tt.op, tt.operands);

        let def = code::lookup(tt.op as u8).expect("definition not found:");
        let (operands_read, n) = code::read_operands(def, &instruction[1..]);
        assert_eq!(tt.bytes_read, n);

        assert_eq!(tt.operands, operands_read);
    }
}
