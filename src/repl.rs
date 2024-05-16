use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};

use crate::compiler::{Compiler, SymbolTable};
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use crate::vm::{self, VM};

const PROMT: &str = ">> ";

pub fn start<R: Read, W: Write>(input: R, mut output: W) -> Result<(), Box<dyn Error + 'static>> {
    let mut lines = BufReader::new(input).lines();

    let mut constants = Vec::new();
    let mut globals = vec![Object::Null; vm::GLOBALS_SIZE];
    let mut symbol_table = SymbolTable::new();

    loop {
        write!(&mut output, "{}", PROMT)?;
        output.flush()?;

        let Some(line) = lines.next() else {
            break;
        };

        let line = line?;
        let l = Lexer::new(line.as_bytes());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors().is_empty() {
            print_parse_errors(&mut output, p.errors());
            continue;
        }

        let mut comp = Compiler::new_with_state(symbol_table, constants);
        if let Err(err) = comp.compile(program) {
            symbol_table = comp.symbol_table;
            constants = comp.constants;

            writeln!(&mut output, "Woops! Compilation failed:\n {err}")?;
            continue;
        }

        let code = comp.bytecode();
        symbol_table = comp.symbol_table;
        constants = code.constants.clone();

        let mut machine = VM::new_with_global_store(code, globals);
        if let Err(err) = machine.run() {
            globals = machine.globals;
            writeln!(&mut output, "Woops! Executing bytecode failed:\n {err}")?;
            continue;
        }

        let last_popped = machine.last_popped_stack_elem();
        write!(&mut output, "{}", last_popped)?;
        writeln!(&mut output)?;

        globals = machine.globals;
    }

    Ok(())
}

const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

fn print_parse_errors<W: Write>(output: &mut W, errors: &[String]) {
    write!(output, "{}", MONKEY_FACE).unwrap();
    writeln!(output, "Woops! We ran into some monkey business here!").unwrap();
    writeln!(output, " parser errors:").unwrap();
    errors
        .iter()
        .for_each(|msg| writeln!(output, "\t{msg}").unwrap());
}
