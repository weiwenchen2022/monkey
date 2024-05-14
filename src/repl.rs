use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;

const PROMT: &str = ">> ";

pub fn start<R: Read, W: Write>(input: R, mut output: W) -> Result<(), Box<dyn Error + 'static>> {
    let mut lines = BufReader::new(input).lines();

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

        let mut comp = Compiler::new();
        if let Err(err) = comp.compile(program) {
            writeln!(&mut output, "Woops! Compilation failed:\n {err}")?;
            continue;
        }

        let mut machine = VM::new(comp.bytecode());
        if let Err(err) = machine.run() {
            writeln!(&mut output, "Woops! Executing bytecode failed:\n {err}")?;
            continue;
        }

        let stack_top = machine.stack_top();
        write!(&mut output, "{}", stack_top)?;
        writeln!(&mut output)?;
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
