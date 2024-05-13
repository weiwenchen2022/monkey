use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};

use crate::ast::Node;
use crate::environment::Environment;
use crate::evaluator::{self, Evaluator};
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

const PROMT: &str = ">> ";

const PRELUDE: &str = "
let unless = macro(condition, consequence, alternative) {
    quote(if (!(unquote(condition))) {
        unquote(consequence);
    } else {
        unquote(alternative);
    });
};

let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };

    iter(arr, [])
};

let reduce = fn(arr, initial, f) {
    let iter = fn(arr, result) {
        if (len(arr) == 0) {
            result
        } else {
            iter(rest(arr), f(result, first(arr)));
        }
    };

    iter(arr, initial)
};

let sum = fn(arr) {
    reduce(arr, 0, fn(initial, elem) { initial + elem })
};
";

pub fn start<R: Read, W: Write>(input: R, mut output: W) -> Result<(), Box<dyn Error + 'static>> {
    let mut lines = BufReader::new(input).lines();
    let env = Environment::new(None);
    let mut macro_env = Environment::new(None);

    {
        let l = Lexer::new(PRELUDE.as_bytes());
        let mut p = Parser::new(l);
        let mut program = p.parse_program();
        if !p.errors().is_empty() {
            // print_parse_errors(&mut output, p.errors());
            return Err(p.errors().join("\n").into());
        }

        evaluator::define_macros(&mut program, &mut macro_env);

        let Node::Program(expanded) = evaluator::expand_macros(program, &macro_env) else {
            panic!("expected program node");
        };

        expanded.eval(env.clone())?;
    }

    loop {
        write!(&mut output, "{}", PROMT)?;
        output.flush()?;

        let Some(line) = lines.next() else {
            break;
        };

        let line = line?;
        let l = Lexer::new(line.as_bytes());
        let mut p = Parser::new(l);
        let mut program = p.parse_program();

        if !p.errors().is_empty() {
            print_parse_errors(&mut output, p.errors());
            continue;
        }

        evaluator::define_macros(&mut program, &mut macro_env);
        let Node::Program(expanded) = evaluator::expand_macros(program, &macro_env) else {
            panic!("expected program node");
        };

        let evaluated = expanded.eval(env.clone()).unwrap_or_else(Object::Error);
        if Object::Null != evaluated {
            write!(output, "{}", evaluated)?;
            writeln!(output)?;
        }
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
