use monkey::repl;
use monkey::{self, Compiler, Environment, Lexer, VM};
use std::time::Instant;

use std::fs;
use std::io;
use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// scripts to executes
    scripts: Vec<PathBuf>,

    /// What engine to run the program in
    #[arg(short, long, default_value_t = Engine::VM)]
    engine: Engine,

    /// enter interactive mode after executing 'scripts'
    #[arg(short, long)]
    interactive: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Engine {
    VM,
    Eval,
}

use std::fmt::{self, Display};

impl Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Engine::VM => write!(f, "vm"),
            Engine::Eval => write!(f, "eval"),
        }
    }
}

fn main() {
    let cli = Cli::parse();

    for script in &cli.scripts {
        let content = match fs::read_to_string(script) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("read error: {err}");
                continue;
            }
        };

        let l = Lexer::new(content.as_bytes());
        let mut p = monkey::Parser::new(l);
        let program = p.parse_program();

        let result;
        let duration;

        match cli.engine {
            Engine::VM => {
                let mut comp = Compiler::new();
                if let Err(err) = comp.compile(program) {
                    eprintln!("compiler error: {err}");
                    continue;
                }

                let mut machine = VM::new(comp.bytecode());

                let start = Instant::now();
                if let Err(err) = machine.run() {
                    eprintln!("vm error: {err}");
                    continue;
                }

                duration = start.elapsed();
                result = machine.last_popped_stack_elem();
            }
            Engine::Eval => {
                let env = Environment::default();
                let start = Instant::now();
                (result, duration) = match monkey::eval(&program.into(), &env) {
                    Ok(result) => (result, start.elapsed()),
                    Err(err) => {
                        eprintln!("eval error: {err}");
                        continue;
                    }
                };
            }
        }

        println!(
            "engine={}, script={}, result={}, duration={:?}",
            cli.engine,
            script.display(),
            result,
            duration
        );
    }

    if cli.interactive {
        let user = std::env::var("USER").unwrap();
        println!("Hello {}! This is the Monkey programming language!", user);
        println!("Feel free to type in commands");

        repl::start(io::stdin().lock(), io::stdout().lock()).unwrap();
    }
}
