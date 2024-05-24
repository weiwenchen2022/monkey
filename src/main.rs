use monkey::repl;
use monkey::{self, Compiler, Lexer, VM};
use std::time::Instant;

use std::fs;
use std::io;
use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// scripts to executes
    scripts: Vec<PathBuf>,

    /// enter interactive mode after executing 'scripts'
    #[arg(short, long)]
    interactive: bool,
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

        let duration = start.elapsed();
        let result = machine.last_popped_stack_elem();

        println!(
            "script={}, result={}, duration={:?}",
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
