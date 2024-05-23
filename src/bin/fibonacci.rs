use std::time::{Duration, Instant};

use clap::{Parser, ValueEnum};
use monkey::{self, Compiler, Environment, Evaluator, Lexer, Object, VM};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// What engine to run the program in
    #[arg(short, long, default_value_t = Engine::VM)]
    engine: Engine,
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
            Engine::VM => "vm".fmt(f),
            Engine::Eval => "eval".fmt(f),
        }
    }
}

const INPUT: &str = "
let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
fibonacci(21);
";

fn main() {
    let cli = Cli::parse();

    let duration: Duration;
    let result: Object;

    let l = Lexer::new(INPUT.as_bytes());
    let mut p = monkey::Parser::new(l);
    let program = p.parse_program();

    match cli.engine {
        Engine::VM => {
            let mut comp = Compiler::new();
            comp.compile(program).expect("compiler error:");

            let mut machine = VM::new(comp.bytecode());

            let start = Instant::now();
            machine.run().expect("vm error:");

            duration = start.elapsed();
            result = machine.last_popped_stack_elem();
        }
        Engine::Eval => {
            let env = Environment::new(None);
            let start = Instant::now();
            result = program.eval(env).unwrap();
            duration = start.elapsed();
        }
    }

    println!(
        "engine={}, result={}, duration={:?}",
        cli.engine, result, duration
    );
}
