use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use monkey::{Compiler, Environment, Evaluator, Lexer, Parser, VM};

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
";

fn bench_fibs(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci");
    for i in [20_i64, 21_i64].iter() {
        group.bench_with_input(BenchmarkId::new("vm", i), i, |b, i| {
            b.iter(|| {
                let input = [INPUT, &format!("fibonacci({i});")].concat();
                let l = Lexer::new(input.as_bytes());
                let mut p = Parser::new(l);
                let program = p.parse_program();

                let mut comp = Compiler::new();
                comp.compile(program).expect("compiler error:");

                let mut machine = VM::new(comp.bytecode());
                machine.run().expect("vm error:");

                machine.last_popped_stack_elem()
            });
        });

        group.bench_with_input(BenchmarkId::new("eval", i), i, |b, i| {
            let input = [INPUT, &format!("fibonacci({i});")].concat();
            b.iter(|| {
                let l = Lexer::new(input.as_bytes());
                let mut p = Parser::new(l);
                let program = p.parse_program();

                let env = Environment::new(None);
                program.eval(env).unwrap();
            });
        });
    }
    group.finish();
}

criterion_group!(benches, bench_fibs);
criterion_main!(benches);
