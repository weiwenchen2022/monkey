#![allow(dead_code)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

use monkey::{self, Compiler, Environment, Lexer, Object, Parser, VM};

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

fn eval(input: &str) -> Object {
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();

    let env = Environment::default();
    monkey::eval(program, &env).unwrap()
}

fn eval_benchmark(c: &mut Criterion) {
    let input = [INPUT, "fibonacci(20);"].concat();
    c.bench_function("fib 20", |b| b.iter(|| eval(&input)));
}

fn vm(input: &str) -> Object {
    let l = Lexer::new(input.as_bytes());
    let mut p = Parser::new(l);
    let program = p.parse_program();

    let mut comp = Compiler::new();
    comp.compile(program).expect("compiler error:");

    let mut machine = VM::new(comp.bytecode());
    machine.run().expect("vm error:");

    machine.last_popped_stack_elem()
}

fn vm_benchmark(c: &mut Criterion) {
    let input = [INPUT, "fibonacci(20);"].concat();
    c.bench_function("fib 20", |b| b.iter(|| vm(&input)));
}

fn bench_fibs(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci");
    for i in [20_i64, 21_i64].iter() {
        group.bench_with_input(BenchmarkId::new("vm", i), i, |b, i| {
            let input = [INPUT, &format!("fibonacci({i});")].concat();
            b.iter(|| vm(&input));
        });

        group.bench_with_input(BenchmarkId::new("eval", i), i, |b, i| {
            let input = [INPUT, &format!("fibonacci({i});")].concat();
            b.iter(|| eval(&input));
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    vm_benchmark,
    // eval_benchmark,
    // bench_fibs,
);
criterion_main!(benches);
