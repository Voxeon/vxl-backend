use criterion::{criterion_group, criterion_main, Criterion};
use voxl_backend::lexer::Lexer;
use voxl_backend::parser::Parser;

fn parse(input: Vec<char>) {
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let parser = Parser::new(tokens);
    parser.parse().unwrap();
}

pub fn benchmark_compiler(c: &mut Criterion) {
    c.bench_function("parser 1", |b| {
        b.iter(|| {
            parse(
                String::from(
                    "%begin main\n\
                    %import print from stdio\n\n\
                    func main -> int\n\
                        @print(\"test\")\n\
                    end(func)",
                )
                .chars()
                .collect(),
            )
        })
    });

    c.bench_function("parser 2", |b| {
        b.iter(|| {
            parse(
                String::from(
                    "%begin main\n\
                    %import print from stdio\n\n\
                    func add(a(int), b(int)) -> int\n\
                        return $a + $b\n\
                    end(func)\n\
                    \n\
                    func main -> int\n\
                        @print(@int_to_string(@add(5, 3)))\n\
                    end(func)\n",
                )
                .chars()
                .collect(),
            )
        })
    });

    c.bench_function("parser 3", |b| {
        b.iter(|| {
            parse(
                String::from(
                    "%begin main\n\
                    %import print from stdio\n\n\
                    func add(a(int), b(int)) -> int\n\
                        return $a + $b\n\
                    end(func)\n\
                    \n\
                    func sub(a(int), b(int)) -> int\n\
                        return $a - $b\n\
                    end(func)\n\
                    \n\
                    func main -> int\n\
                        if @add(5, 3) < 62\n\
                            @print(\"Its okay!\")\n\
                        else\n\
                            @print(\"Its not okay!\")\n\
                        end(if)\n\
                        \n
                        for i : [@add(5, 3) ; 100 ; @sub(5, 4)]\n\
                        end(for)\n\
                        @print(@int_to_string(@add(5, 3)))\n\
                    end(func)",
                )
                .chars()
                .collect(),
            )
        })
    });
}

criterion_group!(benches, benchmark_compiler);
criterion_main!(benches);
