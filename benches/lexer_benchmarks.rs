use criterion::{criterion_group, criterion_main, Criterion};
use vxl_backend::lexer::Lexer;

fn tokenize(input: Vec<char>) {
    let lexer = Lexer::new(input);
    lexer.tokenize().unwrap();
}

pub fn benchmark_lexer(c: &mut Criterion) {
    c.bench_function("lexer 1", |b| {
        b.iter(|| tokenize(String::from("5 + 5").chars().collect()))
    });

    c.bench_function("lexer 2", |b| {
        b.iter(|| tokenize(String::from("5 + 5 + 32 + 12 + 920").chars().collect()))
    });

    c.bench_function("lexer 3", |b| {
        b.iter(|| {
            tokenize(
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

    c.bench_function("lexer 4", |b| {
        b.iter(|| {
            tokenize(
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

    c.bench_function("lexer 5", |b| {
        b.iter(|| {
            tokenize(
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
                        \n\
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

criterion_group!(benches, benchmark_lexer);
criterion_main!(benches);
