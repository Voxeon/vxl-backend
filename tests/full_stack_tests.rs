use std::collections::VecDeque;

use vxl_backend::compiler::context::Context;
use vxl_backend::compiler::operation::Operation;
use vxl_backend::compiler::Compiler;
use vxl_backend::lexer::Lexer;
use vxl_backend::parser::Parser;
use vxl_backend::pre_processor::PreProcessor;

fn compile_test<const S: usize>(file_contents: [&str; S], operations: &[Operation]) {
    let mut trees = VecDeque::new();

    for input in file_contents.into_iter() {
        let lexer = Lexer::new(input.chars().collect());
        let tokens = lexer.tokenize().unwrap();
        let parser = Parser::new(tokens);
        let tree = parser.parse().unwrap();

        trees.push_back(tree);
    }

    let processor = PreProcessor::new(trees);

    let compiled_operations = Compiler::new(processor.process().unwrap()).into_operations();

    for operation in operations.iter().enumerate() {
        assert_eq!(operation, (operation.0, &compiled_operations[operation.0]));
    }

    assert_eq!(operations, compiled_operations);
}

#[test]
fn test_basic_addition() {
    let file_contents = [include_str!("../test_programs/basic_addition/main.vxl")];
    let operations = [Operation::BeginContext(Context::new("root"))];

    compile_test(file_contents, &operations);
}
