use super::*;

#[test]
fn test_basic_main_function() {
    let mut input = VecDeque::new();
    let mut root_ast = AST::new();

    root_ast.push_statement(
        StatementNode::pre_processor_command_statement(
            Token::new_identifier("begin".to_string(), 1, 1, None),
            PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                ROOT_MODULE_NAME.to_string(),
                1,
                1,
                None,
            )),
        )
        .wrapped(),
    );

    let main_function_stmt = StatementNode::function_statement(
        Token::new_identifier("func".to_string(), 2, 1, None),
        Token::new_identifier("main".to_string(), 0, 0, None),
        Vec::new(),
        None,
        Vec::new(),
    )
    .wrapped();

    root_ast.push_statement(main_function_stmt.clone());

    input.push_front(root_ast);

    let processed_output = PreProcessor::new(input).process().unwrap();
    let resolver = Resolver::default()
        .with_modules(processed_output)
        .with_standard_library(false);

    resolver.run().unwrap();
}

#[test]
fn test_integer_main_function() {
    let mut input = VecDeque::new();
    let mut root_ast = AST::new();

    root_ast.push_statement(
        StatementNode::pre_processor_command_statement(
            Token::new_identifier("begin".to_string(), 1, 1, None),
            PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                ROOT_MODULE_NAME.to_string(),
                1,
                1,
                None,
            )),
        )
        .wrapped(),
    );

    let main_function_stmt = StatementNode::function_statement(
        Token::new_identifier("func".to_string(), 2, 1, None),
        Token::new_identifier("main".to_string(), 0, 0, None),
        vec![(
            Token::new_identifier("a".to_string(), 0, 0, None),
            Type::Integer,
        )],
        Some(Type::Integer),
        vec![StatementNode::return_statement(
            Token::new_identifier("return".to_string(), 2, 1, None),
            Some(
                ExpressionNode::literal_expression(
                    Token::new(TokenType::IntegerLiteralToken, "3".to_string(), 0, 0, None),
                    Value::Integer(3),
                )
                .wrapped(),
            ),
        )
        .wrapped()],
    )
    .wrapped();

    root_ast.push_statement(main_function_stmt.clone());

    input.push_front(root_ast);

    let processed_output = PreProcessor::new(input).process().unwrap();
    let resolver = Resolver::default()
        .with_modules(processed_output)
        .with_standard_library(false);

    resolver.run().unwrap();
}

#[test]
fn test_undefined_struct_function_arg() {
    let mut input = VecDeque::new();
    let mut root_ast = AST::new();

    root_ast.push_statement(
        StatementNode::pre_processor_command_statement(
            Token::new_identifier("begin".to_string(), 1, 1, None),
            PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                ROOT_MODULE_NAME.to_string(),
                1,
                1,
                None,
            )),
        )
        .wrapped(),
    );

    let main_function_stmt = StatementNode::function_statement(
        Token::new_identifier("func".to_string(), 2, 1, None),
        Token::new_identifier("main".to_string(), 0, 0, None),
        vec![(
            Token::new_identifier("a".to_string(), 0, 0, None),
            Type::Struct {
                name: String::from("name"),
                module: String::from("root"),
            },
        )],
        Some(Type::Integer),
        Vec::new(),
    )
    .wrapped();

    root_ast.push_statement(main_function_stmt.clone());

    input.push_front(root_ast);

    let processed_output = PreProcessor::new(input).process().unwrap();
    let resolver = Resolver::default()
        .with_modules(processed_output)
        .with_standard_library(false);

    assert_eq!(
        resolver.run().unwrap_err(),
        ResolverError::no_object_defined_with_name_in_module_in_function(
            "name".to_string(),
            "root".to_string(),
            Token::new_identifier("main".to_string(), 0, 0, None)
        )
    );
}

#[test]
fn test_undefined_struct_function_return_type() {
    let mut input = VecDeque::new();
    let mut root_ast = AST::new();

    root_ast.push_statement(
        StatementNode::pre_processor_command_statement(
            Token::new_identifier("begin".to_string(), 1, 1, None),
            PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                ROOT_MODULE_NAME.to_string(),
                1,
                1,
                None,
            )),
        )
        .wrapped(),
    );

    let main_function_stmt = StatementNode::function_statement(
        Token::new_identifier("func".to_string(), 2, 1, None),
        Token::new_identifier("main".to_string(), 0, 0, None),
        Vec::new(),
        Some(Type::Struct {
            name: String::from("name"),
            module: String::from("root"),
        }),
        Vec::new(),
    )
    .wrapped();

    root_ast.push_statement(main_function_stmt.clone());

    input.push_front(root_ast);

    let processed_output = PreProcessor::new(input).process().unwrap();
    let resolver = Resolver::default()
        .with_modules(processed_output)
        .with_standard_library(false);

    assert_eq!(
        resolver.run().unwrap_err(),
        ResolverError::no_object_defined_with_name_in_module_in_function(
            "name".to_string(),
            "root".to_string(),
            Token::new_identifier("main".to_string(), 0, 0, None)
        )
    );
}

#[test]
fn test_variable_scoping() {
    todo!();
}
