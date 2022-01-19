use super::*;

macro_rules! order_operator_test {
    ($name:ident, $tktp:ident, $tok_str:expr) => {
        paste::paste! {
            mod [<test_ $name _binary_expression>] {
                use super::*;

                #[test]
                fn [<test_ $name _binary_expression_1>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                                    Value::Integer(22),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                                    Value::Integer(23),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    let function_body = CompilableBlockNode::new(
                        None,
                        HashMap::new(),
                        vec![CompilableStatementNode::expression_statement(
                            CompilableExpressionNode::[<$name _binary_expression>](
                                CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
                                CompilableExpressionNode::literal_expression(Value::Integer(23)).wrapped(),
                                Type::Integer,
                            )
                            .wrapped(),
                            Some(Type::Boolean),
                        )
                        .wrapped()],
                        false,
                    )
                    .wrapped();

                    assert_eq!(
                        resolver.run().unwrap(),
                        CompilableProgram::new(
                            vec![CompilableFunction::new(
                                "__fn_8_root_main".to_string(),
                                Vec::new(),
                                None,
                                function_body
                            )],
                            Vec::new()
                        )
                    );
                }

                #[test]
                fn [<test_ $name _binary_expression_2>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(
                                        TokenType::FloatLiteralToken,
                                        "22.22".to_string(),
                                        0,
                                        0,
                                        None,
                                    ),
                                    Value::Float(22.22),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(
                                        TokenType::FloatLiteralToken,
                                        "23.22".to_string(),
                                        0,
                                        0,
                                        None,
                                    ),
                                    Value::Float(23.22),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    let function_body = CompilableBlockNode::new(
                        None,
                        HashMap::new(),
                        vec![CompilableStatementNode::expression_statement(
                            CompilableExpressionNode::[<$name _binary_expression>](
                                CompilableExpressionNode::literal_expression(Value::Float(22.22)).wrapped(),
                                CompilableExpressionNode::literal_expression(Value::Float(23.22)).wrapped(),
                                Type::Float,
                            )
                            .wrapped(),
                            Some(Type::Boolean),
                        )
                        .wrapped()],
                        false,
                    )
                    .wrapped();

                    assert_eq!(
                        resolver.run().unwrap(),
                        CompilableProgram::new(
                            vec![CompilableFunction::new(
                                "__fn_8_root_main".to_string(),
                                Vec::new(),
                                None,
                                function_body
                            )],
                            Vec::new()
                        )
                    );
                }

                #[test]
                fn [<test_ $name _binary_expression_3>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                                    Value::Integer(22),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new_identifier("true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    assert_eq!(
                        resolver.run().unwrap_err(),
                        ResolverError::lhs_does_not_match_rhs_binary_expression(
                            Some(Type::Integer),
                            Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                            Some(Type::Boolean)
                        )
                    )
                }

                #[test]
                fn [<test_ $name _binary_expression_4>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new_identifier("true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new_identifier("true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    assert_eq!(
                        resolver.run().unwrap_err(),
                        ResolverError::expected_numeric_values_for_binary_expression(
                            Some(Type::Boolean),
                            Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                        )
                    );
                }

                #[cfg(feature = "test-intensive")]
                mod intensive {
                    use super::*;

                    define_intensive_test!([<test_ $name _binary_expression_1>]);
                    define_intensive_test!([<test_ $name _binary_expression_2>]);
                    define_intensive_test!([<test_ $name _binary_expression_3>]);
                    define_intensive_test!([<test_ $name _binary_expression_4>]);
                }
            }
        }
    };
}

macro_rules! math_operator_test {
    ($name:ident, $tktp:ident, $tok_str:expr) => {
        paste::paste! {
            mod [<test_ $name _binary_expression>] {
                use super::*;

                #[test]
                fn [<test_ $name _binary_expression_1>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                                    Value::Integer(22),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                                    Value::Integer(23),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    let function_body = CompilableBlockNode::new(
                        None,
                        HashMap::new(),
                        vec![CompilableStatementNode::expression_statement(
                            CompilableExpressionNode::[<$name _binary_expression>](
                                CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
                                CompilableExpressionNode::literal_expression(Value::Integer(23)).wrapped(),
                                Type::Integer,
                            )
                            .wrapped(),
                            Some(Type::Integer),
                        )
                        .wrapped()],
                        false,
                    )
                    .wrapped();

                    assert_eq!(
                        resolver.run().unwrap(),
                        CompilableProgram::new(
                            vec![CompilableFunction::new(
                                "__fn_8_root_main".to_string(),
                                Vec::new(),
                                None,
                                function_body
                            )],
                            Vec::new()
                        )
                    );
                }

                #[test]
                fn [<test_ $name _binary_expression_2>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(
                                        TokenType::FloatLiteralToken,
                                        "22.22".to_string(),
                                        0,
                                        0,
                                        None,
                                    ),
                                    Value::Float(22.22),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(
                                        TokenType::FloatLiteralToken,
                                        "23.22".to_string(),
                                        0,
                                        0,
                                        None,
                                    ),
                                    Value::Float(23.22),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    let function_body = CompilableBlockNode::new(
                        None,
                        HashMap::new(),
                        vec![CompilableStatementNode::expression_statement(
                            CompilableExpressionNode::[<$name _binary_expression>](
                                CompilableExpressionNode::literal_expression(Value::Float(22.22)).wrapped(),
                                CompilableExpressionNode::literal_expression(Value::Float(23.22)).wrapped(),
                                Type::Float,
                            )
                            .wrapped(),
                            Some(Type::Float),
                        )
                        .wrapped()],
                        false,
                    )
                    .wrapped();

                    assert_eq!(
                        resolver.run().unwrap(),
                        CompilableProgram::new(
                            vec![CompilableFunction::new(
                                "__fn_8_root_main".to_string(),
                                Vec::new(),
                                None,
                                function_body
                            )],
                            Vec::new()
                        )
                    );
                }

                #[test]
                fn [<test_ $name _binary_expression_3>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                                    Value::Integer(22),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new_identifier("true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    assert_eq!(
                        resolver.run().unwrap_err(),
                        ResolverError::lhs_does_not_match_rhs_binary_expression(
                            Some(Type::Integer),
                            Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                            Some(Type::Boolean)
                        )
                    )
                }

                #[test]
                fn [<test_ $name _binary_expression_4>]() {
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
                        vec![StatementNode::expression_statement(
                            ExpressionNode::binary_expression(
                                ExpressionNode::literal_expression(
                                    Token::new_identifier("true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new_identifier("true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                            )
                            .wrapped(),
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

                    assert_eq!(
                        resolver.run().unwrap_err(),
                        ResolverError::expected_numeric_values_for_binary_expression(
                            Some(Type::Boolean),
                            Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                        )
                    );
                }

                #[cfg(feature = "test-intensive")]
                mod intensive {
                    use super::*;

                    define_intensive_test!([<test_ $name _binary_expression_1>]);
                    define_intensive_test!([<test_ $name _binary_expression_2>]);
                    define_intensive_test!([<test_ $name _binary_expression_3>]);
                    define_intensive_test!([<test_ $name _binary_expression_4>]);
                }
            }
        }
    };
}

mod test_array_allocation_expression {}
mod test_literal_expression {}

mod test_not_unary_expression {}
mod test_negate_unary_expression {}

mod test_equal_to_binary_expression {
    use super::*;

    #[test]
    fn test_equal_to_binary_expression_1() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Boolean(true),
                    )
                    .wrapped(),
                    Token::new(TokenType::EqualsToken, "=".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Boolean(true),
                    )
                    .wrapped(),
                )
                .wrapped(),
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

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::equal_to_binary_expression(
                    CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                    CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                    Type::Boolean,
                )
                .wrapped(),
                Some(Type::Boolean),
            )
            .wrapped()],
            false,
        )
        .wrapped();

        assert_eq!(
            resolver.run().unwrap(),
            CompilableProgram::new(
                vec![CompilableFunction::new(
                    "__fn_8_root_main".to_string(),
                    Vec::new(),
                    None,
                    function_body
                )],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_equal_to_binary_expression_2() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                    Token::new(TokenType::EqualsToken, "=".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(23),
                    )
                    .wrapped(),
                )
                .wrapped(),
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

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::equal_to_binary_expression(
                    CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
                    CompilableExpressionNode::literal_expression(Value::Integer(23)).wrapped(),
                    Type::Integer,
                )
                .wrapped(),
                Some(Type::Boolean),
            )
            .wrapped()],
            false,
        )
        .wrapped();

        assert_eq!(
            resolver.run().unwrap(),
            CompilableProgram::new(
                vec![CompilableFunction::new(
                    "__fn_8_root_main".to_string(),
                    Vec::new(),
                    None,
                    function_body
                )],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_equal_to_binary_expression_3() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                    Token::new(TokenType::EqualsToken, "=".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Boolean(true),
                    )
                    .wrapped(),
                )
                .wrapped(),
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

        assert_eq!(
            resolver.run().unwrap_err(),
            ResolverError::lhs_does_not_match_rhs_binary_expression(
                Some(Type::Integer),
                Token::new(TokenType::EqualsToken, "=".to_string(), 0, 0, None),
                Some(Type::Boolean)
            )
        )
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_equal_to_binary_expression_1);
        define_intensive_test!(test_equal_to_binary_expression_2);
        define_intensive_test!(test_equal_to_binary_expression_3);
    }
}

mod test_not_equal_to_binary_expression {
    use super::*;

    #[test]
    fn test_not_equal_to_binary_expression_1() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Boolean(true),
                    )
                    .wrapped(),
                    Token::new(TokenType::BangEqualsToken, "!=".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Boolean(true),
                    )
                    .wrapped(),
                )
                .wrapped(),
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

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::not_equal_to_binary_expression(
                    CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                    CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                    Type::Boolean,
                )
                .wrapped(),
                Some(Type::Boolean),
            )
            .wrapped()],
            false,
        )
        .wrapped();

        assert_eq!(
            resolver.run().unwrap(),
            CompilableProgram::new(
                vec![CompilableFunction::new(
                    "__fn_8_root_main".to_string(),
                    Vec::new(),
                    None,
                    function_body
                )],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_not_equal_to_binary_expression_2() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                    Token::new(TokenType::BangEqualsToken, "!=".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(23),
                    )
                    .wrapped(),
                )
                .wrapped(),
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

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::not_equal_to_binary_expression(
                    CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
                    CompilableExpressionNode::literal_expression(Value::Integer(23)).wrapped(),
                    Type::Integer,
                )
                .wrapped(),
                Some(Type::Boolean),
            )
            .wrapped()],
            false,
        )
        .wrapped();

        assert_eq!(
            resolver.run().unwrap(),
            CompilableProgram::new(
                vec![CompilableFunction::new(
                    "__fn_8_root_main".to_string(),
                    Vec::new(),
                    None,
                    function_body
                )],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_not_equal_to_binary_expression_3() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                    Token::new(TokenType::BangEqualsToken, "!=".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Boolean(true),
                    )
                    .wrapped(),
                )
                .wrapped(),
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

        assert_eq!(
            resolver.run().unwrap_err(),
            ResolverError::lhs_does_not_match_rhs_binary_expression(
                Some(Type::Integer),
                Token::new(TokenType::BangEqualsToken, "!=".to_string(), 0, 0, None),
                Some(Type::Boolean)
            )
        )
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_not_equal_to_binary_expression_1);
        define_intensive_test!(test_not_equal_to_binary_expression_2);
        define_intensive_test!(test_not_equal_to_binary_expression_3);
    }
}

math_operator_test!(add, PlusToken, "+");
math_operator_test!(subtract, MinusToken, "-");
math_operator_test!(multiply, StarToken, "*");
math_operator_test!(divide, ForwardSlashToken, "/");
order_operator_test!(greater, GreaterThanToken, ">");
order_operator_test!(greater_equal, GreaterThanEqualToken, ">=");
order_operator_test!(less, LessThanToken, "<");
order_operator_test!(less_equal, LessThanEqualToken, "<=");

mod test_or_logical_expression {}
mod test_and_logical_expression {}
mod test_variable_expression {}
mod test_variable_assignment_expression {}
mod test_array_index_assignment_expression {}
mod test_grouping_expression {}

mod test_call_expression {
    use super::*;

    #[test]
    fn test_recursive_function_call() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::call_expression(
                    Token::new(TokenType::AtToken, "@".to_string(), 0, 0, None),
                    None,
                    Token::new_identifier("main".to_string(), 0, 0, None),
                    Vec::new(),
                )
                .wrapped(),
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

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::call_expression(
                    "__fn_8_root_main".to_string(),
                    Vec::new(),
                    None,
                )
                .wrapped(),
                None,
            )
            .wrapped()],
            false,
        )
        .wrapped();

        assert_eq!(
            resolver.run().unwrap(),
            CompilableProgram::new(
                vec![CompilableFunction::new(
                    "__fn_8_root_main".to_string(),
                    Vec::new(),
                    None,
                    function_body
                )],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_function_call() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::call_expression(
                    Token::new(TokenType::AtToken, "@".to_string(), 0, 0, None),
                    None,
                    Token::new_identifier("main_2".to_string(), 0, 0, None),
                    Vec::new(),
                )
                .wrapped(),
            )
            .wrapped()],
        )
        .wrapped();

        let second_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main_2".to_string(), 0, 0, None),
            Vec::new(),
            Some(Type::Integer),
            vec![StatementNode::return_statement(
                Token::new_identifier("return".to_string(), 0, 0, None),
                Some(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                ),
            )
            .wrapped()],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt);
        root_ast.push_statement(second_function_stmt);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::call_expression(
                    "__fn_a_root_main_2".to_string(),
                    Vec::new(),
                    Some(Type::Integer),
                )
                .wrapped(),
                Some(Type::Integer),
            )
            .wrapped()],
            false,
        )
        .wrapped();

        let mut output = resolver.run().unwrap();

        output.functions.sort_by(|a, b| a.label.cmp(&b.label));

        assert_eq!(
            output,
            CompilableProgram::new(
                vec![
                    CompilableFunction::new(
                        "__fn_8_root_main".to_string(),
                        Vec::new(),
                        None,
                        function_body
                    ),
                    CompilableFunction::new(
                        "__fn_a_root_main_2".to_string(),
                        Vec::new(),
                        Some(Type::Integer),
                        CompilableBlockNode::new(
                            None,
                            HashMap::new(),
                            vec![CompilableStatementNode::return_statement(
                                Some(
                                    CompilableExpressionNode::literal_expression(Value::Integer(
                                        22
                                    ))
                                    .wrapped()
                                ),
                                Some(Type::Integer),
                            )
                            .wrapped()],
                            true
                        )
                        .wrapped()
                    )
                ],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_function_call_2() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::call_expression(
                    Token::new(TokenType::AtToken, "@".to_string(), 0, 0, None),
                    None,
                    Token::new_identifier("main_2".to_string(), 0, 0, None),
                    vec![ExpressionNode::literal_expression(
                        Token::new(
                            TokenType::CharacterLiteralToken,
                            "a".to_string(),
                            0,
                            0,
                            None,
                        ),
                        Value::Character('a'),
                    )
                    .wrapped()],
                )
                .wrapped(),
            )
            .wrapped()],
        )
        .wrapped();

        let second_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main_2".to_string(), 0, 0, None),
            vec![(
                Token::new_identifier("abc".to_string(), 0, 0, None),
                Type::Character,
            )],
            Some(Type::Integer),
            vec![StatementNode::return_statement(
                Token::new_identifier("return".to_string(), 0, 0, None),
                Some(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                ),
            )
            .wrapped()],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt);
        root_ast.push_statement(second_function_stmt);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::call_expression(
                    "__fn_a_root_main_2".to_string(),
                    vec![
                        CompilableExpressionNode::literal_expression(Value::Character('a'))
                            .wrapped(),
                    ],
                    Some(Type::Integer),
                )
                .wrapped(),
                Some(Type::Integer),
            )
            .wrapped()],
            false,
        )
        .wrapped();

        let mut output = resolver.run().unwrap();
        output.functions.sort_by(|a, b| a.label.cmp(&b.label));

        assert_eq!(
            output,
            CompilableProgram::new(
                vec![
                    CompilableFunction::new(
                        "__fn_8_root_main".to_string(),
                        Vec::new(),
                        None,
                        function_body
                    ),
                    CompilableFunction::new(
                        "__fn_a_root_main_2".to_string(),
                        vec![("abc".to_string(), Type::Character)],
                        Some(Type::Integer),
                        CompilableBlockNode::new(
                            None,
                            HashMap::new(),
                            vec![CompilableStatementNode::return_statement(
                                Some(
                                    CompilableExpressionNode::literal_expression(Value::Integer(
                                        22
                                    ))
                                    .wrapped()
                                ),
                                Some(Type::Integer),
                            )
                            .wrapped()],
                            true
                        )
                        .wrapped()
                    )
                ],
                Vec::new()
            )
        );
    }

    #[test]
    fn test_function_call_3() {
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
            vec![StatementNode::expression_statement(
                ExpressionNode::call_expression(
                    Token::new(TokenType::AtToken, "@".to_string(), 0, 0, None),
                    None,
                    Token::new_identifier("main_2".to_string(), 0, 0, None),
                    vec![ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped()],
                )
                .wrapped(),
            )
            .wrapped()],
        )
        .wrapped();

        let second_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main_2".to_string(), 0, 0, None),
            vec![(
                Token::new_identifier("abc".to_string(), 0, 0, None),
                Type::Character,
            )],
            Some(Type::Integer),
            vec![StatementNode::return_statement(
                Token::new_identifier("return".to_string(), 0, 0, None),
                Some(
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
                    )
                    .wrapped(),
                ),
            )
            .wrapped()],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt);
        root_ast.push_statement(second_function_stmt);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        assert_eq!(
            resolver.run().unwrap_err(),
            ResolverError::invalid_function_call_argument(
                0,
                Type::Character,
                Some(Type::Integer),
                Token::new_identifier("main_2".to_string(), 0, 0, None),
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_recursive_function_call);
        define_intensive_test!(test_function_call);
        define_intensive_test!(test_function_call_2);
        define_intensive_test!(test_function_call_3);
    }
}

mod test_constructor_call_expression {}
mod test_array_index_expression {}
