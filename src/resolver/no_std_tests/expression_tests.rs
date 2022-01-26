use super::*;
use crate::ast::ArrayLiteral;

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

macro_rules! logical_expression_test {
    ($name:ident, $tktp:ident, $tok_str:expr) => {
        paste::paste! {
            mod [<test_ $name _logical_expression>] {
                use super::*;

                #[test]
                fn [<test_ $name _logical_expression_1>]() {
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
                            ExpressionNode::logical_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::TrueToken, "true".to_string(), 0, 0, None),
                                    Value::Boolean(true),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                                    Value::Boolean(false),
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
                            CompilableExpressionNode::[<$name _logical_expression>](
                                CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                                CompilableExpressionNode::literal_expression(Value::Boolean(false)).wrapped(),
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
                fn [<test_ $name _logical_expression_2>]() {
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
                            ExpressionNode::logical_expression(
                                ExpressionNode::binary_expression(
                                    ExpressionNode::literal_expression(
                                        Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0, 0, None),
                                        Value::Integer(12),
                                    )
                                    .wrapped(),
                                    Token::new(TokenType::LessThanToken, "<".to_string(), 0, 0, None),
                                    ExpressionNode::literal_expression(
                                        Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0, 0, None),
                                        Value::Integer(12),
                                    )
                                    .wrapped(),
                                ).wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                                    Value::Boolean(false),
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
                            CompilableExpressionNode::[<$name _logical_expression>](
                                CompilableExpressionNode::less_binary_expression(
                                    CompilableExpressionNode::literal_expression(
                                        Value::Integer(12),
                                    )
                                    .wrapped(),
                                    CompilableExpressionNode::literal_expression(
                                        Value::Integer(12),
                                    )
                                    .wrapped(),
                                    Type::Integer,
                                ).wrapped(),
                                CompilableExpressionNode::literal_expression(Value::Boolean(false)).wrapped(),
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
                fn [<test_ $name _logical_expression_3>]() {
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
                            ExpressionNode::logical_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0, 0, None),
                                    Value::Integer(12),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                                    Value::Boolean(false),
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
                        ResolverError::logical_expression_left_non_boolean(
                            Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                            Some(Type::Integer)
                        )
                    );
                }

                #[test]
                fn [<test_ $name _logical_expression_4>]() {
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
                            ExpressionNode::logical_expression(
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                                    Value::Boolean(false),
                                )
                                .wrapped(),
                                Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                                ExpressionNode::literal_expression(
                                    Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0, 0, None),
                                    Value::Integer(12),
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
                        ResolverError::logical_expression_right_non_boolean(
                            Token::new(TokenType::$tktp, $tok_str.to_string(), 0, 0, None),
                            Some(Type::Integer)
                        )
                    );
                }
            }
        }
    };
}

mod test_array_allocation_expression {
    use super::*;

    #[test]
    fn test_array_allocation_expression_1() {
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
                ExpressionNode::array_allocation_expression(
                    Token::new(
                        TokenType::StringLiteralToken,
                        "testing 12345.....".to_string(),
                        0,
                        0,
                        None,
                    ),
                    ArrayLiteral::from("testing 12345....."),
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "18".to_string(), 0, 0, None),
                        Value::Integer(18),
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
                CompilableExpressionNode::array_allocation_expression(
                    Type::Array(Box::new(Type::Character)),
                    "testing 12345....."
                        .chars()
                        .map(|c| Value::Character(c))
                        .collect(),
                    CompilableExpressionNode::literal_expression(Value::Integer(18)).wrapped(),
                )
                .wrapped(),
                Some(Type::Array(Box::new(Type::Character))),
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
    fn test_array_allocation_expression_2() {
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
                ExpressionNode::array_allocation_expression(
                    Token::new(
                        TokenType::StringLiteralToken,
                        "testing 12345.....".to_string(),
                        0,
                        0,
                        None,
                    ),
                    ArrayLiteral::from("testing 12345....."),
                    ExpressionNode::literal_expression(
                        Token::new(
                            TokenType::CharacterLiteralToken,
                            "a".to_string(),
                            0,
                            0,
                            None,
                        ),
                        Value::Character('a'),
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
            ResolverError::invalid_array_literal_count_type(
                Token::new(
                    TokenType::StringLiteralToken,
                    "testing 12345.....".to_string(),
                    0,
                    0,
                    None,
                ),
                Some(Type::Character),
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_array_allocation_expression_1);
        define_intensive_test!(test_array_allocation_expression_2);
    }
}

mod test_literal_expression {
    use super::*;

    #[test]
    fn test_true_literal_expression() {
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
                ExpressionNode::literal_expression(
                    Token::new_identifier("true".to_string(), 0, 0, None),
                    Value::Boolean(true),
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
                CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
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
    fn test_false_literal_expression() {
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
                ExpressionNode::literal_expression(
                    Token::new_identifier("false".to_string(), 0, 0, None),
                    Value::Boolean(false),
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
                CompilableExpressionNode::literal_expression(Value::Boolean(false)).wrapped(),
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
    fn test_float_literal_expression() {
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
                ExpressionNode::literal_expression(
                    Token::new(
                        TokenType::FloatLiteralToken,
                        "52.22".to_string(),
                        0,
                        0,
                        None,
                    ),
                    Value::Float(52.22),
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
                CompilableExpressionNode::literal_expression(Value::Float(52.22)).wrapped(),
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
    fn test_integer_literal_expression() {
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
                ExpressionNode::literal_expression(
                    Token::new(TokenType::IntegerLiteralToken, "52".to_string(), 0, 0, None),
                    Value::Integer(52),
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
                CompilableExpressionNode::literal_expression(Value::Integer(52)).wrapped(),
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
    fn test_char_literal_expression() {
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
                ExpressionNode::literal_expression(
                    Token::new(
                        TokenType::CharacterLiteralToken,
                        "a".to_string(),
                        0,
                        0,
                        None,
                    ),
                    Value::Character('a'),
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
                CompilableExpressionNode::literal_expression(Value::Character('a')).wrapped(),
                Some(Type::Character),
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

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_true_literal_expression);
        define_intensive_test!(test_false_literal_expression);
        define_intensive_test!(test_float_literal_expression);
        define_intensive_test!(test_integer_literal_expression);
        define_intensive_test!(test_char_literal_expression);
    }
}

mod test_not_unary_expression {
    use super::*;

    #[test]
    fn test_not_unary_expression_1() {
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
                ExpressionNode::unary_expression(
                    Token::new(TokenType::NotToken, "not".to_string(), 0, 0, None),
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
                CompilableExpressionNode::not_unary_expression(
                    CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
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
    fn test_not_unary_expression_2() {
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
                ExpressionNode::unary_expression(
                    Token::new(TokenType::NotToken, "not".to_string(), 0, 0, None),
                    ExpressionNode::unary_expression(
                        Token::new(TokenType::NotToken, "not".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new_identifier("true".to_string(), 0, 0, None),
                            Value::Boolean(true),
                        )
                        .wrapped(),
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
                CompilableExpressionNode::not_unary_expression(
                    CompilableExpressionNode::not_unary_expression(
                        CompilableExpressionNode::literal_expression(Value::Boolean(true))
                            .wrapped(),
                    )
                    .wrapped(),
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
    fn test_not_unary_expression_3() {
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
                ExpressionNode::unary_expression(
                    Token::new(TokenType::NotToken, "not".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("true".to_string(), 0, 0, None),
                        Value::Integer(22),
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
            ResolverError::invalid_unary_not_type(
                Token::new(TokenType::NotToken, "not".to_string(), 0, 0, None),
                Some(Type::Integer)
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_not_unary_expression_1);
        define_intensive_test!(test_not_unary_expression_2);
        define_intensive_test!(test_not_unary_expression_3);
    }
}

mod test_negate_unary_expression {
    use super::*;

    #[test]
    fn test_negate_unary_expression_1() {
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
                ExpressionNode::unary_expression(
                    Token::new(TokenType::MinusToken, "-".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                        Value::Integer(22),
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
                CompilableExpressionNode::negate_unary_expression(
                    CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
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
    fn test_negate_unary_expression_2() {
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
                ExpressionNode::unary_expression(
                    Token::new(TokenType::MinusToken, "-".to_string(), 0, 0, None),
                    ExpressionNode::unary_expression(
                        Token::new(TokenType::MinusToken, "-".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new(
                                TokenType::IntegerLiteralToken,
                                "22".to_string(),
                                0,
                                0,
                                None,
                            ),
                            Value::Integer(22),
                        )
                        .wrapped(),
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
                CompilableExpressionNode::negate_unary_expression(
                    CompilableExpressionNode::negate_unary_expression(
                        CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
                    )
                    .wrapped(),
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
    fn test_negate_unary_expression_3() {
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
                ExpressionNode::unary_expression(
                    Token::new(TokenType::MinusToken, "-".to_string(), 0, 0, None),
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
            ResolverError::invalid_unary_negate_type(
                Token::new(TokenType::MinusToken, "-".to_string(), 0, 0, None),
                Some(Type::Boolean)
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_negate_unary_expression_1);
        define_intensive_test!(test_negate_unary_expression_2);
        define_intensive_test!(test_negate_unary_expression_3);
    }
}

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
logical_expression_test!(or, OrToken, "or");
logical_expression_test!(and, AndToken, "and");

mod test_variable_assignment_expression {
    use super::*;

    #[test]
    fn test_variable_assignment_expression_1() {
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
            vec![
                StatementNode::variable_declaration_statement(
                    Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
                    Token::new_identifier("abc".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("false".to_string(), 0, 0, None),
                        Value::Boolean(false),
                    )
                    .wrapped(),
                )
                .wrapped(),
                StatementNode::expression_statement(
                    ExpressionNode::assignment_expression(
                        ExpressionNode::variable_expression(
                            Token::new(TokenType::DollarToken, "$".to_string(), 0, 0, None),
                            Variable::from(vec![Token::new_identifier(
                                "abc".to_string(),
                                0,
                                0,
                                None,
                            )]),
                        )
                        .wrapped(),
                        Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new_identifier("true".to_string(), 0, 0, None),
                            Value::Boolean(true),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
            ],
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
            hashmap!["abc".to_string() ; Type::Boolean],
            vec![
                CompilableStatementNode::variable_declaration_statement(
                    "abc".to_string(),
                    CompilableExpressionNode::literal_expression(Value::Boolean(false)).wrapped(),
                    Type::Boolean,
                )
                .wrapped(),
                CompilableStatementNode::expression_statement(
                    CompilableExpressionNode::variable_assignment_expression(
                        Variable::from(vec![Token::new_identifier("abc".to_string(), 0, 0, None)]),
                        CompilableExpressionNode::literal_expression(Value::Boolean(true))
                            .wrapped(),
                    )
                    .wrapped(),
                    None,
                )
                .wrapped(),
            ],
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
    fn test_variable_assignment_expression_2() {
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
            vec![
                StatementNode::variable_declaration_statement(
                    Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
                    Token::new_identifier("abc".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("false".to_string(), 0, 0, None),
                        Value::Boolean(false),
                    )
                    .wrapped(),
                )
                .wrapped(),
                StatementNode::expression_statement(
                    ExpressionNode::assignment_expression(
                        ExpressionNode::variable_expression(
                            Token::new(TokenType::DollarToken, "$".to_string(), 0, 0, None),
                            Variable::from(vec![Token::new_identifier(
                                "abc".to_string(),
                                0,
                                0,
                                None,
                            )]),
                        )
                        .wrapped(),
                        Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new(
                                TokenType::IntegerLiteralToken,
                                "22".to_string(),
                                0,
                                0,
                                None,
                            ),
                            Value::Integer(22),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
            ],
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
            ResolverError::invalid_assignment_type(
                Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                Some(Type::Boolean),
                Some(Type::Integer)
            )
        );
    }

    #[test]
    fn test_variable_assignment_expression_3() {
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
            vec![
                StatementNode::variable_declaration_statement(
                    Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
                    Token::new_identifier("abc".to_string(), 0, 0, None),
                    ExpressionNode::literal_expression(
                        Token::new_identifier("false".to_string(), 0, 0, None),
                        Value::Boolean(false),
                    )
                    .wrapped(),
                )
                .wrapped(),
                StatementNode::expression_statement(
                    ExpressionNode::assignment_expression(
                        ExpressionNode::variable_expression(
                            Token::new(TokenType::DollarToken, "$".to_string(), 0, 0, None),
                            Variable::from(vec![Token::new_identifier(
                                "abcd".to_string(),
                                0,
                                0,
                                None,
                            )]),
                        )
                        .wrapped(),
                        Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::TrueToken, "true".to_string(), 0, 0, None),
                            Value::Boolean(true),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
            ],
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
            ResolverError::no_variable_declared_with_name_in_scope(
                Token::new_identifier("main".to_string(), 0, 0, None),
                vec![Token::new_identifier("abcd".to_string(), 0, 0, None)].into()
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_variable_assignment_expression_1);
        define_intensive_test!(test_variable_assignment_expression_2);
        define_intensive_test!(test_variable_assignment_expression_3);
    }
}

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

mod test_constructor_call_expression {
    use super::*;

    #[test]
    fn test_constructor_call_expression_1() {
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

        let basic_struct_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_A".to_string(), 0, 0, None),
            hashmap!["a".to_string() ; Type::Integer],
        )
        .wrapped();

        root_ast.push_statement(basic_struct_ast);

        let main_function_stmt =
            StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                vec![StatementNode::expression_statement(
                ExpressionNode::constructor_call_expression(
                    Token::new(TokenType::IdentifierToken, "struct_A".to_string(), 0, 0, None),
                    None,
                    hashmap![
                        Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0,0, None),
                            Value::Integer(12)).wrapped()
                    ]
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
                CompilableExpressionNode::constructor_call_expression(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap!["a".to_string() ; CompilableExpressionNode::literal_expression(Value::Integer(12)).wrapped()]
                )
                .wrapped(),
                Some(Type::new_struct("struct_A", "root")),
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
                vec![CompilableStruct::new(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap!["a".to_string() ; Type::Integer]
                )]
            )
        );
    }

    #[test]
    fn test_constructor_call_expression_2() {
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

        let main_function_stmt =
            StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                vec![StatementNode::expression_statement(
                ExpressionNode::constructor_call_expression(
                    Token::new(TokenType::IdentifierToken, "struct_A".to_string(), 0, 0, None),
                    None,
                    hashmap![
                        Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0,0, None),
                            Value::Integer(12)).wrapped()
                    ]
                )
                .wrapped(),
            )
            .wrapped()],
            )
            .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        let basic_struct_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_A".to_string(), 0, 0, None),
            hashmap!["a".to_string() ; Type::Integer],
        )
        .wrapped();

        root_ast.push_statement(basic_struct_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::constructor_call_expression(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap!["a".to_string() ; CompilableExpressionNode::literal_expression(Value::Integer(12)).wrapped()]
                )
                .wrapped(),
                Some(Type::new_struct("struct_A", "root")),
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
                vec![CompilableStruct::new(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap!["a".to_string() ; Type::Integer]
                )]
            )
        );
    }

    #[test]
    fn test_constructor_call_expression_3() {
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

        let main_function_stmt =
            StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                vec![StatementNode::expression_statement(
                ExpressionNode::constructor_call_expression(
                    Token::new(TokenType::IdentifierToken, "struct_A".to_string(), 0, 0, None),
                    None,
                    hashmap![
                        Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0,0, None),
                            Value::Integer(12)).wrapped(),
                        Token::new(TokenType::IdentifierToken, "b".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::FloatLiteralToken, "12.22".to_string(), 0,0, None),
                            Value::Float(12.22)).wrapped()
                    ]
                )
                .wrapped(),
            )
            .wrapped()],
            )
            .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        let basic_struct_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_A".to_string(), 0, 0, None),
            hashmap![
                "a".to_string() ; Type::Integer,
                "b".to_string() ; Type::Float
            ],
        )
        .wrapped();

        root_ast.push_statement(basic_struct_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let function_body = CompilableBlockNode::new(
            None,
            HashMap::new(),
            vec![CompilableStatementNode::expression_statement(
                CompilableExpressionNode::constructor_call_expression(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap![
                        "a".to_string() ; CompilableExpressionNode::literal_expression(Value::Integer(12)).wrapped(),
                        "b".to_string() ; CompilableExpressionNode::literal_expression(Value::Float(12.22)).wrapped()
                    ]
                )
                .wrapped(),
                Some(Type::new_struct("struct_A", "root")),
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
                vec![CompilableStruct::new(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap![
                        "a".to_string() ; Type::Integer,
                        "b".to_string() ; Type::Float
                    ]
                )]
            )
        );
    }

    #[test]
    fn test_constructor_call_expression_4() {
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

        let main_function_stmt =
            StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                vec![StatementNode::expression_statement(
                ExpressionNode::constructor_call_expression(
                    Token::new(TokenType::IdentifierToken, "struct_A".to_string(), 0, 0, None),
                    None,
                    hashmap![
                        Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0,0, None),
                            Value::Integer(12)).wrapped()
                    ]
                )
                .wrapped(),
            )
            .wrapped()],
            )
            .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        let basic_struct_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_A".to_string(), 0, 0, None),
            hashmap![
                "a".to_string() ; Type::Integer,
                "b".to_string() ; Type::Float
            ],
        )
        .wrapped();

        root_ast.push_statement(basic_struct_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        assert_eq!(
            resolver.run().unwrap_err(),
            ResolverError::struct_values_count_does_not_match_definition(
                2,
                1,
                Token::new(
                    TokenType::IdentifierToken,
                    "struct_A".to_string(),
                    0,
                    0,
                    None
                ),
            )
        );
    }

    #[test]
    fn test_constructor_call_expression_5() {
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

        let main_function_stmt =
            StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                vec![StatementNode::expression_statement(
                ExpressionNode::constructor_call_expression(
                    Token::new(TokenType::IdentifierToken, "struct_A".to_string(), 0, 0, None),
                    None,
                    hashmap![
                        Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0,0, None),
                            Value::Integer(12)).wrapped()
                    ]
                )
                .wrapped(),
            )
            .wrapped()],
            )
            .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        let basic_struct_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_A".to_string(), 0, 0, None),
            hashmap![
                "a".to_string() ; Type::Character
            ],
        )
        .wrapped();

        root_ast.push_statement(basic_struct_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        assert_eq!(
            resolver.run().unwrap_err(),
            ResolverError::invalid_constructor_call_argument(
                Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None),
                Type::Character,
                Some(Type::Integer),
            )
        );
    }

    #[test]
    fn test_constructor_call_expression_6() {
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

        let main_function_stmt =
            StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                vec![StatementNode::expression_statement(
                ExpressionNode::constructor_call_expression(
                    Token::new(TokenType::IdentifierToken, "struct_A".to_string(), 0, 0, None),
                    None,
                    hashmap![
                        Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None) ; 
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::IntegerLiteralToken, "12".to_string(), 0,0, None),
                            Value::Integer(12)).wrapped()
                    ]
                )
                .wrapped(),
            )
            .wrapped()],
            )
            .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        let basic_struct_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_A".to_string(), 0, 0, None),
            hashmap![
                "b".to_string() ; Type::Integer
            ],
        )
        .wrapped();

        root_ast.push_statement(basic_struct_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        assert_eq!(
            resolver.run().unwrap_err(),
            ResolverError::no_field_with_name_on_struct(
                Token::new(TokenType::IdentifierToken, "a".to_string(), 0, 0, None),
                "struct_A".to_string()
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_constructor_call_expression_1);
        define_intensive_test!(test_constructor_call_expression_2);
        define_intensive_test!(test_constructor_call_expression_3);
        define_intensive_test!(test_constructor_call_expression_4);
        define_intensive_test!(test_constructor_call_expression_5);
        define_intensive_test!(test_constructor_call_expression_6);
    }
}

mod test_array_index_assignment_expression {
    use super::*;

    #[test]
    fn test_array_index_assignment_expression_1() {
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
            vec![
                StatementNode::variable_declaration_statement(
                    Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
                    Token::new_identifier("my_string".to_string(), 0, 0, None),
                    ExpressionNode::array_allocation_expression(
                        Token::new(
                            TokenType::StringLiteralToken,
                            "testing 12345.....".to_string(),
                            0,
                            0,
                            None,
                        ),
                        ArrayLiteral::from("testing 12345....."),
                        ExpressionNode::literal_expression(
                            Token::new(
                                TokenType::IntegerLiteralToken,
                                "18".to_string(),
                                0,
                                0,
                                None,
                            ),
                            Value::Integer(18),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
                StatementNode::expression_statement(
                    ExpressionNode::assignment_expression(
                        ExpressionNode::array_index_expression(
                            Token::new(
                                TokenType::OpenSquareBraceToken,
                                "[".to_string(),
                                0,
                                0,
                                None,
                            ),
                            ExpressionNode::variable_expression(
                                Token::new(TokenType::DollarToken, "$".to_string(), 0, 0, None),
                                Variable::from(vec![Token::new_identifier(
                                    "my_string".to_string(),
                                    0,
                                    0,
                                    None,
                                )]),
                            )
                            .wrapped(),
                            ExpressionNode::literal_expression(
                                Token::new(
                                    TokenType::IntegerLiteralToken,
                                    "0".to_string(),
                                    0,
                                    0,
                                    None,
                                ),
                                Value::Integer(0),
                            )
                            .wrapped(),
                        )
                        .wrapped(),
                        Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new(
                                TokenType::CharacterLiteralToken,
                                "c".to_string(),
                                0,
                                0,
                                None,
                            ),
                            Value::Character('c'),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
            ],
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
            hashmap!["my_string".to_string() ; Type::Array(Box::new(Type::Character))],
            vec![
                CompilableStatementNode::variable_declaration_statement(
                    "my_string".to_string(),
                    CompilableExpressionNode::array_allocation_expression(
                        Type::Array(Box::new(Type::Character)),
                        "testing 12345....."
                            .chars()
                            .map(|c| Value::Character(c))
                            .collect(),
                        CompilableExpressionNode::literal_expression(Value::Integer(18)).wrapped(),
                    )
                    .wrapped(),
                    Type::Array(Box::new(Type::Character)),
                )
                .wrapped(),
                CompilableStatementNode::expression_statement(
                    CompilableExpressionNode::array_index_assignment_expression(
                        CompilableExpressionNode::array_index_expression(
                            CompilableExpressionNode::variable_expression(Variable::from(vec![
                                Token::new_identifier("my_string".to_string(), 0, 0, None),
                            ]))
                            .wrapped(),
                            CompilableExpressionNode::literal_expression(Value::Integer(0))
                                .wrapped(),
                            Type::Character,
                        )
                        .wrapped(),
                        CompilableExpressionNode::literal_expression(Value::Character('c'))
                            .wrapped(),
                    )
                    .wrapped(),
                    None,
                )
                .wrapped(),
            ],
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
    fn test_array_index_assignment_expression_2() {
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
            vec![
                StatementNode::variable_declaration_statement(
                    Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
                    Token::new_identifier("my_string".to_string(), 0, 0, None),
                    ExpressionNode::array_allocation_expression(
                        Token::new(
                            TokenType::StringLiteralToken,
                            "testing 12345.....".to_string(),
                            0,
                            0,
                            None,
                        ),
                        ArrayLiteral::from("testing 12345....."),
                        ExpressionNode::literal_expression(
                            Token::new(
                                TokenType::IntegerLiteralToken,
                                "18".to_string(),
                                0,
                                0,
                                None,
                            ),
                            Value::Integer(18),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
                StatementNode::expression_statement(
                    ExpressionNode::assignment_expression(
                        ExpressionNode::array_index_expression(
                            Token::new(
                                TokenType::OpenSquareBraceToken,
                                "[".to_string(),
                                0,
                                0,
                                None,
                            ),
                            ExpressionNode::variable_expression(
                                Token::new(TokenType::DollarToken, "$".to_string(), 0, 0, None),
                                Variable::from(vec![Token::new_identifier(
                                    "my_string".to_string(),
                                    0,
                                    0,
                                    None,
                                )]),
                            )
                            .wrapped(),
                            ExpressionNode::literal_expression(
                                Token::new(
                                    TokenType::IntegerLiteralToken,
                                    "0".to_string(),
                                    0,
                                    0,
                                    None,
                                ),
                                Value::Integer(0),
                            )
                            .wrapped(),
                        )
                        .wrapped(),
                        Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                        ExpressionNode::literal_expression(
                            Token::new(TokenType::TrueToken, "true".to_string(), 0, 0, None),
                            Value::Boolean(true),
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                )
                .wrapped(),
            ],
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
            ResolverError::invalid_assignment_type(
                Token::new(TokenType::LeftArrowToken, "<-".to_string(), 0, 0, None),
                Some(Type::Character),
                Some(Type::Boolean)
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_array_index_assignment_expression_1);
        define_intensive_test!(test_array_index_assignment_expression_2);
    }
}
