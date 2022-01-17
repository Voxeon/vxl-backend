use super::*;

mod struct_declaration {
    use super::*;

    #[test]
    fn test_basic_struct_definition() {
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

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let program = resolver.run().unwrap();

        assert_eq!(program.functions, Vec::new());
        assert_eq!(
            program.structs,
            vec![CompilableStruct::new(
                "__struct_c_root_struct_A".to_string(),
                hashmap!["a".to_string() ; Type::Integer]
            )]
        )
    }

    #[test]
    fn test_valid_interdependent_struct_definition() {
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

        let struct_a_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_A".to_string(), 0, 0, None),
                    hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
                ).wrapped();

        let struct_b_ast = StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 0, 0, None),
            Token::new_identifier("struct_B".to_string(), 0, 0, None),
            hashmap!["a".to_string() ; Type::Integer],
        )
        .wrapped();

        root_ast.push_statement(struct_a_ast);
        root_ast.push_statement(struct_b_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let program = resolver.run().unwrap();

        assert_eq!(program.functions, Vec::new());

        let mut structs = program.structs;
        structs.sort_by(|a, b| a.label.cmp(&b.label));

        assert_eq!(
            structs,
            vec![
                CompilableStruct::new(
                    "__struct_c_root_struct_A".to_string(),
                    hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }]
                ),
                CompilableStruct::new(
                    "__struct_c_root_struct_B".to_string(),
                    hashmap!["a".to_string() ; Type::Integer]
                )
            ]
        )
    }

    #[test]
    fn test_invalid_interdependent_struct_definition() {
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

        let struct_a_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_A".to_string(), 0, 0, None),
                    hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
                ).wrapped();

        root_ast.push_statement(struct_a_ast);
        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let err = resolver.run().unwrap_err();

        assert_eq!(
            err,
            ResolverError::no_object_defined_with_name_in_module_in_struct(
                "struct_B".to_string(),
                "root".to_string(),
                "struct_A".to_string()
            )
        );
    }

    #[test]
    fn test_circular_interdependent_struct_definition() {
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

        let struct_a_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_A".to_string(), 0, 0, None),
                    hashmap!["b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
                ).wrapped();

        let struct_b_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_B".to_string(), 0, 0, None),
                    hashmap!["a".to_string() ; Type::Struct { name: "struct_A".to_string(), module: "root".to_string()}],
                ).wrapped();

        root_ast.push_statement(struct_a_ast);
        root_ast.push_statement(struct_b_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let err = resolver.run().unwrap_err();

        assert_eq_one!(
            err,
            ResolverError::recursive_reference_detected(
                "struct_A".to_string(),
                "root".to_string(),
                "struct_B".to_string(),
                "root".to_string(),
                "a".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_B".to_string(),
                "root".to_string(),
                "struct_A".to_string(),
                "root".to_string(),
                "b".to_string()
            )
        );
    }

    #[test]
    fn test_circular_longer_1_interdependent_struct_definition() {
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

        let struct_a_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_A".to_string(), 0, 0, None),
                    hashmap!["b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
                ).wrapped();

        let struct_b_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_B".to_string(), 0, 0, None),
                    hashmap!["c".to_string() ; Type::Struct { name: "struct_C".to_string(), module: "root".to_string()}],
                ).wrapped();

        let struct_c_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_C".to_string(), 0, 0, None),
                    hashmap!["d".to_string() ; Type::Struct { name: "struct_D".to_string(), module: "root".to_string()}],
                ).wrapped();

        let struct_d_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_D".to_string(), 0, 0, None),
                    hashmap!["e".to_string() ; Type::Struct { name: "struct_E".to_string(), module: "root".to_string()}],
                ).wrapped();

        let struct_e_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_E".to_string(), 0, 0, None),
                    hashmap!["a".to_string() ; Type::Struct { name: "struct_A".to_string(), module: "root".to_string()}],
                ).wrapped();

        root_ast.push_statement(struct_a_ast);
        root_ast.push_statement(struct_b_ast);
        root_ast.push_statement(struct_c_ast);
        root_ast.push_statement(struct_d_ast);
        root_ast.push_statement(struct_e_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let err = resolver.run().unwrap_err();

        // Since hashmaps have an element of randomness any of the structs could be processed first.
        assert_eq_one!(
            err,
            ResolverError::recursive_reference_detected(
                "struct_A".to_string(),
                "root".to_string(),
                "struct_E".to_string(),
                "root".to_string(),
                "a".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_B".to_string(),
                "root".to_string(),
                "struct_A".to_string(),
                "root".to_string(),
                "b".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_C".to_string(),
                "root".to_string(),
                "struct_B".to_string(),
                "root".to_string(),
                "c".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_D".to_string(),
                "root".to_string(),
                "struct_C".to_string(),
                "root".to_string(),
                "d".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_E".to_string(),
                "root".to_string(),
                "struct_D".to_string(),
                "root".to_string(),
                "e".to_string()
            )
        );
    }

    #[test]
    fn test_circular_longer_2_interdependent_struct_definition() {
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

        let struct_a_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_A".to_string(), 0, 0, None),
                    hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
                ).wrapped();

        let struct_b_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_B".to_string(), 0, 0, None),
                    hashmap!["c".to_string() ; Type::Struct { name: "struct_C".to_string(), module: "root".to_string()}],
                ).wrapped();

        let struct_c_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_C".to_string(), 0, 0, None),
                    hashmap!["d".to_string() ; Type::Struct { name: "struct_D".to_string(), module: "root".to_string()}],
                ).wrapped();

        let struct_d_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_D".to_string(), 0, 0, None),
                    hashmap!["e".to_string() ; Type::Struct { name: "struct_E".to_string(), module: "root".to_string()}],
                ).wrapped();

        let struct_e_ast = StatementNode::struct_statement(
                    Token::new_identifier("struct".to_string(), 0, 0, None),
                    Token::new_identifier("struct_E".to_string(), 0, 0, None),
                    hashmap!["c".to_string() ; Type::Struct { name: "struct_C".to_string(), module: "root".to_string()}],
                ).wrapped();

        root_ast.push_statement(struct_a_ast);
        root_ast.push_statement(struct_b_ast);
        root_ast.push_statement(struct_c_ast);
        root_ast.push_statement(struct_d_ast);
        root_ast.push_statement(struct_e_ast);

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let err = resolver.run().unwrap_err();

        assert_eq_one!(
            err,
            ResolverError::recursive_reference_detected(
                "struct_C".to_string(),
                "root".to_string(),
                "struct_E".to_string(),
                "root".to_string(),
                "c".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_D".to_string(),
                "root".to_string(),
                "struct_C".to_string(),
                "root".to_string(),
                "d".to_string()
            ),
            ResolverError::recursive_reference_detected(
                "struct_E".to_string(),
                "root".to_string(),
                "struct_D".to_string(),
                "root".to_string(),
                "e".to_string()
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_basic_struct_definition);
        define_intensive_test!(test_valid_interdependent_struct_definition);
        define_intensive_test!(test_invalid_interdependent_struct_definition);
        define_intensive_test!(test_circular_interdependent_struct_definition);
        define_intensive_test!(test_circular_longer_1_interdependent_struct_definition);
        define_intensive_test!(test_circular_longer_2_interdependent_struct_definition);
    }
}

mod function_declaration {
    use super::*;

    #[test]
    fn test_basic_function_definition() {
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
    fn test_function_no_return_1() {
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
            ResolverError::function_does_not_always_return(
                Token::new_identifier("main".to_string(), 0, 0, None),
                Type::Integer
            )
        );
    }

    #[test]
    fn test_function_no_return_2() {
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

        let function_body = StatementNode::if_statement(
            Token::new_identifier("if".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                Value::Boolean(false),
            )
            .wrapped(),
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
            Some(Vec::new()),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            vec![(
                Token::new_identifier("a".to_string(), 0, 0, None),
                Type::Integer,
            )],
            Some(Type::Integer),
            vec![function_body],
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
            ResolverError::function_does_not_always_return(
                Token::new_identifier("main".to_string(), 0, 0, None),
                Type::Integer
            )
        );
    }

    #[test]
    fn test_function_no_return_3() {
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

        let function_body = StatementNode::if_statement(
            Token::new_identifier("if".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                Value::Boolean(false),
            )
            .wrapped(),
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
            Some(vec![StatementNode::if_statement(
                Token::new_identifier("if".to_string(), 0, 0, None),
                ExpressionNode::literal_expression(
                    Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                    Value::Boolean(false),
                )
                .wrapped(),
                vec![StatementNode::return_statement(
                    Token::new_identifier("return".to_string(), 0, 0, None),
                    Some(
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
                    ),
                )
                .wrapped()],
                Some(Vec::new()),
            )
            .wrapped()]),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            vec![(
                Token::new_identifier("a".to_string(), 0, 0, None),
                Type::Integer,
            )],
            Some(Type::Integer),
            vec![function_body],
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
            ResolverError::function_does_not_always_return(
                Token::new_identifier("main".to_string(), 0, 0, None),
                Type::Integer
            )
        );
    }

    #[test]
    fn test_function_no_return_4() {
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

        let function_body = StatementNode::if_statement(
            Token::new_identifier("if".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                Value::Boolean(false),
            )
            .wrapped(),
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
            Some(vec![StatementNode::if_statement(
                Token::new_identifier("if".to_string(), 0, 0, None),
                ExpressionNode::literal_expression(
                    Token::new(TokenType::FalseToken, "false".to_string(), 0, 0, None),
                    Value::Boolean(false),
                )
                .wrapped(),
                vec![StatementNode::return_statement(
                    Token::new_identifier("return".to_string(), 0, 0, None),
                    Some(
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
                    ),
                )
                .wrapped()],
                Some(vec![StatementNode::return_statement(
                    Token::new_identifier("return".to_string(), 0, 0, None),
                    Some(
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
                    ),
                )
                .wrapped()]),
            )
            .wrapped()]),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            vec![(
                Token::new_identifier("a".to_string(), 0, 0, None),
                Type::Integer,
            )],
            Some(Type::Integer),
            vec![function_body],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        // We aren't checking the output here, just whether an error occurs
        resolver.run().unwrap();
    }

    #[test]
    fn test_function_no_return_5() {
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

        let function_body = StatementNode::return_statement(
            Token::new_identifier("return".to_string(), 0, 0, None),
            None,
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            vec![(
                Token::new_identifier("a".to_string(), 0, 0, None),
                Type::Integer,
            )],
            None,
            vec![function_body],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        // We aren't checking the output here, just whether an error occurs
        resolver.run().unwrap();
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_basic_function_definition);
        define_intensive_test!(test_function_no_return_1);
        define_intensive_test!(test_function_no_return_2);
        define_intensive_test!(test_function_no_return_3);
        define_intensive_test!(test_function_no_return_4);
        define_intensive_test!(test_function_no_return_5);
    }
}

mod while_loop {
    use super::*;

    #[test]
    fn test_while_statement_1() {
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

        let while_statement = StatementNode::while_statement(
            Token::new_identifier("while".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::TrueToken, "true".to_string(), 0, 0, None),
                Value::Boolean(true),
            )
            .wrapped(),
            Vec::new(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![while_statement],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let function_body =
            CompilableBlockNode::new(None, HashMap::new(), Vec::new(), false).wrapped();

        let while_body = CompilableBlockNode::new(
            Some(function_body.clone()),
            HashMap::new(),
            Vec::new(),
            false,
        )
        .wrapped();

        function_body.borrow_mut().body = vec![CompilableStatementNode::while_statement(
            CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
            while_body,
        )
        .wrapped()];

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
    fn test_while_statement_2() {
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

        let while_statement = StatementNode::while_statement(
            Token::new_identifier("while".to_string(), 0, 0, None),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(
                    Token::new(TokenType::IntegerLiteralToken, "52".to_string(), 0, 0, None),
                    Value::Integer(52),
                )
                .wrapped(),
                Token::new(TokenType::LessThanEqualToken, "<=".to_string(), 0, 0, None),
                ExpressionNode::literal_expression(
                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                    Value::Integer(22),
                )
                .wrapped(),
            )
            .wrapped(),
            Vec::new(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![while_statement],
        )
        .wrapped();

        root_ast.push_statement(main_function_stmt.clone());

        input.push_front(root_ast);

        let processed_output = PreProcessor::new(input).process().unwrap();
        let resolver = Resolver::default()
            .with_modules(processed_output)
            .with_standard_library(false);

        let function_body =
            CompilableBlockNode::new(None, HashMap::new(), Vec::new(), false).wrapped();

        let while_body = CompilableBlockNode::new(
            Some(function_body.clone()),
            HashMap::new(),
            Vec::new(),
            false,
        )
        .wrapped();

        function_body.borrow_mut().body = vec![CompilableStatementNode::while_statement(
            CompilableExpressionNode::less_equal_binary_expression(
                CompilableExpressionNode::literal_expression(Value::Integer(52)).wrapped(),
                CompilableExpressionNode::literal_expression(Value::Integer(22)).wrapped(),
                Type::Integer,
            )
            .wrapped(),
            while_body,
        )
        .wrapped()];

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
    fn test_while_statement_3() {
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

        let while_statement = StatementNode::while_statement(
            Token::new_identifier("while".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::IntegerLiteralToken, "52".to_string(), 0, 0, None),
                Value::Integer(52),
            )
            .wrapped(),
            Vec::new(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![while_statement],
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
            ResolverError::invalid_while_loop_condition_type(
                Token::new_identifier("while".to_string(), 0, 0, None),
                Some(Type::Integer)
            )
        );
    }

    #[test]
    fn test_while_statement_4() {
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

        let while_statement = StatementNode::while_statement(
            Token::new_identifier("while".to_string(), 0, 0, None),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(
                    Token::new(TokenType::IntegerLiteralToken, "52".to_string(), 0, 0, None),
                    Value::Integer(52),
                )
                .wrapped(),
                Token::new(TokenType::PlusToken, "+".to_string(), 0, 0, None),
                ExpressionNode::literal_expression(
                    Token::new(TokenType::IntegerLiteralToken, "22".to_string(), 0, 0, None),
                    Value::Integer(22),
                )
                .wrapped(),
            )
            .wrapped(),
            Vec::new(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![while_statement],
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
            ResolverError::invalid_while_loop_condition_type(
                Token::new_identifier("while".to_string(), 0, 0, None),
                Some(Type::Integer)
            )
        );
    }

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_while_statement_1);
        define_intensive_test!(test_while_statement_2);
        define_intensive_test!(test_while_statement_3);
        define_intensive_test!(test_while_statement_4);
    }
}

mod for_loop {}

mod if_statement {}

mod variable_declaration {
    use super::*;

    #[test]
    fn test_variable_declaration_statement_1() {
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

        let variable_declaration_statement_1 = StatementNode::variable_declaration_statement(
            Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
            Token::new_identifier("test_var".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::TrueToken, "true".to_string(), 0, 0, None),
                Value::Boolean(true),
            )
            .wrapped(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![variable_declaration_statement_1],
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
            hashmap!["test_var".to_string() ; Type::Boolean],
            vec![CompilableStatementNode::variable_declaration_statement(
                "test_var".to_string(),
                CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                Type::Boolean,
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
    fn test_variable_declaration_statement_2() {
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

        let variable_declaration_statement_1 = StatementNode::variable_declaration_statement(
            Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
            Token::new_identifier("test_var".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::IntegerLiteralToken, "52".to_string(), 0, 0, None),
                Value::Integer(52),
            )
            .wrapped(),
        )
        .wrapped();

        let variable_declaration_statement_2 = StatementNode::variable_declaration_statement(
            Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
            Token::new_identifier("test_var".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::TrueToken, "true".to_string(), 0, 0, None),
                Value::Boolean(true),
            )
            .wrapped(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![
                variable_declaration_statement_1,
                variable_declaration_statement_2,
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
            hashmap!["test_var".to_string() ; Type::Boolean],
            vec![
                CompilableStatementNode::variable_declaration_statement(
                    "test_var".to_string(),
                    CompilableExpressionNode::literal_expression(Value::Integer(52)).wrapped(),
                    Type::Integer,
                )
                .wrapped(),
                CompilableStatementNode::variable_declaration_statement(
                    "test_var".to_string(),
                    CompilableExpressionNode::literal_expression(Value::Boolean(true)).wrapped(),
                    Type::Boolean,
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
    fn test_variable_declaration_statement_3() {
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

        let variable_declaration_statement_1 = StatementNode::variable_declaration_statement(
            Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
            Token::new_identifier("test_var".to_string(), 0, 0, None),
            ExpressionNode::literal_expression(
                Token::new(TokenType::IntegerLiteralToken, "52".to_string(), 0, 0, None),
                Value::Integer(52),
            )
            .wrapped(),
        )
        .wrapped();

        let variable_declaration_statement_2 = StatementNode::variable_declaration_statement(
            Token::new(TokenType::HashToken, "#".to_string(), 0, 0, None),
            Token::new_identifier("test_var".to_string(), 0, 0, None),
            ExpressionNode::variable_expression(
                Token::new(TokenType::DollarToken, "$".to_string(), 0, 0, None),
                vec![Token::new_identifier("test_var".to_string(), 0, 0, None)].into(),
            )
            .wrapped(),
        )
        .wrapped();

        let main_function_stmt = StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("main".to_string(), 0, 0, None),
            Vec::new(),
            None,
            vec![
                variable_declaration_statement_1,
                variable_declaration_statement_2,
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
            hashmap!["test_var".to_string() ; Type::Integer],
            vec![
                CompilableStatementNode::variable_declaration_statement(
                    "test_var".to_string(),
                    CompilableExpressionNode::literal_expression(Value::Integer(52)).wrapped(),
                    Type::Integer,
                )
                .wrapped(),
                CompilableStatementNode::variable_declaration_statement(
                    "test_var".to_string(),
                    CompilableExpressionNode::variable_expression(
                        vec![Token::new_identifier("test_var".to_string(), 0, 0, None)].into(),
                    )
                    .wrapped(),
                    Type::Integer,
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

    #[cfg(feature = "test-intensive")]
    mod intensive {
        use super::*;

        define_intensive_test!(test_variable_declaration_statement_1);
        define_intensive_test!(test_variable_declaration_statement_2);
        define_intensive_test!(test_variable_declaration_statement_3);
    }
}
