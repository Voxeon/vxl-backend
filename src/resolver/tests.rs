use super::*;

use std::collections::VecDeque;

use crate::ast::{StatementNode, Value, AST};
use crate::pre_processor::{PreProcessor, PreProcessorCommand};
use crate::Token;
use crate::ROOT_MODULE_NAME;

//TODO: Determine where to enforce a main function requirement

#[cfg(feature = "test-intensive")]
const INTENSIVE_TEST_CYCLES: usize = 100_000;

mod no_std_tests {
    use super::*;

    mod statement_tests {
        use super::*;

        mod struct_tests {
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

                #[test]
                fn intensive_test_circular_longer_2_interdependent_struct_definition() {
                    for i in 0..INTENSIVE_TEST_CYCLES {
                        println!(
                    "intensive_test_circular_longer_2_interdependent_struct_definition cycle {}",
                    i
                );

                        test_circular_longer_2_interdependent_struct_definition();
                    }
                }

                #[test]
                fn intensive_test_circular_longer_1_interdependent_struct_definition() {
                    for i in 0..INTENSIVE_TEST_CYCLES {
                        println!(
                    "intensive_test_circular_longer_1_interdependent_struct_definition cycle {}",
                    i
                );

                        test_circular_longer_1_interdependent_struct_definition();
                    }
                }

                #[test]
                fn intensive_test_circular_interdependent_struct_definition() {
                    for i in 0..INTENSIVE_TEST_CYCLES {
                        println!(
                            "intensive_test_circular_interdependent_struct_definition cycle {}",
                            i
                        );

                        test_circular_interdependent_struct_definition();
                    }
                }
            }
        }

        mod function_tests {
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
        }
    }

    mod expression_tests {
        use super::*;
    }

    mod larger_program_tests {
        use super::*;
    }

    mod logic_tests {
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
    }
}
