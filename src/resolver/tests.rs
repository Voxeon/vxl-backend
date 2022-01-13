use super::*;

use std::collections::VecDeque;

use crate::ast::{new_statement, StatementNode, AST};
use crate::pre_processor::{PreProcessor, PreProcessorCommand};
use crate::Token;
use crate::ROOT_MODULE_NAME;

//TODO: Determine where to enforece a main function requirement
//TODO: Add a check for circular dependencies (struct_a -> struct_b -> struct_a)

mod no_std_tests {
    use super::*;

    mod statement_tests {
        use super::*;

        #[test]
        fn test_basic_struct_definition() {
            let mut input = VecDeque::new();
            let mut root_ast = AST::new();

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let basic_struct_ast = new_statement(StatementNode::struct_statement(
                Token::new_identifier("struct".to_string(), 0, 0, None),
                Token::new_identifier("struct_A".to_string(), 0, 0, None),
                hashmap!["a".to_string() ; Type::Integer],
            ));

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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let struct_a_ast = new_statement(StatementNode::struct_statement(
                Token::new_identifier("struct".to_string(), 0, 0, None),
                Token::new_identifier("struct_A".to_string(), 0, 0, None),
                hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
            ));

            let struct_b_ast = new_statement(StatementNode::struct_statement(
                Token::new_identifier("struct".to_string(), 0, 0, None),
                Token::new_identifier("struct_B".to_string(), 0, 0, None),
                hashmap!["a".to_string() ; Type::Integer],
            ));

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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let struct_a_ast = new_statement(StatementNode::struct_statement(
                Token::new_identifier("struct".to_string(), 0, 0, None),
                Token::new_identifier("struct_A".to_string(), 0, 0, None),
                hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
            ));

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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let struct_a_ast = new_statement(StatementNode::struct_statement(
                Token::new_identifier("struct".to_string(), 0, 0, None),
                Token::new_identifier("struct_A".to_string(), 0, 0, None),
                hashmap!["s_b".to_string() ; Type::Struct { name: "struct_B".to_string(), module: "root".to_string() }],
            ));

            let struct_b_ast = new_statement(StatementNode::struct_statement(
                Token::new_identifier("struct".to_string(), 0, 0, None),
                Token::new_identifier("struct_B".to_string(), 0, 0, None),
                hashmap!["a".to_string() ; Type::Struct { name: "struct_A".to_string(), module: "root".to_string()}],
            ));

            root_ast.push_statement(struct_a_ast);
            root_ast.push_statement(struct_b_ast);

            input.push_front(root_ast);

            let processed_output = PreProcessor::new(input).process().unwrap();
            let resolver = Resolver::default()
                .with_modules(processed_output)
                .with_standard_library(false);

            let err = resolver.run().unwrap_err();

            assert_eq!(
                err,
                ResolverError::recursive_reference_detected(
                    "struct_A".to_string(),
                    "root".to_string(),
                    "struct_B".to_string(),
                    "root".to_string(),
                    "a".to_string()
                )
            )
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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let main_function_stmt = new_statement(StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                None,
                Vec::new(),
            ));

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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let main_function_stmt = new_statement(StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                vec![(
                    Token::new_identifier("a".to_string(), 0, 0, None),
                    Type::Integer,
                )],
                Some(Type::Integer),
                Vec::new(),
            ));

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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let main_function_stmt = new_statement(StatementNode::function_statement(
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
            ));

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

            root_ast.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    Token::new_identifier("begin".to_string(), 1, 1, None),
                    PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                        ROOT_MODULE_NAME.to_string(),
                        1,
                        1,
                        None,
                    )),
                ),
            ));

            let main_function_stmt = new_statement(StatementNode::function_statement(
                Token::new_identifier("func".to_string(), 2, 1, None),
                Token::new_identifier("main".to_string(), 0, 0, None),
                Vec::new(),
                Some(Type::Struct {
                    name: String::from("name"),
                    module: String::from("root"),
                }),
                Vec::new(),
            ));

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
