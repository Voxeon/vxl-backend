use super::PreProcessorCommand;
use crate::ast::{Statement, StatementNode, Type, AST};
use crate::error::PreProcessorError;
use crate::lexer::token::Token;
use crate::ROOT_MODULE_NAME;

use std::collections::{HashMap, VecDeque};

type PreProcessorResult<T> = Result<T, PreProcessorError>;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct ObjectName {
    pub(crate) module: Token,
    pub(crate) name: Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDefinition {
    pub(crate) name: String,
    pub(crate) fields: HashMap<String, Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompilableModule {
    pub(crate) name: String,
    pub(crate) imports: Vec<ObjectName>,
    pub(crate) functions: HashMap<String, Statement>,
    pub(crate) structs: HashMap<String, StructDefinition>,
}

#[derive(Clone, Debug)]
pub struct PreProcessor {
    trees: VecDeque<AST>,
    current_module_name: Option<Token>,
    modules: Vec<Token>,
    imports: HashMap<String, Vec<ObjectName>>,
    functions: HashMap<String, Vec<(String, Statement)>>,
    structs: HashMap<String, Vec<StructDefinition>>,
}

impl PreProcessor {
    pub fn new(trees: VecDeque<AST>) -> Self {
        if trees.is_empty() {
            panic!("Atleast one tree is required.");
        }

        return Self {
            trees,
            imports: HashMap::new(),
            current_module_name: None,
            modules: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
        };
    }

    pub fn process(mut self) -> PreProcessorResult<HashMap<String, CompilableModule>> {
        while let Some(mut tree) = self.trees.pop_front() {
            while let Some(stmt) = tree.pop_front() {
                if self.current_module_name.is_none() {
                    match &*stmt.borrow() {
                        StatementNode::PreProcessorCommandStatement {
                            symbol: _,
                            command: PreProcessorCommand::BeginModuleCommand(module_name),
                        } => {
                            // if let PreProcessorCommand::BeginModuleCommand(module_name) = cmd {

                            // }
                            if module_name.lexeme() != ROOT_MODULE_NAME {
                                return Err(PreProcessorError::NoRootModuleDefined);
                            } else {
                                self.current_module_name = Some(module_name.clone());
                            }
                        }
                        _ => return Err(PreProcessorError::NoRootModuleDefined),
                    }
                } else {
                    self.process_root_statement(stmt)?;
                }
            }
        }

        if let Some(n) = self.current_module_name {
            self.modules.push(n);
        }

        let mut output = HashMap::new();

        for module in self.modules {
            let mut structs = HashMap::new();
            let mut functions = HashMap::new();

            for s in self.structs.remove(module.lexeme()).unwrap_or(Vec::new()) {
                structs.insert(s.name.clone(), s);
            }

            for (name, func) in self.functions.remove(module.lexeme()).unwrap_or(Vec::new()) {
                functions.insert(name, func);
            }

            output.insert(
                module.lexeme().clone(),
                CompilableModule {
                    name: module.lexeme().clone(),
                    imports: self.imports.remove(module.lexeme()).unwrap_or(Vec::new()),
                    functions,
                    structs,
                },
            );
        }

        return Ok(output);
    }

    pub fn get_current_module_name(&self, reference_token: &Token) -> PreProcessorResult<&Token> {
        return self.current_module_name.as_ref().ok_or(
            PreProcessorError::no_current_module_defined(reference_token.clone()),
        );
    }

    fn process_root_statement(&mut self, stmt: Statement) -> PreProcessorResult<()> {
        if stmt.borrow().is_function_statement() {
            return self.define_function(stmt);
        } else if stmt.borrow().is_pre_processor_command_statement() {
            return self.process_preprocessor_command(stmt.borrow().borrow_preprocessor_command());
        } else if stmt.borrow().is_struct_statement() {
            return self.define_struct(stmt);
        } else {
            panic!("Unexpected top level statement.");
        }
    }

    fn process_preprocessor_command(
        &mut self,
        command: &PreProcessorCommand,
    ) -> PreProcessorResult<()> {
        match command {
            PreProcessorCommand::BeginModuleCommand(module_name) => {
                if self.modules.contains(&module_name) {
                    return Err(PreProcessorError::module_already_defined(
                        module_name.clone(),
                        self.current_module_name.as_ref().unwrap().clone(),
                    ));
                } else {
                    self.modules.push(self.current_module_name.take().unwrap());
                    self.current_module_name = Some(module_name.clone());
                }
            }
            PreProcessorCommand::ImportCommand(module_name, items) => {
                if let Some(imports) = self
                    .imports
                    .get_mut(self.current_module_name.as_ref().unwrap().lexeme())
                {
                    for item in items {
                        imports.push(ObjectName {
                            module: module_name.clone(),
                            name: item.clone(),
                        });
                    }
                } else {
                    let mut imports = Vec::new();

                    for item in items {
                        imports.push(ObjectName {
                            module: module_name.clone(),
                            name: item.clone(),
                        });
                    }

                    self.imports.insert(
                        self.current_module_name.as_ref().unwrap().lexeme().clone(),
                        imports,
                    );
                }
            }
        }

        return Ok(());
    }

    fn define_function(&mut self, stmt: Statement) -> PreProcessorResult<()> {
        let module = if let StatementNode::FunctionStatement {
            keyword,
            name: _,
            arguments: _,
            return_type: _,
            body: _,
        } = &*stmt.borrow()
        {
            self.get_current_module_name(keyword)?.lexeme().clone()
        } else {
            panic!("Expected function definition statement.");
        };

        if let Some(funcs) = self.functions.get_mut(&module) {
            let name = stmt.borrow().borrow_function_name().lexeme().clone();
            funcs.push((name, stmt));
        } else {
            let name = stmt.borrow().borrow_function_name().lexeme().clone();

            self.functions.insert(module, vec![(name, stmt)]);
        }

        return Ok(());
    }

    fn define_struct(&mut self, stmt: Statement) -> PreProcessorResult<()> {
        if let StatementNode::StructStatement {
            keyword,
            name,
            fields,
        } = &*stmt.borrow()
        {
            let module = self.get_current_module_name(keyword)?.lexeme().clone();

            let name_string = name.lexeme().clone();

            if let Some(structs) = self.structs.get_mut(&module) {
                structs.push(StructDefinition {
                    name: name_string,
                    fields: fields.clone(),
                });
            } else {
                self.structs.insert(
                    module,
                    vec![StructDefinition {
                        name: name_string,
                        fields: fields.clone(),
                    }],
                );
            }
        } else {
            panic!("Expected struct definition statement.");
        }

        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{new_statement, StatementNode};
    use crate::lexer::token::Token;

    macro_rules! hashmap {
        [$($k:expr ; $v:expr),*] => {
            vec![$(($k, $v)),*].into_iter().collect()
        };
    }

    #[test]
    fn test_single_function() {
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
            HashMap::new(),
            None,
            Vec::new(),
        ));

        root_ast.push_statement(main_function_stmt.clone());

        input.push_front(root_ast);

        let processor = PreProcessor::new(input);

        assert_eq!(
            processor.process().unwrap(),
            hashmap![ROOT_MODULE_NAME.to_string() ; CompilableModule {
                name: ROOT_MODULE_NAME.to_string(),
                imports: Vec::new(),
                functions: hashmap!["main".to_string() ; main_function_stmt],
                structs: HashMap::new(),
            }]
        );
    }

    #[test]
    fn test_single_struct() {
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

        let my_struct_stmt = new_statement(StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 2, 1, None),
            Token::new_identifier("my_struct".to_string(), 0, 0, None),
            HashMap::new(),
        ));

        root_ast.push_statement(my_struct_stmt.clone());

        input.push_front(root_ast);

        let processor = PreProcessor::new(input);

        assert_eq!(
            processor.process().unwrap(),
            hashmap![ROOT_MODULE_NAME.to_string() ; CompilableModule {
                name: ROOT_MODULE_NAME.to_string(),
                imports: Vec::new(),
                functions: HashMap::new(),
                structs: hashmap!["my_struct".to_string() ; StructDefinition {
                    name: "my_struct".to_string(),
                    fields: HashMap::new(),
                }],
            }]
        );
    }

    #[test]
    fn test_single_import() {
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

        let module = Token::new_identifier("secondary".to_string(), 1, 1, None);
        let import_a = Token::new_identifier("a".to_string(), 1, 1, None);

        root_ast.push_statement(new_statement(
            StatementNode::pre_processor_command_statement(
                Token::new_identifier("import".to_string(), 1, 1, None),
                PreProcessorCommand::ImportCommand(module.clone(), vec![import_a.clone()]),
            ),
        ));

        input.push_front(root_ast);

        let processor = PreProcessor::new(input);

        assert_eq!(
            processor.process().unwrap(),
            hashmap![ROOT_MODULE_NAME.to_string() ; CompilableModule {
                name: ROOT_MODULE_NAME.to_string(),
                imports: vec![ObjectName {
                    module: module,
                    name: import_a,
                }],
                functions: HashMap::new(),
                structs: HashMap::new(),
            }]
        );
    }

    #[test]
    fn test_single_function_single_struct() {
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
            HashMap::new(),
            None,
            Vec::new(),
        ));

        root_ast.push_statement(main_function_stmt.clone());

        let my_struct_stmt = new_statement(StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 2, 1, None),
            Token::new_identifier("my_struct".to_string(), 0, 0, None),
            HashMap::new(),
        ));

        root_ast.push_statement(my_struct_stmt.clone());

        input.push_front(root_ast);

        let processor = PreProcessor::new(input);

        assert_eq!(
            processor.process().unwrap(),
            hashmap![ROOT_MODULE_NAME.to_string() ; CompilableModule {
                name: ROOT_MODULE_NAME.to_string(),
                imports: Vec::new(),
                functions: hashmap!["main".to_string() ; main_function_stmt],
                structs: hashmap!["my_struct".to_string() ; StructDefinition {
                    name: "my_struct".to_string(),
                    fields: HashMap::new(),
                }],
            }]
        );
    }

    #[test]
    fn test_single_function_single_struct_multi_module() {
        let mut input = VecDeque::new();
        let mut root_ast = AST::new();
        let mut second_ast = AST::new();

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
            HashMap::new(),
            None,
            Vec::new(),
        ));

        root_ast.push_statement(main_function_stmt.clone());

        let my_struct_stmt = new_statement(StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 2, 1, None),
            Token::new_identifier("my_struct".to_string(), 0, 0, None),
            HashMap::new(),
        ));

        root_ast.push_statement(my_struct_stmt.clone());

        second_ast.push_statement(new_statement(
            StatementNode::pre_processor_command_statement(
                Token::new_identifier("begin".to_string(), 1, 1, None),
                PreProcessorCommand::BeginModuleCommand(Token::new_identifier(
                    "second".to_string(),
                    1,
                    1,
                    None,
                )),
            ),
        ));

        let second_function_stmt = new_statement(StatementNode::function_statement(
            Token::new_identifier("func".to_string(), 2, 1, None),
            Token::new_identifier("second".to_string(), 0, 0, None),
            HashMap::new(),
            None,
            Vec::new(),
        ));

        second_ast.push_statement(second_function_stmt.clone());

        let second_struct_stmt = new_statement(StatementNode::struct_statement(
            Token::new_identifier("struct".to_string(), 2, 1, None),
            Token::new_identifier("my_second_struct".to_string(), 0, 0, None),
            HashMap::new(),
        ));

        second_ast.push_statement(second_struct_stmt.clone());

        input.push_front(root_ast);
        input.push_back(second_ast);

        let processor = PreProcessor::new(input);

        assert_eq!(
            processor.process().unwrap(),
            hashmap![ROOT_MODULE_NAME.to_string() ; CompilableModule {
                name: ROOT_MODULE_NAME.to_string(),
                imports: Vec::new(),
                functions: hashmap!["main".to_string() ; main_function_stmt],
                structs: hashmap!["my_struct".to_string() ; StructDefinition {
                    name: "my_struct".to_string(),
                    fields: HashMap::new(),
                }],
            }, "second".to_string() ; CompilableModule {
                name: "second".to_string(),
                imports: Vec::new(),
                functions: hashmap!["second".to_string() ; second_function_stmt],
                structs: hashmap!["my_second_struct".to_string() ; StructDefinition {
                    name: "my_second_struct".to_string(),
                    fields: HashMap::new(),
                }],
            }]
        );
    }
}
