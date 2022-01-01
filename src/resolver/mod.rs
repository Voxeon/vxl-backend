use std::collections::{HashMap, HashSet};

use crate::ast::{Expression, Statement, StatementNode, Type};
use crate::error::ResolverError;
use crate::lexer::token::Token;
use crate::pre_processor::{ObjectName, ProcessedModule};
use crate::ROOT_MODULE_NAME;

type ResolverResult<T> = Result<T, ResolverError>;

/// Performs type checking and importing of the standard library.
pub struct Resolver {
    modules: HashMap<String, ProcessedModule>,
    processed_modules: HashMap<String, ProcessedModule>,
    type_checking: bool,
    name_checking: bool,
    definition_checking: bool,
    include_std_library: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub struct Resolution;

impl Resolver {
    pub fn new() -> Self {
        return Self {
            modules: HashMap::new(),
            processed_modules: HashMap::new(),
            type_checking: false,
            name_checking: false,
            definition_checking: false,
            include_std_library: false,
        };
    }

    with_wrapper!(with_modules, modules, HashMap<String, ProcessedModule>);
    with_wrapper!(with_type_checking, type_checking, bool);
    with_wrapper!(with_name_checking, name_checking, bool);
    with_wrapper!(with_definition_checking, definition_checking, bool);
    with_wrapper!(with_standard_library, include_std_library, bool);

    pub fn run(mut self) -> ResolverResult<Resolution> {
        if self.include_std_library {
            self.import_std_library_modules();
        }

        if self.name_checking || self.type_checking || self.definition_checking {
            return self.run_resolution();
        } else {
            return Ok(self.merge());
        }
    }

    fn run_resolution(mut self) -> ResolverResult<Resolution> {
        if self.modules.contains_key(ROOT_MODULE_NAME) {
            self.process_module(ROOT_MODULE_NAME.to_string())?;

            return Ok(self.merge());
        } else {
            return Err(ResolverError::NoRootModuleDefined);
        }
    }

    fn merge(mut self) -> Resolution {
        todo!();
    }

    fn process_module(&mut self, module_name: String) -> ResolverResult<()> {
        if let Some(module) = self.modules.remove(&module_name) {
            // Check high level imports
            let imports = self.import_check(&module)?;

            // Check every variable reference and function call types and names
            self.depth_resolution(&module, &imports)?;

            self.processed_modules
                .insert(module_name.to_string(), module);

            for import in imports {
                self.process_module(import)?;
            }
        } else {
            panic!();
        }

        todo!();
    }

    fn import_check(&mut self, module: &ProcessedModule) -> ResolverResult<HashSet<String>> {
        fn handle_module_found(
            missing_modules_errors: &mut Vec<ResolverError>,
            required_modules: &mut HashSet<String>,
            import: &ObjectName,
            referenced_module: &ProcessedModule,
        ) {
            if referenced_module
                .functions
                .contains_key(import.name.lexeme())
                && referenced_module.structs.contains_key(import.name.lexeme())
            {
                required_modules.insert(import.name.lexeme().clone());
            } else {
                missing_modules_errors.push(ResolverError::no_object_defined(
                    import.name.clone(),
                    import.module.clone(),
                ));
            }
        }

        let mut missing_modules_errors = Vec::new();
        let mut required_modules = HashSet::new();

        // Check the imports
        for import in &module.imports {
            if let Some(referenced_module) = self.modules.get(import.module.lexeme()) {
                handle_module_found(
                    &mut missing_modules_errors,
                    &mut required_modules,
                    import,
                    referenced_module,
                );
            } else if let Some(referenced_module) =
                self.processed_modules.get(import.module.lexeme())
            {
                handle_module_found(
                    &mut missing_modules_errors,
                    &mut required_modules,
                    import,
                    referenced_module,
                );
            } else {
                missing_modules_errors
                    .push(ResolverError::no_module_defined(import.module.clone()));
            }
        }

        return Ok(required_modules);
    }

    fn encoded_fn_name(&mut self, fn_name: &String, module_name: &String) -> String {
        return format!(
            "__fn_{}_{}_{}",
            module_name.len() + fn_name.len(),
            &module_name,
            fn_name
        );
    }

    fn encoded_struct_name(&mut self, struct_name: &String, module_name: &String) -> String {
        return format!(
            "__struct_{}_{}_{}",
            module_name.len() + struct_name.len(),
            &module_name,
            struct_name
        );
    }

    fn import_std_library_modules(&mut self) {
        todo!();
    }

    fn depth_resolution(
        &mut self,
        module: &ProcessedModule,
        imports: &HashSet<String>,
    ) -> ResolverResult<()> {
        // Check struct field types
        for (struct_name, struct_definition) in &module.structs {
            for (_name, field_tp) in &struct_definition.fields {
                self.is_valid_type(
                    field_tp,
                    None,
                    None,
                    None,
                    Some(struct_name),
                    module,
                    Some(imports),
                )?;
            }
        }

        for (f_name, function_statement) in &module.functions {
            if let StatementNode::FunctionStatement {
                keyword: _,
                name,
                arguments,
                return_type,
                body,
            } = &*function_statement.borrow()
            {
                self.check_function(name, arguments, return_type, body, module, Some(imports))?;
            } else {
                panic!("Internal Error: Unexpected statement in the \"{}\" module. Function name: \"{}\"", module.name, f_name);
            }
        }

        return Ok(());
    }

    fn is_valid_type(
        &self,
        tp: &Type,
        reference_token: Option<&Token>,
        current_function: Option<&Token>,
        custom_message: Option<&String>,
        current_struct: Option<&String>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<()> {
        if let Type::Array(nested) = tp {
            return self.is_valid_type(
                nested,
                reference_token,
                current_function,
                custom_message,
                current_struct,
                current_module,
                imports,
            );
        } else if let Type::Struct { name, module } = tp {
            let found_module = {
                if let Some(imports) = imports {
                    imports.contains(module) || module == &current_module.name
                } else {
                    module == &current_module.name
                }
            };

            if found_module {
                let m = {
                    if module == &current_module.name {
                        current_module
                    } else {
                        self.lookup_module(module).unwrap()
                    }
                };

                if m.structs.contains_key(name) {
                    return Ok(());
                } else {
                    if let Some(s) = current_struct {
                        return Err(
                            ResolverError::no_object_defined_with_name_in_module_in_struct(
                                name.clone(),
                                module.clone(),
                                s.clone(),
                            ),
                        );
                    } else if reference_token.is_some() || current_function.is_some() {
                        let ref_token;

                        if let Some(reference_token) = reference_token {
                            ref_token = reference_token;
                        } else {
                            ref_token = current_function.unwrap();
                        }

                        return Err(
                            ResolverError::no_object_defined_with_name_in_module_in_function(
                                name.clone(),
                                module.clone(),
                                ref_token.clone(),
                            ),
                        );
                    } else {
                        return Err(ResolverError::no_module_defined_with_name(module.clone()));
                    }
                }
            } else {
                return Err(ResolverError::module_not_imported(
                    module.clone(),
                    current_module.name.clone(),
                ));
            }
        } else {
            return Ok(());
        }
    }

    fn check_function(
        &self,
        function_name: &Token,
        function_arguments: &HashMap<String, Type>,
        return_type: &Option<Type>,
        body: &Vec<Statement>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<()> {
        if let Some(return_type) = return_type {
            self.is_valid_type(
                return_type,
                None,
                Some(function_name),
                None,
                None,
                current_module,
                imports,
            )?;
        }

        for (_name, tp) in function_arguments {
            self.is_valid_type(
                tp,
                None,
                Some(function_name),
                None,
                None,
                current_module,
                imports,
            )?;
        }

        return self.check_block(
            function_name,
            true,
            return_type,
            body,
            current_module,
            imports,
        );
    }

    fn check_block(
        &self,
        function_reference_token: &Token,
        return_allowed: bool,
        return_type: &Option<Type>,
        statements: &Vec<Statement>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<()> {
        for statement in statements {
            self.check_statement(
                function_reference_token,
                return_allowed,
                return_type,
                current_module,
                imports,
                statement,
            )?;
        }

        return Ok(());
    }

    fn check_statement(
        &self,
        function_reference_token: &Token,
        return_allowed: bool,
        return_type: &Option<Type>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
        statement: &Statement,
    ) -> ResolverResult<()> {
        return match &*statement.borrow() {
            StatementNode::ExpressionStatement { expr } => {
                self.check_expression(current_module, imports, expr)?;

                Ok(())
            }
            StatementNode::ReturnStatement { keyword, value } => {
                if !return_allowed {
                    return Err(ResolverError::no_return_statement_permitted(
                        keyword.clone(),
                    ));
                }

                if let Some(return_value) = value {
                    if let Some(return_type) = return_type {
                        let tp = self.check_expression(current_module, imports, return_value)?;

                        if !tp.is_same_type(return_type) {
                            return Err(ResolverError::return_type_does_not_match(
                                function_reference_token.clone(),
                                Some(return_type.clone()),
                                Some(tp),
                            ));
                        } else {
                            return Ok(());
                        }
                    }
                } else if value.is_none() && return_type.is_none() {
                    return Ok(());
                }

                return Err(ResolverError::return_type_does_not_match(
                    function_reference_token.clone(),
                    return_type.clone(),
                    None,
                ));
            }
            StatementNode::VariableDeclarationStatement {
                keyword,
                name,
                initializer,
            } => todo!(),
            StatementNode::BlockStatement { open_brace, body } => todo!(),
            StatementNode::WhileStatement {
                keyword,
                condition,
                body,
            } => todo!(),
            StatementNode::ForStatement {
                keyword,
                variable_name,
                start,
                stop,
                step,
                body,
            } => todo!(),
            StatementNode::IfStatement {
                keyword,
                condition,
                body,
                else_body,
            } => todo!(),
            StatementNode::FunctionStatement {
                keyword,
                name,
                arguments,
                return_type,
                body,
            } => todo!(),
            StatementNode::StructStatement {
                keyword,
                name,
                fields,
            } => todo!(),
            StatementNode::PreProcessorCommandStatement { symbol, command: _ } => {
                panic!(
                    "Unexpected pre-processor command statement in the resolver. Symbol: {:?}",
                    symbol
                );
            }
        };
    }

    fn check_expression(
        &self,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
        expression: &Expression,
    ) -> ResolverResult<Type> {
        todo!();
    }

    fn lookup_module(&self, name: &str) -> Option<&ProcessedModule> {
        let mut m = self.processed_modules.get(name);

        if m.is_none() {
            m = self.modules.get(name);
        }

        return m;
    }
}

impl Default for Resolver {
    fn default() -> Self {
        return Self {
            modules: HashMap::new(),
            processed_modules: HashMap::new(),
            type_checking: true,
            name_checking: true,
            definition_checking: true,
            include_std_library: true,
        };
    }
}

#[cfg(test)]
mod tests;