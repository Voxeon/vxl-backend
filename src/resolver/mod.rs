use std::collections::{HashMap, HashSet};

use crate::ast::compilable::{
    CompilableBlock, CompilableBlockNode, CompilableExpression, CompilableExpressionNode,
    CompilableFunction, CompilableStatement, CompilableStatementNode, CompilableStruct,
};
use crate::ast::{Expression, ExpressionNode, Statement, StatementNode, Type};
use crate::error::ResolverError;
use crate::lexer::token::{Token, TokenType};
use crate::pre_processor::{ObjectName, ProcessedModule, StructDefinition};
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

    fn import_std_library_modules(&mut self) {
        todo!();
    }

    fn depth_resolution(
        &mut self,
        module: &ProcessedModule,
        imports: &HashSet<String>,
    ) -> ResolverResult<()> {
        let mut structs = Vec::new();
        let mut functions = Vec::new();

        // Check struct field types
        for (struct_name, struct_definition) in &module.structs {
            structs.push(self.check_struct(module, imports, struct_name, struct_definition)?)
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
                functions.push(self.check_function(
                    name,
                    arguments,
                    return_type,
                    body,
                    module,
                    Some(imports),
                )?);
            } else {
                panic!("Internal Error: Unexpected statement in the \"{}\" module. Function name: \"{}\"", module.name, f_name);
            }
        }

        return Ok(());
    }

    fn check_struct(
        &self,
        current_module: &ProcessedModule,
        imports: &HashSet<String>,
        struct_name: &String,
        struct_def: &StructDefinition,
    ) -> ResolverResult<CompilableStruct> {
        for (_name, field_tp) in &struct_def.fields {
            self.check_type(
                field_tp,
                None,
                None,
                Some(struct_name),
                current_module,
                Some(imports),
            )?;
        }

        return Ok(CompilableStruct::new(
            Self::encode_struct_name(struct_name, &current_module.name),
            struct_def.fields.clone(),
        ));
    }

    fn check_function(
        &self,
        function_name: &Token,
        function_arguments: &HashMap<String, Type>,
        return_type: &Option<Type>,
        body: &Vec<Statement>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<CompilableFunction> {
        if let Some(return_type) = return_type {
            self.check_type(
                return_type,
                None,
                Some(function_name),
                None,
                current_module,
                imports,
            )?;
        }

        for (_name, tp) in function_arguments {
            self.check_type(tp, None, Some(function_name), None, current_module, imports)?;
        }

        return Ok(CompilableFunction::new(
            function_name.lexeme().clone(),
            return_type.clone(),
            self.check_block(
                function_name,
                true,
                return_type,
                body,
                current_module,
                None,
                imports,
            )?,
        ));
    }

    fn check_block(
        &self,
        function_reference_token: &Token,
        return_allowed: bool,
        return_type: &Option<Type>,
        statements: &Vec<Statement>,
        current_module: &ProcessedModule,
        parent_block: Option<&mut CompilableBlock>,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<CompilableBlock> {
        let mut block =
            CompilableBlockNode::new(parent_block.map(|b| b.clone()), HashMap::new(), Vec::new())
                .wrapped();
        let mut body = Vec::new();

        for statement in statements {
            let stmt = self.check_statement(
                function_reference_token,
                return_allowed,
                return_type,
                current_module,
                imports,
                &mut block,
                statement,
            )?;

            body.push(stmt);
        }

        // Only borrow the pointer once
        block.borrow_mut().body = body;

        return Ok(block);
    }

    fn check_statement(
        &self,
        function_reference_token: &Token,
        return_allowed: bool,
        return_type: &Option<Type>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
        current_block: &mut CompilableBlock,
        statement: &Statement,
    ) -> ResolverResult<CompilableStatement> {
        return match &*statement.borrow() {
            StatementNode::ExpressionStatement { expr } => {
                return self
                    .check_expression(current_module, function_reference_token, imports, expr)
                    .map(|(expr, expression_type)| {
                        CompilableStatementNode::expression_statement(expr, expression_type)
                            .wrapped()
                    });
            }
            StatementNode::ReturnStatement { keyword, value } => {
                if !return_allowed {
                    return Err(ResolverError::no_return_statement_permitted(
                        keyword.clone(),
                    ));
                }

                if let Some(return_value) = value {
                    if let Some(return_type) = return_type {
                        let (expr, expression_type) = self.check_expression(
                            current_module,
                            function_reference_token,
                            imports,
                            return_value,
                        )?;

                        if expression_type.is_none()
                            && !expression_type.as_ref().unwrap().is_same_type(return_type)
                        {
                            return Err(ResolverError::return_type_does_not_match(
                                function_reference_token.clone(),
                                Some(return_type.clone()),
                                expression_type,
                            ));
                        } else {
                            return Ok(CompilableStatementNode::return_statement(
                                Some(expr),
                                expression_type,
                            )
                            .wrapped());
                        }
                    }
                } else if value.is_none() && return_type.is_none() {
                    return Ok(CompilableStatementNode::return_statement(None, None).wrapped());
                }

                return Err(ResolverError::return_type_does_not_match(
                    function_reference_token.clone(),
                    return_type.clone(),
                    None,
                ));
            }
            StatementNode::VariableDeclarationStatement {
                keyword: _,
                name,
                initializer,
            } => {
                let (compiled_initializer, expr_tp) = self.check_expression(
                    current_module,
                    function_reference_token,
                    imports,
                    initializer,
                )?;

                if let Some(tp) = expr_tp {
                    let name = name.lexeme().clone();
                    current_block
                        .borrow_mut()
                        .variable_list
                        .insert(name.clone(), tp.clone());

                    Ok(CompilableStatementNode::variable_declaration_statement(
                        name,
                        compiled_initializer,
                        tp,
                    )
                    .wrapped())
                } else {
                    return Err(ResolverError::variable_declaration_requires_value(
                        name.clone(),
                    ));
                }
            }
            StatementNode::BlockStatement {
                open_brace: _,
                body,
            } => {
                let block = self.check_block(
                    function_reference_token,
                    return_allowed,
                    return_type,
                    body,
                    current_module,
                    Some(current_block),
                    imports,
                )?;

                Ok(CompilableStatementNode::block(block).wrapped())
            }
            StatementNode::WhileStatement {
                keyword,
                condition,
                body,
            } => {
                let (cond, cond_tp) = self.check_expression(
                    current_module,
                    function_reference_token,
                    imports,
                    condition,
                )?;

                if cond_tp.is_none() || !cond_tp.as_ref().unwrap().is_boolean() {
                    return Err(ResolverError::invalid_while_loop_condition_type(
                        keyword.clone(),
                        cond_tp,
                    ));
                }

                let body = self.check_block(
                    function_reference_token,
                    return_allowed,
                    return_type,
                    body,
                    current_module,
                    Some(current_block),
                    imports,
                )?;

                Ok(CompilableStatementNode::while_statement(cond, body).wrapped())
            }
            StatementNode::ForStatement {
                keyword,
                variable_name,
                start,
                stop,
                step,
                body,
            } => {
                let (start_expr, start_tp) = self.check_expression(
                    current_module,
                    function_reference_token,
                    imports,
                    start,
                )?;

                if start_tp.is_none() || !start_tp.as_ref().unwrap().is_integer() {
                    return Err(ResolverError::invalid_start_for_loop_type(
                        keyword.clone(),
                        start_tp,
                    ));
                }

                let (stop_expr, stop_tp) =
                    self.check_expression(current_module, function_reference_token, imports, stop)?;

                if stop_tp.is_none() || !stop_tp.as_ref().unwrap().is_integer() {
                    return Err(ResolverError::invalid_stop_for_loop_type(
                        keyword.clone(),
                        stop_tp,
                    ));
                }

                let (step_expr, step_tp) =
                    self.check_expression(current_module, function_reference_token, imports, step)?;

                if step_tp.is_none() || !step_tp.as_ref().unwrap().is_integer() {
                    return Err(ResolverError::invalid_step_for_loop_type(
                        keyword.clone(),
                        step_tp,
                    ));
                }

                let mut variable_block =
                    CompilableBlockNode::new_child(current_block.clone()).wrapped();

                variable_block
                    .borrow_mut()
                    .variable_list
                    .insert(variable_name.lexeme().clone(), start_tp.unwrap());

                let body = self.check_block(
                    function_reference_token,
                    return_allowed,
                    return_type,
                    body,
                    current_module,
                    Some(&mut variable_block),
                    imports,
                )?;

                Ok(CompilableStatementNode::for_statement(
                    variable_name.lexeme().clone(),
                    start_expr,
                    stop_expr,
                    step_expr,
                    body,
                )
                .wrapped())
            }
            StatementNode::IfStatement {
                keyword,
                condition,
                body,
                else_body,
            } => {
                let (cond, cond_tp) = self.check_expression(
                    current_module,
                    function_reference_token,
                    imports,
                    condition,
                )?;

                if cond_tp.is_none() || !cond_tp.as_ref().unwrap().is_boolean() {
                    return Err(ResolverError::invalid_if_condition_type(
                        keyword.clone(),
                        cond_tp,
                    ));
                }

                let body = self.check_block(
                    function_reference_token,
                    return_allowed,
                    return_type,
                    body,
                    current_module,
                    Some(current_block),
                    imports,
                )?;

                let else_body = if let Some(else_body) = else_body {
                    Some(self.check_block(
                        function_reference_token,
                        return_allowed,
                        return_type,
                        else_body,
                        current_module,
                        Some(current_block),
                        imports,
                    )?)
                } else {
                    None
                };

                Ok(CompilableStatementNode::if_statement(cond, body, else_body).wrapped())
            }
            StatementNode::FunctionStatement {
                keyword: _,
                name,
                arguments: _,
                return_type: _,
                body: _,
            } => {
                return Err(ResolverError::function_definition_forbidden_here(
                    name.clone(),
                ))
            }
            StatementNode::StructStatement {
                keyword: _,
                name,
                fields: _,
            } => {
                return Err(ResolverError::struct_definition_forbidden_here(
                    name.clone(),
                ))
            }
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
        function_reference_token: &Token,
        imports: Option<&HashSet<String>>,
        expression: &Expression,
    ) -> ResolverResult<(CompilableExpression, Option<Type>)> {
        return Ok(match &*expression.borrow() {
            ExpressionNode::ArrayAllocationExpression {
                reference_token,
                array_type,
                count,
            } => {
                self.check_type(
                    array_type.get_type_ref(),
                    Some(reference_token),
                    Some(function_reference_token),
                    None,
                    current_module,
                    imports,
                )?;

                let (count_expr, count_tp) = self.check_expression(
                    current_module,
                    function_reference_token,
                    imports,
                    count,
                )?;

                let tp = Type::Array(Box::new(array_type.get_type()));

                if count_tp.is_none() || !count_tp.as_ref().unwrap().is_integer() {
                    return Err(ResolverError::invalid_array_literal_count_type(
                        reference_token.clone(),
                        count_tp,
                    ));
                }

                let expr = CompilableExpressionNode::array_allocation_expression(
                    tp.clone(),
                    array_type.contents().clone(),
                    count_expr,
                )
                .wrapped();

                (expr, Some(tp))
            }
            ExpressionNode::LiteralExpression { value } => (
                CompilableExpressionNode::literal_expression(value.clone()).wrapped(),
                Some(value.get_type()),
            ),
            ExpressionNode::UnaryExpression { operator, rhs } => todo!(),
            ExpressionNode::BinaryExpression { lhs, operator, rhs } => todo!(),
            ExpressionNode::LogicalExpression { lhs, operator, rhs } => todo!(),
            ExpressionNode::VariableExpression { keyword, variable } => todo!(),
            ExpressionNode::AssignmentExpression { lhs, operator, rhs } => todo!(),
            ExpressionNode::GroupingExpression {
                reference_token: _,
                expression,
            } => {
                let (expr, tp) = self.check_expression(
                    current_module,
                    function_reference_token,
                    imports,
                    expression,
                )?;

                (
                    CompilableExpressionNode::grouping_expression(expr, tp.clone()).wrapped(),
                    tp,
                )
            }
            ExpressionNode::CallExpression {
                keyword,
                module,
                name,
                arguments,
            } => todo!(),
            ExpressionNode::ConstructorCallExpression {
                target_struct,
                module,
                arguments,
            } => todo!(),
            ExpressionNode::ArrayIndexExpression {
                open_brace_token,
                array_expression,
                index_expression,
            } => todo!(),
        });
    }

    fn lookup_module(&self, name: &str) -> Option<&ProcessedModule> {
        let mut m = self.processed_modules.get(name);

        if m.is_none() {
            m = self.modules.get(name);
        }

        return m;
    }

    fn check_type(
        &self,
        tp: &Type,
        reference_token: Option<&Token>,
        current_function: Option<&Token>,
        current_struct: Option<&String>,
        current_module: &ProcessedModule,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<()> {
        if let Type::Array(nested) = tp {
            return self.check_type(
                nested,
                reference_token,
                current_function,
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

    fn encode_fn_name(fn_name: &String, module_name: &String) -> String {
        return format!(
            "__fn_{:x}_{}_{}",
            module_name.len() + fn_name.len(),
            &module_name,
            fn_name
        );
    }

    fn encode_struct_name(struct_name: &String, module_name: &String) -> String {
        return format!(
            "__struct_{:x}_{}_{}",
            module_name.len() + struct_name.len(),
            &module_name,
            struct_name
        );
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
