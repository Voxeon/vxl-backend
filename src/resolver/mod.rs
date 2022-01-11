use std::collections::{HashMap, HashSet};

use crate::ast::compilable::{
    CompilableBlock, CompilableBlockNode, CompilableExpression, CompilableExpressionNode,
    CompilableFunction, CompilableProgram, CompilableStatement, CompilableStatementNode,
    CompilableStruct,
};
use crate::ast::{Expression, ExpressionNode, Statement, StatementNode, Type, Variable};
use crate::error::ResolverError;
use crate::pre_processor::{ObjectName, ProcessedModule, StructDefinition};
use crate::ROOT_MODULE_NAME;
use crate::{Token, TokenType};

type ResolverResult<T> = Result<T, ResolverError>;

/// Performs type checking and importing of the standard library.
pub struct Resolver {
    modules: HashMap<String, ProcessedModule>,
    functions: Vec<CompilableFunction>,
    structs: Vec<CompilableStruct>,
    current_module: String,
    include_std_library: bool,
}

impl Resolver {
    pub fn new() -> Self {
        return Self {
            modules: HashMap::new(),
            include_std_library: false,
            functions: Vec::new(),
            structs: Vec::new(),
            current_module: String::new(),
        };
    }

    with_wrapper!(with_modules, modules, HashMap<String, ProcessedModule>);
    with_wrapper!(with_standard_library, include_std_library, bool);

    pub fn run(mut self) -> ResolverResult<CompilableProgram> {
        if self.include_std_library {
            self.import_std_library_modules();
        }

        return self.run_resolution();
    }

    fn run_resolution(mut self) -> ResolverResult<CompilableProgram> {
        if self.modules.contains_key(ROOT_MODULE_NAME) {
            self.process_module(ROOT_MODULE_NAME.to_string())?;

            return Ok(self.merge());
        } else {
            return Err(ResolverError::NoRootModuleDefined);
        }
    }

    fn merge(self) -> CompilableProgram {
        return CompilableProgram::new(self.functions, self.structs);
    }

    fn process_module(&mut self, module_name: String) -> ResolverResult<()> {
        if !self.modules.contains_key(&module_name) {
            panic!();
        }

        self.current_module = module_name;

        // Check high level imports
        let imports = self.import_check()?;

        // Check every variable reference and function call types and names
        self.depth_resolution(&imports)?;

        for import in imports {
            self.process_module(import)?;
        }

        return Ok(());
    }

    fn import_check(&mut self) -> ResolverResult<HashSet<String>> {
        let module = self.modules.get(&self.current_module).unwrap();

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

    fn depth_resolution(&mut self, imports: &HashSet<String>) -> ResolverResult<()> {
        let module = self.modules.get(&self.current_module).unwrap();

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
                internal_error!(format!(
                    "Unexpected statement in the \"{}\" module. Function name: \"{}\"",
                    module.name, f_name
                ));
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
        function_arguments: &Vec<(Token, Type)>,
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
            Self::encode_fn_name(function_name.lexeme(), &current_module.name),
            function_arguments
                .into_iter()
                .map(|(name, tp)| (name.lexeme().clone(), tp.clone()))
                .collect(),
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
                    .check_expression(
                        current_module,
                        current_block,
                        function_reference_token,
                        imports,
                        expr,
                    )
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
                            current_block,
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
                    current_block,
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
                    current_block,
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
                    current_block,
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

                let (stop_expr, stop_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    stop,
                )?;

                if stop_tp.is_none() || !stop_tp.as_ref().unwrap().is_integer() {
                    return Err(ResolverError::invalid_stop_for_loop_type(
                        keyword.clone(),
                        stop_tp,
                    ));
                }

                let (step_expr, step_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    step,
                )?;

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
                    current_block,
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
                internal_error!(format!(
                    "Unexpected pre-processor command statement in the resolver. Symbol: {:?}",
                    symbol
                ));
            }
        };
    }

    fn check_expression(
        &self,
        current_module: &ProcessedModule,
        current_block: &CompilableBlock,
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
                    current_block,
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
            ExpressionNode::UnaryExpression { operator, rhs } => match operator.token_type() {
                TokenType::NotToken => {
                    let (rhs_expr, rhs_tp) = self.check_expression(
                        current_module,
                        current_block,
                        function_reference_token,
                        imports,
                        rhs,
                    )?;

                    if rhs_tp != Some(Type::Boolean) {
                        return Err(ResolverError::invalid_unary_not_type(
                            operator.clone(),
                            rhs_tp,
                        ));
                    }

                    (
                        CompilableExpressionNode::not_unary_expression(rhs_expr).wrapped(),
                        Some(Type::Boolean),
                    )
                }
                TokenType::MinusToken => {
                    let (rhs_expr, rhs_tp) = self.check_expression(
                        current_module,
                        current_block,
                        function_reference_token,
                        imports,
                        rhs,
                    )?;

                    if rhs_tp != Some(Type::Float) && rhs_tp != Some(Type::Integer) {
                        return Err(ResolverError::invalid_unary_negate_type(
                            operator.clone(),
                            rhs_tp,
                        ));
                    }

                    (
                        CompilableExpressionNode::negate_unary_expression(rhs_expr).wrapped(),
                        rhs_tp,
                    )
                }
                _ => internal_error!(format!("Unexpected unary expression operator {}", operator)),
            },
            ExpressionNode::BinaryExpression { lhs, operator, rhs } => {
                let (lhs_expr, lhs_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    lhs,
                )?;

                let (rhs_expr, rhs_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    rhs,
                )?;

                if lhs_tp != rhs_tp {
                    return Err(ResolverError::lhs_does_not_match_rhs_binary_expression(
                        lhs_tp,
                        operator.clone(),
                        rhs_tp,
                    ));
                }

                match operator.token_type() {
                    TokenType::PlusToken
                    | TokenType::MinusToken
                    | TokenType::StarToken
                    | TokenType::ForwardSlashToken => {
                        // We only need to check lhs since we previously established boths sides are identical
                        if lhs_tp != Some(Type::Integer) && lhs_tp != Some(Type::Float) {
                            return Err(
                                ResolverError::expected_numeric_values_for_binary_expression(
                                    lhs_tp,
                                    operator.clone(),
                                ),
                            );
                        }

                        let tp = lhs_tp.unwrap();

                        match operator.token_type() {
                            TokenType::PlusToken => (
                                CompilableExpressionNode::add_binary_expression(
                                    lhs_expr,
                                    rhs_expr,
                                    tp.clone(),
                                )
                                .wrapped(),
                                Some(tp),
                            ),
                            TokenType::MinusToken => (
                                CompilableExpressionNode::subtract_binary_expression(
                                    lhs_expr,
                                    rhs_expr,
                                    tp.clone(),
                                )
                                .wrapped(),
                                Some(tp),
                            ),
                            TokenType::StarToken => (
                                CompilableExpressionNode::multiply_binary_expression(
                                    lhs_expr,
                                    rhs_expr,
                                    tp.clone(),
                                )
                                .wrapped(),
                                Some(tp),
                            ),
                            TokenType::ForwardSlashToken => (
                                CompilableExpressionNode::divide_binary_expression(
                                    lhs_expr,
                                    rhs_expr,
                                    tp.clone(),
                                )
                                .wrapped(),
                                Some(tp),
                            ),
                            _ => internal_error!(format!(
                                "Unexpected operator for a binary expression: {}",
                                operator
                            )),
                        }
                    }
                    TokenType::GreaterThanToken
                    | TokenType::GreaterThanEqualToken
                    | TokenType::LessThanToken
                    | TokenType::LessThanEqualToken => {
                        // We only need to check lhs since we previously established boths sides are identical
                        if lhs_tp != Some(Type::Integer) && lhs_tp != Some(Type::Float) {
                            return Err(
                                ResolverError::expected_numeric_values_for_binary_expression(
                                    lhs_tp,
                                    operator.clone(),
                                ),
                            );
                        }

                        let tp = lhs_tp.unwrap();

                        match operator.token_type() {
                            TokenType::GreaterThanToken => (
                                CompilableExpressionNode::greater_binary_expression(
                                    lhs_expr, rhs_expr, tp,
                                )
                                .wrapped(),
                                Some(Type::Boolean),
                            ),
                            TokenType::GreaterThanEqualToken => (
                                CompilableExpressionNode::greater_equal_binary_expression(
                                    lhs_expr, rhs_expr, tp,
                                )
                                .wrapped(),
                                Some(Type::Boolean),
                            ),
                            TokenType::LessThanToken => (
                                CompilableExpressionNode::less_binary_expression(
                                    lhs_expr, rhs_expr, tp,
                                )
                                .wrapped(),
                                Some(Type::Boolean),
                            ),
                            TokenType::LessThanEqualToken => (
                                CompilableExpressionNode::less_equal_binary_expression(
                                    lhs_expr, rhs_expr, tp,
                                )
                                .wrapped(),
                                Some(Type::Boolean),
                            ),
                            _ => internal_error!(format!(
                                "Unexpected operator for a binary expression: {}",
                                operator
                            )),
                        }
                    }
                    TokenType::EqualsToken => (
                        CompilableExpressionNode::equal_to_binary_expression(
                            lhs_expr,
                            rhs_expr,
                            lhs_tp.unwrap(),
                        )
                        .wrapped(),
                        Some(Type::Boolean),
                    ),
                    TokenType::BangEqualsToken => (
                        CompilableExpressionNode::not_equal_to_binary_expression(
                            lhs_expr,
                            rhs_expr,
                            lhs_tp.unwrap(),
                        )
                        .wrapped(),
                        Some(Type::Boolean),
                    ),
                    _ => internal_error!(format!(
                        "Unexpected operator for a binary expression: {}",
                        operator
                    )),
                }
            }
            ExpressionNode::LogicalExpression { lhs, operator, rhs } => {
                let (lhs_expr, lhs_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    lhs,
                )?;

                let (rhs_expr, rhs_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    rhs,
                )?;

                if lhs_tp != Some(Type::Boolean) {
                    return Err(ResolverError::logical_expression_left_non_boolean(
                        operator.clone(),
                        lhs_tp.clone(),
                    ));
                }

                if rhs_tp != Some(Type::Boolean) {
                    return Err(ResolverError::logical_expression_right_non_boolean(
                        operator.clone(),
                        rhs_tp.clone(),
                    ));
                }

                let expr = match operator.token_type() {
                    TokenType::AndToken => {
                        CompilableExpressionNode::and_logical_expression(lhs_expr, rhs_expr)
                            .wrapped()
                    }
                    TokenType::OrToken => {
                        CompilableExpressionNode::or_logical_expression(lhs_expr, rhs_expr)
                            .wrapped()
                    }
                    _ => internal_error!(format!(
                        "Unexpected token in logical expression {}",
                        operator
                    )),
                };

                (expr, Some(Type::Boolean))
            }
            ExpressionNode::VariableExpression {
                keyword: _,
                variable,
            } => {
                let tp = self.check_variable(
                    variable,
                    current_block,
                    function_reference_token,
                    imports,
                )?;

                (
                    CompilableExpressionNode::variable_expression(variable.clone()).wrapped(),
                    Some(tp),
                )
            }
            ExpressionNode::AssignmentExpression { lhs, operator, rhs } => {
                let (lhs_expr, lhs_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    lhs,
                )?;

                let (rhs_expr, rhs_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    rhs,
                )?;

                let res;

                if lhs_expr.borrow().is_array_index_expression() {
                    if rhs_tp.is_none() || lhs_tp != rhs_tp {
                        return Err(ResolverError::invalid_assignment_type(
                            operator.clone(),
                            lhs_tp,
                            rhs_tp,
                        ));
                    }

                    res = (
                        CompilableExpressionNode::array_index_assignment_expression(
                            lhs_expr, rhs_expr,
                        )
                        .wrapped(),
                        None,
                    );
                } else if let CompilableExpressionNode::VariableExpression { variable } =
                    &*lhs_expr.borrow()
                {
                    if rhs_tp.is_none() || lhs_tp != rhs_tp {
                        return Err(ResolverError::invalid_assignment_type(
                            operator.clone(),
                            lhs_tp,
                            rhs_tp,
                        ));
                    }

                    let variable = variable.clone();

                    res = (
                        CompilableExpressionNode::variable_assignment_expression(
                            variable, rhs_expr,
                        )
                        .wrapped(),
                        None,
                    );
                } else {
                    internal_error!(format!(
                        "Unexpected expression type for assignment.\n{:?}",
                        lhs_expr
                    ));
                }

                res
            }
            ExpressionNode::GroupingExpression {
                reference_token: _,
                expression,
            } => {
                let (expr, tp) = self.check_expression(
                    current_module,
                    current_block,
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
                keyword: _,
                module,
                name,
                arguments,
            } => {
                let module_string = module
                    .as_ref()
                    .map(|m| m.lexeme())
                    .unwrap_or(&self.current_module);

                let encoded_function_name = Self::encode_fn_name(name.lexeme(), module_string);

                let mut args = Vec::with_capacity(arguments.len());

                let func;

                if let Some(m) = self.modules.get(module_string) {
                    if let Some(f) = m.functions.get(name.lexeme()) {
                        func = f;
                    } else {
                        return Err(ResolverError::no_function_with_name_in_module(
                            name.clone(),
                            module_string.clone(),
                        ));
                    }
                } else {
                    if let Some(m) = module {
                        return Err(ResolverError::no_module_defined_with_name_token(m.clone()));
                    } else {
                        return Err(ResolverError::no_module_defined_with_name(
                            self.current_module.clone(),
                        ));
                    }
                }

                let resulting_type;

                if let StatementNode::FunctionStatement {
                    keyword: _,
                    name: _,
                    arguments: defined_arguments,
                    return_type: defined_return_tp,
                    body: _,
                } = &*func.borrow()
                {
                    if arguments.len() != defined_arguments.len() {
                        return Err(ResolverError::arguments_count_does_not_match_definition(
                            defined_arguments.len(),
                            arguments.len(),
                            name.clone(),
                        ));
                    }

                    for (i, arg) in arguments.iter().enumerate() {
                        let (arg_expr, arg_tp) = self.check_expression(
                            current_module,
                            current_block,
                            function_reference_token,
                            imports,
                            arg,
                        )?;

                        if arg_tp.is_none() || &defined_arguments[i].1 != arg_tp.as_ref().unwrap() {
                            return Err(ResolverError::invalid_function_call_argument(
                                i,
                                defined_arguments[i].1.clone(),
                                arg_tp,
                                name.clone(),
                            ));
                        }

                        args.push(arg_expr);
                    }

                    resulting_type = defined_return_tp.clone();
                } else {
                    internal_error!(format!(
                        "Unexpected non-function statement. Statement: {:?}",
                        func
                    ));
                }

                (
                    CompilableExpressionNode::call_expression(
                        encoded_function_name,
                        args,
                        resulting_type.clone(),
                    )
                    .wrapped(),
                    resulting_type,
                )
            }
            ExpressionNode::ConstructorCallExpression {
                target_struct,
                module,
                arguments,
            } => {
                let module_string = module
                    .as_ref()
                    .map(|m| m.lexeme())
                    .unwrap_or(&self.current_module);

                let encoded_struct_name =
                    Self::encode_struct_name(target_struct.lexeme(), module_string);

                let strct;

                if let Some(m) = self.modules.get(module_string) {
                    if let Some(s) = m.structs.get(target_struct.lexeme()) {
                        strct = s;
                    } else {
                        return Err(ResolverError::no_struct_with_name_in_module(
                            target_struct.clone(),
                            module_string.clone(),
                        ));
                    }
                } else {
                    if let Some(m) = module {
                        return Err(ResolverError::no_module_defined_with_name_token(m.clone()));
                    } else {
                        return Err(ResolverError::no_module_defined_with_name(
                            self.current_module.clone(),
                        ));
                    }
                }

                let mut args = HashMap::new();

                if strct.fields.len() != arguments.len() {
                    return Err(
                        ResolverError::struct_values_count_does_not_match_definition(
                            strct.fields.len(),
                            arguments.len(),
                            target_struct.clone(),
                        ),
                    );
                }

                for (field_name, value) in arguments {
                    if let Some(field_tp) = strct.fields.get(field_name.lexeme()) {
                        let (value_expr, arg_tp) = self.check_expression(
                            current_module,
                            current_block,
                            function_reference_token,
                            imports,
                            value,
                        )?;

                        if arg_tp.is_none() || field_tp != arg_tp.as_ref().unwrap() {
                            return Err(ResolverError::invalid_constructor_call_argument(
                                field_name.clone(),
                                field_tp.clone(),
                                arg_tp,
                            ));
                        }

                        args.insert(field_name.lexeme().clone(), value_expr);
                    } else {
                        return Err(ResolverError::no_field_with_name_on_struct(
                            field_name.clone(),
                            target_struct.lexeme().clone(),
                        ));
                    }
                }

                (
                    CompilableExpressionNode::constructor_call_expression(
                        encoded_struct_name,
                        args,
                    )
                    .wrapped(),
                    Some(Type::Struct {
                        name: target_struct.lexeme().clone(),
                        module: module_string.clone(),
                    }),
                )
            }
            ExpressionNode::ArrayIndexExpression {
                open_brace_token,
                array_expression,
                index_expression,
            } => {
                let (array_expr, array_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    array_expression,
                )?;

                let (index_expr, index_tp) = self.check_expression(
                    current_module,
                    current_block,
                    function_reference_token,
                    imports,
                    index_expression,
                )?;

                if array_tp.is_none() || !array_tp.as_ref().unwrap().is_array() {
                    return Err(ResolverError::invalid_array_index_array_type(
                        open_brace_token.clone(),
                        array_tp,
                    ));
                }

                if index_tp != Some(Type::Integer) {
                    return Err(ResolverError::invalid_array_index_index_type(
                        open_brace_token.clone(),
                        array_tp,
                    ));
                }

                let tp = array_tp.unwrap().nested_array_type().unwrap().clone();

                (
                    CompilableExpressionNode::array_index_expression(
                        array_expr,
                        index_expr,
                        tp.clone(),
                    )
                    .wrapped(),
                    Some(tp),
                )
            }
        });
    }

    fn get_field(
        &self,
        reference_token: &Token,
        root_tp: &Type,
        field: &String,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<&Type> {
        match root_tp {
            Type::Boolean | Type::Integer | Type::Float | Type::Character | Type::Array(_) => {
                return Err(ResolverError::variable_type_does_not_have_field(
                    reference_token.clone(),
                    Some(root_tp.clone()),
                    field.clone(),
                ));
            }
            Type::Struct { name, module } => {
                if imports.is_none() || !imports.unwrap().contains(module) {
                    return Err(ResolverError::module_not_imported(
                        module.clone(),
                        self.current_module.clone(),
                    ));
                }

                if let Some(m) = self.modules.get(module) {
                    if let Some(strct) = m.structs.get(name) {
                        if let Some(field_tp) = strct.fields.get(field) {
                            return Ok(field_tp);
                        } else {
                            return Err(ResolverError::variable_type_does_not_have_field(
                                reference_token.clone(),
                                Some(root_tp.clone()),
                                field.clone(),
                            ));
                        }
                    } else {
                        return Err(ResolverError::no_object_defined_with_name_in_module(
                            name.clone(),
                            module.clone(),
                        ));
                    }
                } else {
                    return Err(ResolverError::no_module_defined_with_name(module.clone()));
                }
            }
        }
    }

    fn check_variable(
        &self,
        var: &Variable,
        current_block: &CompilableBlock,
        function_reference_token: &Token,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<Type> {
        if var.path().is_empty() {
            internal_error!(format!(
                "Variable cannot be checked when empty. {}",
                function_reference_token
            ));
        }

        let root_struct_tp = if let Some(var_tp) = current_block
            .borrow()
            .variable_list
            .get(var.path()[0].lexeme())
        {
            var_tp.clone()
        } else {
            return Err(ResolverError::no_variable_declared_with_name_in_scope(
                function_reference_token.clone(),
            ));
        };

        let mut current_field_tp = &root_struct_tp;

        for i in 1..var.path().len() {
            let field = &var.path()[i];

            current_field_tp = self.get_field(field, &current_field_tp, field.lexeme(), imports)?;
        }

        return Ok(current_field_tp.clone());
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
                        self.modules.get(module).unwrap()
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
            include_std_library: true,
            functions: Vec::new(),
            structs: Vec::new(),
            current_module: String::new(),
        };
    }
}

#[cfg(test)]
mod tests;
