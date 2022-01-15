use crate::ast::Type;
use crate::Token;
use crate::ROOT_MODULE_NAME;

use std::error::Error as ErrorTrait;
use std::fmt;

struct_enum_with_functional_inits! {
    pub
    [Clone, Debug, PartialEq, Eq]
    ResolverError {
        NoRootModuleDefined
        NoModuleDefined {
            module: Token
        }
        NoModuleDefinedWithName {
            module: String
        }
        NoModuleDefinedWithNameToken {
            module: Token
        }
        NoObjectDefinedWithNameInModule {
            object: String,
            module: String
        }
        NoModuleDefinedWithNameInStruct {
            module: String,
            associated_struct: String
        }
        NoObjectDefinedWithNameInModuleInStruct {
            object: String,
            module: String,
            associated_struct: String
        }
        NoObjectDefinedWithNameInModuleInFunction {
            object: String,
            module: String,
            reference_token: Token
        }
        NoObjectDefined {
            object: Token,
            module: Token
        }
        ModuleNotImported {
            import_module: String,
            current_module: String
        }
        ReturnTypeDoesNotMatch {
            function_reference_token: Token,
            keyword: Token,
            function_return_type: Option<Type>,
            return_type: Option<Type>
        }
        VariableDeclarationRequiresValue {
            name: Token
        }
        FunctionDefinitionForbiddenHere {
            name: Token
        }
        StructDefinitionForbiddenHere {
            name: Token
        }
        InvalidStartForLoopType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidStopForLoopType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidStepForLoopType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidWhileLoopConditionType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidIfConditionType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidArrayLiteralCountType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidUnaryNotType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidUnaryNegateType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidArrayIndexArrayType {
            reference_token: Token,
            tp: Option<Type>
        }
        InvalidArrayIndexIndexType {
            reference_token: Token,
            tp: Option<Type>
        }
        VariableTypeDoesNotHaveField {
            reference_token: Token,
            field_tp: Option<Type>,
            field: String
        }
        NoVariableDeclaredWithNameInScope {
            reference_token: Token
        }
        InvalidAssignmentType {
            reference_token: Token,
            lhs_tp: Option<Type>,
            rhs_tp: Option<Type>
        }
        NoFunctionWithNameInModule {
            name: Token,
            module: String
        }
        NoStructWithNameInModule {
            name: Token,
            module: String
        }
        ArgumentsCountDoesNotMatchDefinition {
            defined_len: usize,
            arguments_len: usize,
            call_reference_token: Token
        }
        StructValuesCountDoesNotMatchDefinition {
            defined_len: usize,
            arguments_len: usize,
            call_reference_token: Token
        }
        InvalidFunctionCallArgument {
            arg_index: usize,
            defined_type: Type,
            found_type: Option<Type>,
            call_reference_token: Token
        }
        InvalidConstructorCallArgument {
            field_name: Token,
            defined_type: Type,
            found_type: Option<Type>
        }
        NoFieldWithNameOnStruct {
            field_name: Token,
            struct_name: String
        }
        LogicalExpressionLeftNonBoolean {
            operator_token: Token,
            lhs_tp: Option<Type>
        }
        LogicalExpressionRightNonBoolean {
            operator_token: Token,
            rhs_tp: Option<Type>
        }
        LhsDoesNotMatchRhsBinaryExpression {
            lhs_tp: Option<Type>,
            operator: Token,
            rhs_tp: Option<Type>
        }
        ExpectedNumericValuesForBinaryExpression {
            tp: Option<Type>,
            operator: Token
        }
        RecursiveReferenceDetected {
            root_struct_name: String,
            root_struct_module: String,
            recursive_struct_name: String,
            recursive_struct_module: String,
            field_name: String
        }
        FunctionDoesNotAlwaysReturn {
            function_reference_token: Token,
            return_type: Type
        }
        UnreachableStatement {
            function_reference_token: Token,
            statement: Token
        }
    }
}

impl ErrorTrait for ResolverError {}

fn optional_type_to_string(tp: &Option<Type>) -> String {
    return tp
        .as_ref()
        .map(|tp| tp.to_string())
        .unwrap_or("No Value".to_string());
}

impl fmt::Display for ResolverError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            ResolverError::NoRootModuleDefined => {
                write!(
                    f,
                    "No \'{}\' module defined.\nStart the first file with `%begin {}`",
                    ROOT_MODULE_NAME, ROOT_MODULE_NAME
                )
            }
            ResolverError::NoModuleDefined { module } => {
                write!(
                    f,
                    "No \'{}\' module defined.\nStart a module with `%begin {}`",
                    module.lexeme(),
                    module.lexeme()
                )
            }
            ResolverError::NoObjectDefined { object, module } => {
                write!(
                    f,
                    "No object called \'{}\' is defined in the module \'{}\'.",
                    object.lexeme(),
                    module.lexeme()
                )
            }
            ResolverError::NoModuleDefinedWithNameInStruct {
                module,
                associated_struct,
            } => {
                write!(
                    f,
                    "No \'{}\' module defined. Referenced in the definition of the struct \'{}\'.\nStart a module with `%begin {}`",
                    module,
                    associated_struct,
                    module
                )
            }
            ResolverError::NoObjectDefinedWithNameInModuleInStruct {
                object,
                module,
                associated_struct,
            } => {
                write!(
                    f,
                    "No object called \'{}\' is defined in the module \'{}\'.\nReferenced in the definition of the struct \'{}\'.",
                    object,
                    module,
                    associated_struct
                )
            }
            ResolverError::NoModuleDefinedWithName { module } => {
                write!(
                    f,
                    "No \'{}\' module defined.\nStart a module with `%begin {}`",
                    module, module
                )
            }
            ResolverError::NoModuleDefinedWithNameToken { module } => {
                write!(
                    f,
                    "No module with the name \'{}\' has been defined {}",
                    module.lexeme(),
                    module
                )
            }
            ResolverError::NoObjectDefinedWithNameInModule { object, module } => {
                write!(
                    f,
                    "No object called \'{}\' is defined in the module \'{}\'.",
                    object, module,
                )
            }
            ResolverError::ModuleNotImported {
                import_module,
                current_module,
            } => {
                write!(
                    f,
                    "The module \'{}\' has not been imported into the module \'{}\'.\nPlease import before using the associated module's types.",
                    import_module,
                    current_module
                )
            }
            ResolverError::ReturnTypeDoesNotMatch {
                function_reference_token,
                keyword,
                function_return_type,
                return_type,
            } => write!(
                f,
                "Return type does not match function definition ({} != {}) {} {}",
                optional_type_to_string(function_return_type),
                optional_type_to_string(return_type),
                function_reference_token,
                keyword
            ),

            ResolverError::NoObjectDefinedWithNameInModuleInFunction {
                object,
                module,
                reference_token,
            } => write!(
                f,
                "No object defined with the name '{}' in the module '{}', {}",
                object, module, reference_token
            ),
            ResolverError::VariableDeclarationRequiresValue { name } => {
                write!(f, "Variable declaration requires a value {}", name)
            }
            ResolverError::FunctionDefinitionForbiddenHere { name } => write!(
                f,
                "Cannot define a new function here. Define a new function at the top level. {}",
                name
            ),
            ResolverError::StructDefinitionForbiddenHere { name } => write!(
                f,
                "Cannot define a new struct here. Define a new struct at the top level. {}",
                name
            ),
            ResolverError::InvalidStartForLoopType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "The start value for a for loop must be an integer value not '{}' {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidStopForLoopType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "The stop value for a for loop must be an integer value not '{}' {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidStepForLoopType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "The step value for a for loop must be an integer value not '{}' {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidWhileLoopConditionType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "The condition for a while loop must be a boolean value not '{}' {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidIfConditionType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "The condition for an if statement must be a boolean value not '{}' {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidArrayLiteralCountType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "An array literal must have a count of type integer not '{}' {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidUnaryNotType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "Cannot use 'not' on expressions of type '{}', use a boolean instead {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidUnaryNegateType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "Cannot negate expressions of type '{}', use a float or integer instead {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidArrayIndexArrayType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "Cannot index into objects of the type '{}', use an array instead {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::InvalidArrayIndexIndexType {
                reference_token,
                tp,
            } => {
                write!(
                    f,
                    "Cannot use objects of the type '{}', to index an array {}",
                    optional_type_to_string(tp),
                    reference_token
                )
            }
            ResolverError::VariableTypeDoesNotHaveField {
                reference_token,
                field_tp,
                field,
            } => {
                write!(
                    f,
                    "There is no field called \'{}\' on an object of type {} {}",
                    field,
                    optional_type_to_string(field_tp),
                    reference_token
                )
            }
            ResolverError::NoVariableDeclaredWithNameInScope { reference_token } => {
                write!(
                    f,
                    "There is no variable called \'{}\' in the current scope {}",
                    reference_token.lexeme(),
                    reference_token
                )
            }
            ResolverError::InvalidAssignmentType {
                reference_token,
                lhs_tp,
                rhs_tp,
            } => {
                write!(
                    f,
                    "Cannot assign a value of type {} to a variable of type {} {}",
                    optional_type_to_string(rhs_tp),
                    optional_type_to_string(lhs_tp),
                    reference_token
                )
            }
            ResolverError::NoFunctionWithNameInModule { name, module } => write!(
                f,
                "There is no function called \'{}\' in the module \'{}\' {}",
                name.lexeme(),
                module,
                name
            ),
            ResolverError::ArgumentsCountDoesNotMatchDefinition {
                defined_len,
                arguments_len,
                call_reference_token,
            } => write!(
                f,
                "This function expects {} arguments, instead {} were provided {}",
                defined_len, arguments_len, call_reference_token
            ),
            ResolverError::StructValuesCountDoesNotMatchDefinition {
                defined_len,
                arguments_len,
                call_reference_token,
            } => write!(
                f,
                "This struct expects {} values, instead {} were provided {}",
                defined_len, arguments_len, call_reference_token
            ),
            ResolverError::InvalidFunctionCallArgument {
                arg_index,
                defined_type,
                found_type,
                call_reference_token,
            } => {
                write!(f, "Argument {} of this function call expected an argument of type \'{}\' instead an argument of type \'{}\' was found {}", arg_index, defined_type, optional_type_to_string(found_type), call_reference_token)
            }
            ResolverError::NoStructWithNameInModule { name, module } => write!(
                f,
                "There is no struct called \'{}\' in the module \'{}\' {}",
                name.lexeme(),
                module,
                name
            ),
            ResolverError::InvalidConstructorCallArgument {
                field_name,
                defined_type,
                found_type,
            } => {
                write!(f, "The field called '{}' of this struct expects an argument of type '{}' instead an argument of type '{}' was found {}", field_name.lexeme(), defined_type, optional_type_to_string(found_type), field_name)
            }
            ResolverError::NoFieldWithNameOnStruct {
                field_name,
                struct_name,
            } => write!(
                f,
                "The field called '{}' does not exist in the definition of the struct '{}' {}",
                field_name.lexeme(),
                struct_name,
                field_name
            ),
            ResolverError::LogicalExpressionLeftNonBoolean {
                operator_token,
                lhs_tp,
            } => {
                write!(f, "Expected an expression of type 'boolean' left-hand side of this logical expression instead found an expression of type '{}' {}", optional_type_to_string(lhs_tp), operator_token)
            }
            ResolverError::LogicalExpressionRightNonBoolean {
                operator_token,
                rhs_tp,
            } => write!(f, "Expected an expression of type 'boolean' right-hand side of this logical expression instead found an expression of type '{}' {}", optional_type_to_string(rhs_tp), operator_token),
            ResolverError::LhsDoesNotMatchRhsBinaryExpression { lhs_tp, operator, rhs_tp } => {
                write!(f, "The left-hand side is of type '{}', whilst the right-hand side is of type '{}', ensure both sides of a binary expression match {}", optional_type_to_string(lhs_tp), optional_type_to_string(rhs_tp), operator)
            },
            ResolverError::ExpectedNumericValuesForBinaryExpression { tp, operator } => {
                write!(f, "This binary expression expected values of type integer or float instead values of type '{}' were provided {}", optional_type_to_string(tp), operator)
            }
            ResolverError::RecursiveReferenceDetected { root_struct_name, root_struct_module, recursive_struct_name, recursive_struct_module, field_name } => {
                write!(f, "Recursive struct definition detected. The struct '{}' in the module '{}' is referenced in the field '{}' of the struct '{}' in the module '{}' which forms a circular reference.", root_struct_name, root_struct_module, field_name, recursive_struct_name, recursive_struct_module)
            },
            ResolverError::FunctionDoesNotAlwaysReturn { function_reference_token, return_type } => {
                write!(f, "The function '{}' does not always return. Ensure the function returns a value of type {} {}", function_reference_token.lexeme(), return_type, function_reference_token)
            },
            ResolverError::UnreachableStatement { function_reference_token, statement } => {
                write!(f, "The function '{}' returns before reaching the statement '{}' {}", function_reference_token.lexeme(), statement.lexeme(), statement)
            },
        };
    }
}
