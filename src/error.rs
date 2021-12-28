use crate::lexer::token::Token;
use crate::ROOT_MODULE_NAME;

use std::error::Error as ErrorTrait;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VoxlError<BE: ErrorTrait> {
    IOError(String),
    CustomError(String),
    LexerError(LexerError),
    BuilderError(BE),
    ParserError(ParserError),
    PreProcessorError(PreProcessorError),
    ResolverError(ResolverError),
    AssemblerError(String),
    AssemblerExecutionError(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LexerError {
    tp: LexerErrorType,
    row: usize,
    column: usize,
    file: Option<String>,
}

struct_enum_with_functional_inits! {
    pub
    [Clone, Debug, PartialEq, Eq, Hash]
    LexerErrorType {
        Expected {
            expected_string: String,
            after_string: String
        }
        UnexpectedCharacter {
            character: char
        }
        UnexpectedCharacterExpected {
            unexpected_character: char,
            expected_string: String
        }
        UnterminatedString
        UnterminatedCharacterLiteral
        EmptyCharacterLiteral
        InvalidUnicodeEscapeSequence {
            sequence: String
        }
        InvalidUnicodeEscapeSequenceLength
    }
}

struct_enum_with_functional_inits! {
    pub
    [Clone, Debug, PartialEq, Eq, Hash]
    ParserError {
        UnexpectedToken {
            token: Token
        }
        ExpectedFoundEOF {
            expected: String
        }
        ExpectedFound {
            expected: String,
            found: String
        }
        InvalidIntegerLiteral {
            literal: Token
        }
        InvalidDoubleLiteral {
            literal: Token
        }
        TooManyFunctionArguments {
            reference_token: Token
        }
        InvalidAssignmentTarget {
            reference_token: Token
        }
        UnterminatedBlock {
            reference_token: Token
        }
        InvalidPreProcessorCommand {
            command: Token
        }
        FieldAlreadyDefinedForStruct {
            field_name: String,
            struct_name: Token
        }
    }
}

struct_enum_with_functional_inits! {
    pub
    [Clone, Debug, PartialEq, Eq, Hash]
    PreProcessorError {
        NoRootModuleDefined
        ModuleAlreadyDefined {
            new_module: Token,
            original_module: Token
        }
        NoCurrentModuleDefined {
            reference_token: Token
        }
    }
}

struct_enum_with_functional_inits! {
    pub
    [Clone, Debug, PartialEq, Eq, Hash]
    ResolverError {
        NoRootModuleDefined
        NoModuleDefined {
            module: Token
        }
        NoModuleDefinedWithName {
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
            function: String
        }
        NoObjectDefined {
            object: Token,
            module: Token
        }
        ModuleNotImported {
            import_module: String,
            current_module: String
        }
        Multiple {
            errors: Vec<ResolverError>
        }
    }
}

impl<BE: ErrorTrait> VoxlError<BE> {
    pub fn new_io_error(e: std::io::Error) -> Self {
        return Self::IOError(format!("{}", e));
    }
}

impl<BE: ErrorTrait> fmt::Display for VoxlError<BE> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            VoxlError::CustomError(description) => write!(f, "{}", description),
            VoxlError::BuilderError(err) => write!(f, "Builder error: {}", err),
            VoxlError::IOError(description) => write!(f, "IOError: {}", description),
            VoxlError::AssemblerError(description) => write!(f, "{}", description),
            VoxlError::AssemblerExecutionError(description) => {
                write!(f, "Assembler execution error: {}", description)
            }
            VoxlError::LexerError(le) => write!(f, "{}", le),
            VoxlError::ParserError(err) => write!(f, "Parser error: {}", err),
            VoxlError::PreProcessorError(err) => write!(f, "Pre-Processor error: {}", err),
            VoxlError::ResolverError(err) => write!(f, "Resolver error: {}", err),
        };
    }
}

impl<BE: ErrorTrait> ErrorTrait for VoxlError<BE> {}

impl fmt::Display for LexerErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            LexerErrorType::UnexpectedCharacter { character } => {
                write!(f, "Unexpected character '{}'", character)
            }
            LexerErrorType::UnexpectedCharacterExpected {
                unexpected_character,
                expected_string,
            } => write!(
                f,
                "Unexpected character '{}', expected '{}'",
                unexpected_character, expected_string
            ),
            LexerErrorType::UnterminatedString => {
                write!(f, "Unterminated string")
            }
            LexerErrorType::UnterminatedCharacterLiteral => {
                write!(f, "Unterminated character literal",)
            }
            LexerErrorType::EmptyCharacterLiteral => write!(f, "Empty character literal"),
            LexerErrorType::InvalidUnicodeEscapeSequence { sequence } => {
                write!(f, "Invalid unicode escape sequence '\\u{{{}}}'", sequence)
            }
            LexerErrorType::InvalidUnicodeEscapeSequenceLength => {
                write!(f, "Invalid unicode sequence. Must contain 8 hex characters")
            }
            LexerErrorType::Expected {
                expected_string,
                after_string,
            } => write!(f, "Expected '{}' after '{}'", expected_string, after_string),
        };
    }
}

impl LexerError {
    pub fn new(tp: LexerErrorType, row: usize, column: usize, file: Option<String>) -> Self {
        return Self {
            tp,
            row,
            column,
            file,
        };
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match &self.file {
            Some(file) => write!(f, "{}\n{} ({}, {})", &self.tp, file, self.row, self.column),
            None => write!(f, "{}({}, {})", &self.tp, self.row, self.column),
        };
    }
}

impl ErrorTrait for LexerError {}

impl ErrorTrait for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            ParserError::UnexpectedToken { token } => {
                write!(f, "Unexpected token {}", token)
            }
            ParserError::ExpectedFoundEOF { expected } => {
                write!(
                    f,
                    "Expected {} instead found the end of the file.",
                    expected
                )
            }
            ParserError::ExpectedFound { expected, found } => {
                write!(f, "Expected {} instead found \"{}\".", expected, found)
            }
            ParserError::InvalidIntegerLiteral { literal } => {
                write!(f, "Invalid integer literal {}", literal)
            }
            ParserError::InvalidDoubleLiteral { literal } => {
                write!(f, "Invalid double literal {}", literal)
            }
            ParserError::TooManyFunctionArguments { reference_token } => {
                write!(
                    f,
                    "Too many function arguments (Max 255) {}",
                    reference_token
                )
            }
            ParserError::InvalidAssignmentTarget { reference_token } => {
                write!(f, "Invalid assignment target. {}", reference_token)
            }
            ParserError::UnterminatedBlock { reference_token } => {
                write!(
                    f,
                    "Unterminated block. Ensure that each block ends with an 'end' keyword. {}",
                    reference_token
                )
            }
            ParserError::InvalidPreProcessorCommand { command } => {
                write!(f, "Invalid pre-processor command. {}", command)
            }
            ParserError::FieldAlreadyDefinedForStruct {
                field_name,
                struct_name,
            } => {
                write!(
                    f,
                    "The field with the name: \'{}\', has already been defined for this struct. {}",
                    field_name, struct_name
                )
            }
        };
    }
}

impl ErrorTrait for PreProcessorError {}

impl fmt::Display for PreProcessorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            PreProcessorError::NoRootModuleDefined => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart the first file with `%begin {}`",
                    ROOT_MODULE_NAME, ROOT_MODULE_NAME
                )
            }
            PreProcessorError::ModuleAlreadyDefined {
                new_module,
                original_module,
            } => {
                write!(
                    f,
                    "Module \"{}\" already defined here {}.",
                    new_module, original_module
                )
            }
            PreProcessorError::NoCurrentModuleDefined { reference_token } => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart a new module with `%begin`. {}",
                    ROOT_MODULE_NAME, reference_token
                )
            }
        };
    }
}

impl ErrorTrait for ResolverError {}

impl fmt::Display for ResolverError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            ResolverError::NoRootModuleDefined => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart the first file with `%begin {}`",
                    ROOT_MODULE_NAME, ROOT_MODULE_NAME
                )
            }
            ResolverError::Multiple { errors } => {
                write!(
                    f,
                    "{}",
                    errors
                        .iter()
                        .map(|e| [e.to_string(), String::from("\n")].join(""))
                        .collect::<String>()
                )
            }
            ResolverError::NoModuleDefined { module } => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart a module with `%begin {}`",
                    module.lexeme(),
                    module.lexeme()
                )
            }
            ResolverError::NoObjectDefined { object, module } => {
                write!(
                    f,
                    "No object called \"{}\" is defined in the module \"{}\".",
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
                    "No \"{}\" module defined. Referenced in the definition of the struct \"{}\".\nStart a module with `%begin {}`",
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
                    "No object called \"{}\" is defined in the module \"{}\".\nReferenced in the definition of the struct \"{}\".",
                    object,
                    module,
                    associated_struct
                )
            }
            ResolverError::NoModuleDefinedWithName { module } => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart a module with `%begin {}`",
                    module, module
                )
            }
            ResolverError::NoObjectDefinedWithNameInModule { object, module } => {
                write!(
                    f,
                    "No object called \"{}\" is defined in the module \"{}\".",
                    object, module,
                )
            }
            ResolverError::ModuleNotImported {
                import_module,
                current_module,
            } => {
                write!(
                    f,
                    "The module \"{}\" has not been imported into the module \"{}\".\nPlease import before using the associated module's types.",
                    import_module,
                    current_module
                )
            }
            ResolverError::NoObjectDefinedWithNameInModuleInFunction { .. } => todo!(),
        };
    }
}
