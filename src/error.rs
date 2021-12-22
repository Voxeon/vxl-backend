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
    AssemblerError(String),
    AssemblerExecutionError(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LexerError {
    Expected(String, String, usize, usize, Option<String>),
    UnexpectedCharacter(char, usize, usize, Option<String>),
    UnexpectedCharacterExpected(char, String, usize, usize, Option<String>),
    UnterminatedString(usize, usize, Option<String>),
    UnterminatedCharacterLiteral(usize, usize, Option<String>),
    EmptyCharacterLiteral(usize, usize, Option<String>),
    InvalidUnicodeEscapeSequence(String, usize, usize, Option<String>),
    InvalidUnicodeEscapeSequenceLength(usize, usize, Option<String>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ParserError {
    UnexpectedToken(Token),
    ExpectedFoundEOF(String),
    ExpectedFound(String, String),
    InvalidIntegerLiteral(Token),
    InvalidDoubleLiteral(Token),
    TooManyFunctionArguments(Token),
    InvalidAssignmentTarget(Token),
    UnterminatedBlock(Token),
    InvalidPreProcessorCommand(Token),
    FieldAlreadyDefinedForStruct(String, Token),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PreProcessorError {
    NoRootModuleDefined,
    ModuleAlreadyDefined(Token, Token),
    NoCurrentModuleDefined(Token),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ResolverError {
    NoRootModuleDefined,
    NoModuleDefined(Token),
    NoObjectDefined(Token, Token),
    Multiple(Vec<ResolverError>),
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
        };
    }
}

impl<BE: ErrorTrait> ErrorTrait for VoxlError<BE> {}

impl LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            LexerError::UnexpectedCharacter(ch, line, col, file) => match file {
                Some(file) => write!(
                    f,
                    "Unexpected character '{}'\n{} ({}, {})",
                    ch, file, line, col
                ),
                None => write!(f, "Unexpected character '{}'({}, {})", ch, line, col),
            },
            LexerError::UnexpectedCharacterExpected(ch, expected, line, col, file) => match file {
                Some(file) => write!(
                    f,
                    "Unexpected character '{}', expected '{}'.\n{} ({}, {})",
                    ch, expected, file, line, col
                ),
                None => write!(
                    f,
                    "Unexpected character '{}', expected '{}' ({}, {})",
                    ch, expected, line, col
                ),
            },
            LexerError::UnterminatedString(line, col, file) => match file {
                Some(file) => write!(f, "Unterminated string \n{} ({}, {})", file, line, col),
                None => write!(f, "Unterminated string ({}, {})", line, col),
            },
            LexerError::UnterminatedCharacterLiteral(line, col, file) => match file {
                Some(file) => write!(
                    f,
                    "Unterminated character literal \n{} ({}, {})",
                    file, line, col
                ),
                None => write!(f, "Unterminated character literal ({}, {})", line, col),
            },
            LexerError::EmptyCharacterLiteral(line, col, file) => match file {
                Some(file) => write!(f, "Empty character literal \n{} ({}, {})", file, line, col),
                None => write!(f, "Empty character literal ({}, {})", line, col),
            },
            LexerError::InvalidUnicodeEscapeSequence(text, line, col, file) => match file {
                Some(file) => write!(
                    f,
                    "Invalid unicode sequence '\\u{{{}}}'\n{} ({}, {})",
                    text, file, line, col
                ),
                None => write!(
                    f,
                    "Invalid unicode sequence '\\u{{{}}}' ({}, {})",
                    text, line, col
                ),
            },
            LexerError::InvalidUnicodeEscapeSequenceLength(line, col, file) => match file {
                Some(file) => write!(
                    f,
                    "Invalid unicode sequence. Must contain 8 hex characters.\n{} ({}, {})",
                    file, line, col
                ),
                None => write!(
                    f,
                    "Invalid unicode sequence. Must contain 8 hex characters. ({}, {})",
                    line, col
                ),
            },
            LexerError::Expected(expected, after, line, col, file) => match file {
                Some(file) => write!(
                    f,
                    "Expected \"{}\" after \"{}\".\n{} ({}, {})",
                    expected, after, file, line, col
                ),
                None => write!(
                    f,
                    "Expected \"{}\" after \"{}\".({}, {})",
                    expected, after, line, col
                ),
            },
        };
    }
}

impl ErrorTrait for LexerError {}

impl ErrorTrait for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            ParserError::UnexpectedToken(tok) => write!(f, "Unexpected token {}", tok),
            ParserError::ExpectedFoundEOF(tp) => {
                write!(f, "Expected {} instead found the end of the file.", tp)
            }
            ParserError::ExpectedFound(tp, found) => {
                write!(f, "Expected {} instead found \"{}\".", tp, found)
            }
            ParserError::InvalidIntegerLiteral(tok) => {
                write!(f, "Invalid integer literal {}", tok)
            }
            ParserError::InvalidDoubleLiteral(tok) => {
                write!(f, "Invalid double literal {}", tok)
            }
            ParserError::TooManyFunctionArguments(tok) => {
                write!(f, "Too many function arguments (Max 255) {}", tok)
            }
            ParserError::InvalidAssignmentTarget(tok) => {
                write!(f, "Invalid assignment target. {}", tok)
            }
            ParserError::UnterminatedBlock(tok) => {
                write!(
                    f,
                    "Unterminated block. Ensure that each block ends with an 'end' keyword. {}",
                    tok
                )
            }
            ParserError::InvalidPreProcessorCommand(tok) => {
                write!(f, "Invalid pre-processor command. {}", tok)
            }
            ParserError::FieldAlreadyDefinedForStruct(field_name, struct_name) => {
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
            PreProcessorError::ModuleAlreadyDefined(new, original) => {
                write!(f, "Module \"{}\" already defined here {}.", new, original)
            }
            PreProcessorError::NoCurrentModuleDefined(tok) => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart a new module with `%begin`. {}",
                    ROOT_MODULE_NAME, tok
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
            ResolverError::Multiple(errs) => {
                write!(
                    f,
                    "{}",
                    errs.iter()
                        .map(|e| [e.to_string(), String::from("\n")].join(""))
                        .collect::<String>()
                )
            }
            ResolverError::NoModuleDefined(m) => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart a module with `%begin {}`",
                    m.lexeme(),
                    m.lexeme()
                )
            }
            ResolverError::NoObjectDefined(o, m) => {
                write!(
                    f,
                    "No object called \"{}\" is defined in the module \"{}\".",
                    o.lexeme(),
                    m.lexeme()
                )
            }
        };
    }
}
