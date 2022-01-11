use crate::Token;

use std::error::Error as ErrorTrait;
use std::fmt;

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
        StringLiteralLongerThanMax {
            string: Token,
            length: usize,
            max_length: usize
        }
        DuplicateFunctionArgument {
            function_name_token: Token,
            duplicate_token: Token
        }
    }
}

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
            ParserError::StringLiteralLongerThanMax {
                string,
                length,
                max_length,
            } => write!(
                f,
                "The string '{}' of length {} is longer than the maximum allowed length({})",
                string, length, max_length
            ),
            ParserError::DuplicateFunctionArgument {
                function_name_token,
                duplicate_token,
            } => {
                write!(
                    f,
                    "Duplicate arguments ('{}') in function definition {}",
                    duplicate_token.lexeme(),
                    function_name_token
                )
            }
        };
    }
}
