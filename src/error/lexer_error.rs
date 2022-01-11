use std::error::Error as ErrorTrait;
use std::fmt;

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
