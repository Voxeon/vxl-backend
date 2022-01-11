use super::{LexerError, ParserError, PreProcessorError, ResolverError};

use std::error::Error as ErrorTrait;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
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
