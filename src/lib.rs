#[macro_use]
mod macros;

pub mod ast;
//pub mod compiler;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod pre_processor;
pub mod processed_module;
pub mod resolver;
pub mod std_library;
mod token;

const ROOT_MODULE_NAME: &'static str = "root";
pub use token::{Token, TokenType};
