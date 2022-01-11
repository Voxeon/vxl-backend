use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum PreProcessorCommand {
    BeginModuleCommand(Token),        // Module name
    ImportCommand(Token, Vec<Token>), // Module name, function/struct names
}
