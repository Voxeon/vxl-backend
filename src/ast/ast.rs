use super::Statement;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    root_statements: VecDeque<Statement>,
}

impl AST {
    pub(crate) fn new() -> Self {
        return Self {
            root_statements: VecDeque::new(),
        };
    }

    pub(crate) fn push_statement(&mut self, statement: Statement) {
        self.root_statements.push_back(statement);
    }

    pub(crate) fn pop_front(&mut self) -> Option<Statement> {
        return self.root_statements.pop_front();
    }

    /// By default this function will append the contents of another
    pub fn merge_with(&mut self, mut other: Self) {
        self.root_statements.append(&mut other.root_statements);
    }
}
