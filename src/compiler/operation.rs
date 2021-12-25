use crate::ast::Value;
use crate::compiler::context::Context;

#[derive(Clone, PartialEq, Debug)]
pub enum Operation {
    BeginContext(Context),
    NewValue(u64, Value),
    CloneValue(u64, u64),

    // Binary Operations
    AdditionOperation(u64, u64),
    SubtractionOperation(u64, u64),
    DivisionOperation(u64, u64),
    MultiplicationOperation(u64, u64),
}
