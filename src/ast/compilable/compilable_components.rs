use std::collections::HashMap;

use crate::ast::compilable::CompilableStatement;
use crate::ast::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableProgram {
    functions: Vec<CompilableFunction>,
    structs: Vec<CompilableStruct>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableFunction {
    label: String,
    return_type: Type,
    body: CompilableBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableStruct {
    label: String,
    fields: HashMap<String, Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableBlock {
    variable_list: HashMap<String, Type>,
    body: Vec<CompilableStatement>,
}
