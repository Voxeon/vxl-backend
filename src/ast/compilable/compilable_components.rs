use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::compilable::CompilableStatement;
use crate::ast::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableProgram {
    pub(crate) functions: Vec<CompilableFunction>,
    pub(crate) structs: Vec<CompilableStruct>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableFunction {
    pub(crate) label: String,
    pub(crate) return_type: Option<Type>,
    pub(crate) body: CompilableBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableStruct {
    pub(crate) label: String,
    pub(crate) fields: HashMap<String, Type>,
}

pub type CompilableBlock = Rc<RefCell<CompilableBlockNode>>;

#[derive(Debug, PartialEq, Clone)]
pub struct CompilableBlockNode {
    pub(crate) parent_block: Option<CompilableBlock>,
    pub(crate) variable_list: HashMap<String, Type>,
    pub(crate) body: Vec<CompilableStatement>,
}

impl CompilableFunction {
    pub fn new(label: String, return_type: Option<Type>, body: CompilableBlock) -> Self {
        return Self {
            label,
            return_type,
            body,
        };
    }
}

impl CompilableStruct {
    pub fn new(label: String, fields: HashMap<String, Type>) -> Self {
        return Self { label, fields };
    }
}

impl CompilableBlockNode {
    pub fn new(
        parent_block: Option<CompilableBlock>,
        variable_list: HashMap<String, Type>,
        body: Vec<CompilableStatement>,
    ) -> Self {
        return Self {
            parent_block,
            variable_list,
            body,
        };
    }

    pub fn new_child(parent_block: CompilableBlock) -> Self {
        return Self {
            parent_block: Some(parent_block),
            variable_list: HashMap::new(),
            body: Vec::new(),
        };
    }

    pub fn wrapped(self) -> CompilableBlock {
        return Rc::new(RefCell::new(self));
    }

    pub fn lookup_variable(&self) -> Option<Type> {
        todo!();
    }
}

impl Default for CompilableBlockNode {
    fn default() -> Self {
        return Self::new(None, HashMap::new(), Vec::new());
    }
}
