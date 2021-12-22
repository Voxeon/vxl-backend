use std::cell::RefCell;
use std::rc::Rc;

use crate::compiler::operation::Operation;
use crate::compiler::values::Values;

pub type ContextPtr = Rc<RefCell<Context>>;

#[derive(Clone, PartialEq, Debug)]
pub struct Context {
    name: String,
    parent: Option<ContextPtr>,
    operations: Vec<Operation>,
    values: Values,
    arguments: u64,
}

impl Context {
    pub fn new(name: &str) -> Self {
        return Self {
            name: name.to_string(),
            parent: None,
            operations: Vec::new(),
            values: Values::new(),
            arguments: 0,
        };
    }

    pub fn new_child(name: &str, parent: ContextPtr) -> Self {
        let values = Values::new_child(parent.borrow().values.clone());

        return Self {
            name: name.to_string(),
            parent: Some(parent),
            operations: Vec::new(),
            values,
            arguments: 0,
        };
    }
}
