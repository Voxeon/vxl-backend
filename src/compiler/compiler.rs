// Start with the root module then compile the rest. Checking to ensure that each call and variable exists.

use super::operation::Operation;
use super::Backend;
use crate::ast::StatementNode;
use crate::pre_processor::ProcessedModule;

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Compiler {
    modules: HashMap<String, ProcessedModule>,
}

impl Compiler {
    pub fn new(modules: HashMap<String, ProcessedModule>) -> Self {
        return Self { modules };
    }

    pub fn into_operations(mut self) -> Vec<Operation> {
        todo!();
    }

    pub fn compile<B: Backend>(mut self, backend: B) -> Vec<u8> {
        todo!();
    }

    fn process_module(&mut self, module: ProcessedModule) {
        for (method_name, root_statement) in module.functions {
            //     self.append_operation(Operation::DefineFunction(method_name.clone()));

            //     match &*root_statement.borrow() {
            //         StatementNode::FunctionStatement(
            //             _keyword,
            //             _name,
            //             _arguments,
            //             _return_type,
            //             _body,
            //         ) => todo!(),
            //         _ => todo!(),
            //     }
        }
    }
}
