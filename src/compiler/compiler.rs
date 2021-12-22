// Start with the root module then compile the rest. Checking to ensure that each call and variable exists.

use super::operation::Operation;
use super::Backend;
use crate::parser::ast::StatementNode;
use crate::pre_processor::CompilableModule;

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Compiler {
    modules: HashMap<String, CompilableModule>,
}

impl Compiler {
    pub fn new(modules: HashMap<String, CompilableModule>) -> Self {
        return Self { modules };
    }

    pub fn into_operations(mut self) -> Vec<Operation> {
        todo!();
    }

    pub fn compile<B: Backend>(mut self, backend: B) -> Vec<u8> {
        todo!();
    }

    fn process_module(&mut self, module: CompilableModule) {
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
