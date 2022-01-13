use std::collections::HashMap;

use crate::ast::{Statement, Type};
use crate::std_library::StandardLibrary;
use crate::Token;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct ObjectName {
    module: Token,
    name: Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDefinition {
    name: String,
    fields: HashMap<String, Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreProcessedModule {
    name: String,
    imports: Vec<ObjectName>,
    functions: HashMap<String, Statement>,
    structs: HashMap<String, StructDefinition>,
}

pub enum ProcessedModule {
    PreProcessedModule(PreProcessedModule),
    StandardLibrary(Box<dyn StandardLibrary>),
}

impl ObjectName {
    pub fn new(name: Token, module: Token) -> Self {
        return Self { name, module };
    }

    pub fn name(&self) -> &Token {
        return &self.name;
    }

    pub fn module(&self) -> &Token {
        return &self.module;
    }
}

impl StructDefinition {
    pub fn new(name: String, fields: HashMap<String, Type>) -> Self {
        return Self { name, fields };
    }

    pub fn name(&self) -> &String {
        return &self.name;
    }

    pub fn fields(&self) -> &HashMap<String, Type> {
        return &self.fields;
    }
}

impl PreProcessedModule {
    pub fn new(
        name: String,
        imports: Vec<ObjectName>,
        functions: HashMap<String, Statement>,
        structs: HashMap<String, StructDefinition>,
    ) -> Self {
        return Self {
            name,
            imports,
            functions,
            structs,
        };
    }

    pub fn name(&self) -> &String {
        return &self.name;
    }

    pub fn imports(&self) -> &Vec<ObjectName> {
        return &self.imports;
    }

    pub fn functions(&self) -> &HashMap<String, Statement> {
        return &self.functions;
    }

    pub fn structs(&self) -> &HashMap<String, StructDefinition> {
        return &self.structs;
    }
}

impl Into<ProcessedModule> for PreProcessedModule {
    fn into(self) -> ProcessedModule {
        return ProcessedModule::PreProcessedModule(self);
    }
}

impl ProcessedModule {
    pub fn name(&self) -> &String {
        return match self {
            ProcessedModule::PreProcessedModule(m) => m.name(),
            ProcessedModule::StandardLibrary(s) => s.name(),
        };
    }
    pub fn functions(&self) -> &HashMap<String, Statement> {
        return match self {
            ProcessedModule::PreProcessedModule(m) => m.functions(),
            ProcessedModule::StandardLibrary(s) => s.functions(),
        };
    }

    pub fn structs(&self) -> &HashMap<String, StructDefinition> {
        return match self {
            ProcessedModule::PreProcessedModule(m) => m.structs(),
            ProcessedModule::StandardLibrary(s) => s.structs(),
        };
    }

    /// This function will panic if you try to take the imports form an STL module.
    pub fn unwrap_imports(&self) -> &Vec<ObjectName> {
        return match self {
            ProcessedModule::PreProcessedModule(m) => m.imports(),
            ProcessedModule::StandardLibrary(_) => {
                panic!(
                    "Unexpectedly found an STL module when trying to unwrap a pre-processed module."
                )
            }
        };
    }

    pub fn is_stl_module(&self) -> bool {
        return match self {
            ProcessedModule::PreProcessedModule(_) => false,
            ProcessedModule::StandardLibrary(_) => true,
        };
    }

    pub fn unwrap_pre_processed_ref(&self) -> &PreProcessedModule {
        return match self {
            ProcessedModule::PreProcessedModule(m) => m,
            ProcessedModule::StandardLibrary(_) => panic!(
                "Unexpectedly found an STL module when trying to unwrap a pre-processed module."
            ),
        };
    }

    pub fn unwrap_pre_processed(self) -> PreProcessedModule {
        return match self {
            ProcessedModule::PreProcessedModule(m) => m,
            ProcessedModule::StandardLibrary(_) => panic!(
                "Unexpectedly found an STL module when trying to unwrap a pre-processed module."
            ),
        };
    }
}
