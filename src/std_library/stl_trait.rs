use std::collections::HashMap;
use std::fmt::Debug;

use crate::ast::compilable::{CompilableFunction, CompilableStruct};
use crate::ast::Statement;
use crate::processed_module::StructDefinition;

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum TargetOS {
    VoxeonOS,
    Unix,
    Windows,
    None,
}

pub trait StandardLibrary {
    fn new() -> Self
    where
        Self: Sized;

    fn target() -> TargetOS
    where
        Self: Sized;

    fn name(&self) -> &String;

    /// This method will return the compilable functions and structs for this STL,
    /// this method can only be called once and after being called the object should be considered invalid.
    fn take_compilable_objects(&self) -> (Vec<CompilableFunction>, Vec<CompilableStruct>);

    fn functions(&self) -> &HashMap<String, Statement>;

    fn structs(&self) -> &HashMap<String, StructDefinition>;
}
