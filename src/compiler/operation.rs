use crate::parser::ast::Value;

#[derive(Clone, PartialEq, Debug)]
pub enum Operation {
    DefineVariable {
        value: Option<Value>,
        variable_name: String,
    },
    StoreVariable {
        variable_name: String,
        register: Register,
    },
    LoadVariable {
        variable_name: String,
        register: Register,
    },
    LoadValue {
        value: Value,
        register: Register,
    },
    DereferenceStructField {
        field_name: String,
        struct_name: String,
        pointer_register: Register,
    },
    AllocateNewStruct {
        struct_name: String,
        register: Register,
    },
    AllocateNewArray {
        elements: usize,
        element_size: usize,
        register: Register,
    },
    FreePtr {
        register: Register,
    },
    CallFunction {
        function_name: String,
        arguments: Vec<Value>,
    },
    LoadArrayElement {
        index: usize,
        element_size: usize,
        register: Register,
    },
    BitwiseNot {
        register: Register,
    },
    BitwiseAnd {
        lhs: Register,
        rhs: Register,
    },
    BitwiseOr {
        lhs: Register,
        rhs: Register,
    },
    BitwiseXor {
        lhs: Register,
        rhs: Register,
    },
    BooleanLogic(BooleanLogicOperation),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    /// RBX is a preserved register.
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,

    R8,
    R9,
    R10,
    R11,
    R12,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum BooleanLogicOperation {
    LogicalAnd {
        lhs: Box<BooleanLogicOperation>,
        rhs: Box<BooleanLogicOperation>,
    },
    LogicalOr {
        lhs: Box<BooleanLogicOperation>,
        rhs: Box<BooleanLogicOperation>,
    },
    LogicalComparison(ComparisonOperation),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ComparisonOperation {
    Equal { lhs: Register, rhs: Register },
    NotEqual { lhs: Register, rhs: Register },
    LessThan { lhs: Register, rhs: Register },
    LessThanEqual { lhs: Register, rhs: Register },
    GreaterThan { lhs: Register, rhs: Register },
    GreaterThanEqual { lhs: Register, rhs: Register },
}
