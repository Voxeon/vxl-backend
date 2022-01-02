use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{Type, Value, Variable};

pub type CompilableExpression = Rc<RefCell<CompilableExpressionNode>>;

struct_enum_with_functional_inits! {
    pub
    [Debug, Clone, PartialEq]
    CompilableExpressionNode {
        ArrayAllocationExpression {
            tp: Type,
            contents: Vec<Value>,
            count : CompilableExpression
        }
        LiteralExpression {
            value: Value
        }
        NotUnaryExpression {
            expr: CompilableExpression
        }
        NegateUnaryExpression {
            expr: CompilableExpression
        }
        AddBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        SubtractBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        MultiplyBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        EqualToBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        NotEqualToBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        GreaterBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        GreaterEqualBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        LessBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        LessEqualBinaryExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression,
            tp: Type
        }
        OrLogicalExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression
        }
        AndLogicalExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression
        }
        VariableExpression {
            variable: Variable
        }
        AssignmentExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression
        }
        GroupingExpression {
            expression: CompilableExpression,
            tp: Option<Type>
        }
        CallExpression {
            module: Option<String>,
            name: String,
            arguments: Vec<CompilableExpression>
        }
        ConstructorCallExpression {
            target_struct: String,
            module: Option<String>,
            arguments: Vec<CompilableExpression>
        }
    }
}

impl CompilableExpressionNode {
    pub fn wrapped(self) -> CompilableExpression {
        return Rc::new(RefCell::new(self));
    }
}
