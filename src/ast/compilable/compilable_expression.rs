use std::cell::RefCell;
use std::collections::HashMap;
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
        DivideBinaryExpression {
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
        VariableAssignmentExpression {
            lhs: Variable,
            rhs: CompilableExpression
        }
        ArrayIndexAssignmentExpression {
            lhs: CompilableExpression,
            rhs: CompilableExpression
        }
        GroupingExpression {
            expression: CompilableExpression,
            tp: Option<Type>
        }
        CallExpression {
            name: String,
            arguments: Vec<CompilableExpression>,
            resulting_type: Option<Type>
        }
        ConstructorCallExpression {
            target_struct: String,
            arguments: HashMap<String, CompilableExpression>
        }
        ArrayIndexExpression {
            array_expression: CompilableExpression,
            index_expression: CompilableExpression,
            tp: Type
        }
    }
}

impl CompilableExpressionNode {
    pub fn wrapped(self) -> CompilableExpression {
        return Rc::new(RefCell::new(self));
    }
}
