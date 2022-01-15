use super::value::{ArrayLiteral, Value};
use super::Variable;
use crate::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Expression = Rc<RefCell<ExpressionNode>>;

struct_enum_with_functional_inits! {
    pub
    [Debug, Clone, PartialEq]
    ExpressionNode {
        ArrayAllocationExpression {
            reference_token: Token,
            array_type : ArrayLiteral,
            count : Expression
        }
        ArrayIndexExpression {
            open_brace_token: Token,
            array_expression: Expression,
            index_expression: Expression
        }
        LiteralExpression {
            reference_token: Token,
            value: Value
        }
        UnaryExpression {
            operator: Token,
            rhs: Expression
        }
        BinaryExpression {
            lhs: Expression,
            operator: Token,
            rhs: Expression
        }
        LogicalExpression {
            lhs: Expression,
            operator: Token,
            rhs: Expression
        }
        VariableExpression {
            keyword: Token,
            variable: Variable
        }
        AssignmentExpression {
            lhs: Expression,
            operator: Token,
            rhs: Expression
        }
        GroupingExpression {
            reference_token: Token,
            expression: Expression
        }
        CallExpression {
            keyword: Token,
            module: Option<Token>,
            name: Token,
            arguments: Vec<Expression>
        }
        ConstructorCallExpression {
            target_struct: Token,
            module: Option<Token>,
            arguments: HashMap<Token, Expression>
        }
    }
}

impl ExpressionNode {
    #[inline]
    pub fn wrapped(self) -> Expression {
        return Rc::new(RefCell::new(self));
    }

    pub fn reference_token(&self) -> &Token {
        return match self {
            ExpressionNode::ArrayAllocationExpression {
                reference_token,
                array_type: _,
                count: _,
            } => reference_token,
            ExpressionNode::ArrayIndexExpression {
                open_brace_token,
                array_expression: _,
                index_expression: _,
            } => open_brace_token,
            ExpressionNode::LiteralExpression {
                reference_token,
                value: _,
            } => reference_token,
            ExpressionNode::UnaryExpression { operator, rhs: _ } => operator,
            ExpressionNode::BinaryExpression {
                lhs: _,
                operator,
                rhs: _,
            } => operator,
            ExpressionNode::LogicalExpression {
                lhs: _,
                operator,
                rhs: _,
            } => operator,
            ExpressionNode::VariableExpression {
                keyword: _,
                variable: _,
            } => todo!(),
            ExpressionNode::AssignmentExpression {
                lhs: _,
                operator,
                rhs: _,
            } => operator,
            ExpressionNode::GroupingExpression {
                reference_token,
                expression: _,
            } => reference_token,
            ExpressionNode::CallExpression {
                keyword,
                module: _,
                name: _,
                arguments: _,
            } => keyword,
            ExpressionNode::ConstructorCallExpression {
                target_struct,
                module: _,
                arguments: _,
            } => target_struct,
        };
    }
    pub fn is_assignable_expression(&self) -> bool {
        return self.is_variable_expression() || self.is_array_index_expression();
    }
}
