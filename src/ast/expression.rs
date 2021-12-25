use super::value::{ArrayLiteral, Value};
use super::Variable;
use crate::lexer::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub type Expression = Rc<RefCell<ExpressionNode>>;

ast_enum! {
    pub
    [Debug, Clone, PartialEq],
    ExpressionNode {
        ArrayAllocationExpression {
            array_type : ArrayLiteral,
            count : Token
        }
        LiteralExpression {
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
            arguments: Vec<Expression>
        }
    }
}

#[inline]
pub(crate) fn new_expression(node: ExpressionNode) -> Expression {
    return Rc::new(RefCell::new(node));
}

impl ExpressionNode {
    pub fn is_assignable_expression(&self) -> bool {
        return self.is_variable_expression();
    }
}
