use super::value::{ArrayLiteral, Value};
use super::Variable;
use crate::lexer::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub type Expression = Rc<RefCell<ExpressionNode>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    ArrayAllocationExpression(ArrayLiteral, Token), // Array type, count
    LiteralExpression(Value),
    UnaryExpression(Token, Expression),
    BinaryExpression(Expression, Token, Expression),
    LogicalExpression(Expression, Token, Expression),
    VariableExpression(Token, Variable),
    AssignmentExpression(Expression, Token, Expression),
    GroupingExpression(Token, Expression),
    CallExpression(Token, Option<Token>, Token, Vec<Expression>), // keyword, module, name, arguments
    ConstructorCallExpression(Token, Option<Token>, Vec<Expression>), // struct, module, arguments
}

#[inline]
pub(crate) fn new_expression(node: ExpressionNode) -> Expression {
    return Rc::new(RefCell::new(node));
}

impl ExpressionNode {
    pub fn is_assignable_expression(&self) -> bool {
        return match self {
            Self::VariableExpression(_, _) => true,
            _ => false,
        };
    }
}
