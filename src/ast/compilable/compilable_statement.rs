use std::cell::RefCell;
use std::rc::Rc;

use super::CompilableExpression;
use crate::ast::compilable::CompilableBlock;
use crate::ast::Type;

pub type CompilableStatement = Rc<RefCell<CompilableStatementNode>>;

struct_enum_with_functional_inits! {
    pub
    [Debug, Clone, PartialEq]
    CompilableStatementNode {
        ExpressionStatement {
            expr: CompilableExpression,
            expression_type: Option<Type>
        }
        ReturnStatement {
            value: Option<CompilableExpression>,
            value_type: Option<Type>
        }
        VariableDeclarationStatement {
            name: String,
            initializer: CompilableExpression,
            initializer_type: Type
        }
        WhileStatement {
            // Condition must be guaranteed to be a boolean type in a compilable while statement
            condition: CompilableExpression,
            body: CompilableBlock
        }
        ForStatement {
            variable_name: String,
            // Guaranteed to all be integer types in a compilable node
            start: CompilableExpression,
            stop: CompilableExpression,
            step: CompilableExpression,
            body: CompilableBlock
        }
        IfStatement {
            // Condition must be guaranteed to be a boolean type in a compilable while statement
            condition: CompilableExpression,
            body: CompilableBlock,
            else_body: Option<CompilableBlock>
        }
        Block {
            block: CompilableBlock
        }
    }
}

impl CompilableStatementNode {
    pub fn wrapped(self) -> CompilableStatement {
        return Rc::new(RefCell::new(self));
    }

    pub fn always_returns(&self) -> bool {
        return match self {
            CompilableStatementNode::ReturnStatement {
                value: _,
                value_type: _,
            } => true,
            CompilableStatementNode::WhileStatement { condition: _, body } => {
                body.borrow().always_returns
            }
            CompilableStatementNode::ForStatement {
                variable_name: _,
                start: _,
                stop: _,
                step: _,
                body,
            } => body.borrow().always_returns,
            CompilableStatementNode::IfStatement {
                condition: _,
                body,
                else_body,
            } => {
                body.borrow().always_returns
                    && else_body.is_some()
                    && else_body.as_ref().unwrap().borrow().always_returns
            }
            CompilableStatementNode::Block { block } => block.borrow().always_returns,
            _ => false,
        };
    }
}
