use super::{Expression, Type};
use crate::lexer::token::Token;
use crate::pre_processor::PreProcessorCommand;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Statement = Rc<RefCell<StatementNode>>;

struct_enum_with_functional_inits! {
    pub
    [Debug, Clone, PartialEq]
    StatementNode {
        ExpressionStatement {
            expr: Expression
        }
        ReturnStatement {
            keyword: Token,
            value: Option<Expression>
        }
        VariableDeclarationStatement {
            keyword: Token,
            name: Token,
            initializer: Expression
        }
        BlockStatement {
            open_brace: Token,
            body: Vec<Statement>
        }
        WhileStatement {
            keyword: Token,
            condition: Expression,
            body: Vec<Statement>
        }
        ForStatement {
            keyword: Token,
            variable_name: Token,
            start: Expression,
            stop: Expression,
            step: Expression,
            body: Vec<Statement>
        }
        IfStatement {
            keyword: Token,
            condition: Expression,
            body: Vec<Statement>,
            else_body: Option<Vec<Statement>>
        }
        FunctionStatement {
            keyword: Token,
            name: Token,
            arguments: HashMap<String, Type>,
            return_type: Option<Type>,
            body: Vec<Statement>
        }
        StructStatement {
            keyword: Token,
            name: Token,
            fields: HashMap<String, Type>
        }
        PreProcessorCommandStatement {
            symbol: Token,
            command: PreProcessorCommand
        }
    }
}

#[inline]
pub(crate) fn new_statement(node: StatementNode) -> Statement {
    return Rc::new(RefCell::new(node));
}

impl StatementNode {
    #[inline]
    pub fn borrow_preprocessor_command(&self) -> &PreProcessorCommand {
        return match self {
            StatementNode::PreProcessorCommandStatement { symbol: _, command } => command,
            _ => panic!("Not a pre processor command statement"),
        };
    }

    #[inline]
    pub fn borrow_function_name(&self) -> &Token {
        return match self {
            StatementNode::FunctionStatement {
                keyword: _,
                name,
                arguments: _,
                return_type: _,
                body: _,
            } => name,
            _ => panic!("Not a function definition statement"),
        };
    }
}
