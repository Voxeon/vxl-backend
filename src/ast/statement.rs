use super::{Expression, Type};
use crate::pre_processor::PreProcessorCommand;
use crate::Token;
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
            arguments: Vec<(Token, Type)>,
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

impl StatementNode {
    #[inline]
    pub fn wrapped(self) -> Statement {
        return Rc::new(RefCell::new(self));
    }

    #[inline]
    pub fn borrow_preprocessor_command(&self) -> &PreProcessorCommand {
        return match self {
            StatementNode::PreProcessorCommandStatement { symbol: _, command } => command,
            _ => internal_error!("Not a pre processor command statement"),
        };
    }

    pub fn reference_token(&self) -> Token {
        return match self {
            StatementNode::ExpressionStatement { expr } => expr.borrow().reference_token().clone(),
            StatementNode::ReturnStatement { keyword, value: _ } => keyword.clone(),
            StatementNode::VariableDeclarationStatement {
                keyword,
                name: _,
                initializer: _,
            } => keyword.clone(),
            StatementNode::BlockStatement {
                open_brace,
                body: _,
            } => open_brace.clone(),
            StatementNode::WhileStatement {
                keyword,
                condition: _,
                body: _,
            } => keyword.clone(),
            StatementNode::ForStatement {
                keyword,
                variable_name: _,
                start: _,
                stop: _,
                step: _,
                body: _,
            } => keyword.clone(),
            StatementNode::IfStatement {
                keyword,
                condition: _,
                body: _,
                else_body: _,
            } => keyword.clone(),
            StatementNode::FunctionStatement {
                keyword,
                name: _,
                arguments: _,
                return_type: _,
                body: _,
            } => keyword.clone(),
            StatementNode::StructStatement {
                keyword,
                name: _,
                fields: _,
            } => keyword.clone(),
            StatementNode::PreProcessorCommandStatement { symbol, command: _ } => symbol.clone(),
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
            _ => internal_error!("Not a function definition statement"),
        };
    }
}
