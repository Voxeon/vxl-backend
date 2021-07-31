use super::{Expression, Type};
use crate::lexer::token::Token;
use crate::pre_processor::PreProcessorCommand;
use std::cell::RefCell;
use std::rc::Rc;

pub type Statement = Rc<RefCell<StatementNode>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub tp: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementNode {
    ExpressionStatement(Expression),
    ReturnStatement(Token, Option<Expression>),
    VariableDeclarationStatement(Token, Token, Expression), // symbol, name, initializer
    BlockStatement(Token, Vec<Statement>),
    WhileStatement(Token, Expression, Vec<Statement>),
    ForStatement(
        Token,
        Token,
        Expression,
        Expression,
        Expression,
        Vec<Statement>,
    ), // symbol, variable name, start, stop, step, body
    IfStatement(Token, Expression, Vec<Statement>, Option<Vec<Statement>>),
    FunctionStatement(Token, Token, Vec<Field>, Option<Type>, Vec<Statement>), // keyword, name, arguments, return type, body
    StructStatement(Token, Token, Vec<Field>),
    PreProcessorCommandStatement(Token, PreProcessorCommand),
}

#[inline]
pub(crate) fn new_statement(node: StatementNode) -> Statement {
    return Rc::new(RefCell::new(node));
}

impl StatementNode {
    #[inline]
    pub fn is_function(&self) -> bool {
        return match self {
            StatementNode::FunctionStatement(_, _, _, _, _) => true,
            _ => false,
        };
    }

    #[inline]
    pub fn is_struct(&self) -> bool {
        return match self {
            StatementNode::StructStatement(_, _, _) => true,
            _ => false,
        };
    }

    #[inline]
    pub fn is_preprocessor(&self) -> bool {
        return match self {
            StatementNode::PreProcessorCommandStatement(_, _) => true,
            _ => false,
        };
    }

    #[inline]
    pub fn borrow_preprocessor_command(&self) -> &PreProcessorCommand {
        return match self {
            StatementNode::PreProcessorCommandStatement(_, cmd) => cmd,
            _ => panic!("Not a pre processor command statement"),
        };
    }

    #[inline]
    pub fn borrow_function_name(&self) -> &Token {
        return match self {
            StatementNode::FunctionStatement(_, name, _, _, _) => name,
            _ => panic!("Not a function definition statement"),
        };
    }
}
