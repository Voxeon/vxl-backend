use std::collections::HashMap;

use crate::ast::*;
use crate::error::ParserError;
use crate::lexer::token::{Token, TokenType};
use crate::pre_processor::PreProcessorCommand;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
    tree: AST,
}

type ParserResult<T> = Result<T, ParserError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Self {
            tokens,
            current_index: 0,
            tree: AST::new(),
        };
    }

    pub fn parse(mut self) -> ParserResult<AST> {
        loop {
            while self.matches(TokenType::NewLineToken) {
                self.consume_token_one([TokenType::NewLineToken])?;
            }

            if self.is_at_end() {
                break;
            }

            let statement = self.parse_top_level_statement()?;
            self.tree.push_statement(statement);
        }

        return Ok(self.tree);
    }

    fn parse_top_level_statement(&mut self) -> ParserResult<Statement> {
        // Function and struct declarations that should be at the file level.

        if self.matches(TokenType::FunctionToken) {
            return self.parse_function_declaration();
        } else if self.matches(TokenType::StructToken) {
            return self.parse_struct_declaration();
        } else if self.matches(TokenType::PercentToken) {
            return self.parse_preprocessor_command();
        } else {
            if self.is_at_end() {
                return Err(ParserError::ExpectedFoundEOF(
                    "top level statement (func or struct)".to_string(),
                ));
            } else {
                return Err(ParserError::ExpectedFound(
                    "top level statement (func or struct)".to_string(),
                    self.current_token().unwrap().lexeme().clone(),
                ));
            }
        }
    }

    fn parse_preprocessor_command(&mut self) -> ParserResult<Statement> {
        self.consume_token_one([TokenType::PercentToken])?;
        let identifier = self.consume_token_one([TokenType::IdentifierToken])?;

        let cmd = match identifier.lexeme().as_str() {
            "import" => self.parse_preprocessor_import()?,
            "begin" => self.parse_preprocessor_begin_module()?,
            _ => return Err(ParserError::InvalidPreProcessorCommand(identifier.clone())),
        };

        return Ok(new_statement(
            StatementNode::pre_processor_command_statement(identifier, cmd),
        ));
    }

    fn parse_preprocessor_import(&mut self) -> ParserResult<PreProcessorCommand> {
        let mut imports = Vec::new();

        loop {
            let import = self.consume_token_one([TokenType::IdentifierToken])?;

            if import.lexeme() != "from" {
                imports.push(import);

                if !self.is_at_end() && self.current_token().unwrap().lexeme() != "from" {
                    self.consume_token_one([TokenType::CommaToken])?;
                }
            } else {
                break;
            }
        }

        let module_name = self.consume_token_one([TokenType::IdentifierToken])?;
        self.consume_token_one([TokenType::NewLineToken])?;

        return Ok(PreProcessorCommand::ImportCommand(module_name, imports));
    }

    fn parse_preprocessor_begin_module(&mut self) -> ParserResult<PreProcessorCommand> {
        let module_name = self.consume_token_one([TokenType::IdentifierToken])?;
        self.consume_token_one([TokenType::NewLineToken])?;

        return Ok(PreProcessorCommand::BeginModuleCommand(module_name));
    }

    fn parse_function_declaration(&mut self) -> ParserResult<Statement> {
        let keyword = self.consume_token_one([TokenType::FunctionToken])?;
        let name = self.consume_token_one([TokenType::IdentifierToken])?;
        let mut args = HashMap::new();
        let mut return_type = None;

        if self.matches(TokenType::OpenRoundBraceToken) {
            self.consume_token_one([TokenType::OpenRoundBraceToken])?;

            if !self.matches(TokenType::CloseRoundBraceToken) {
                loop {
                    let (name, tp) = self.parse_field()?;
                    args.insert(name, tp);

                    if !self.matches(TokenType::CommaToken) {
                        break;
                    } else {
                        self.consume_token_one([TokenType::CommaToken])?;
                    }
                }
            }

            self.consume_token_one([TokenType::CloseRoundBraceToken])?;
        }

        if self.matches(TokenType::RightArrowToken) {
            self.consume_token_one([TokenType::RightArrowToken])?;

            return_type = Some(self.parse_type()?);
        }

        self.consume_token_one([TokenType::NewLineToken])?;

        let body = self.parse_block(&keyword)?;
        self.parse_end(TokenType::FunctionToken)?;

        return Ok(new_statement(StatementNode::function_statement(
            keyword,
            name,
            args,
            return_type,
            body,
        )));
    }

    fn parse_struct_declaration(&mut self) -> ParserResult<Statement> {
        let keyword = self.consume_token_one([TokenType::StructToken])?;
        let name = self.consume_token_one([TokenType::IdentifierToken])?;
        self.consume_token_one([TokenType::NewLineToken])?;

        let mut fields = HashMap::new();

        while !self.matches(TokenType::EndToken) {
            let (field_name, field_tp) = self.parse_field()?;

            if fields.insert(field_name.clone(), field_tp).is_some() {
                return Err(ParserError::FieldAlreadyDefinedForStruct(field_name, name));
            }

            self.consume_token_one([TokenType::NewLineToken])?;
        }

        self.parse_end(TokenType::StructToken)?;

        return Ok(new_statement(StatementNode::struct_statement(
            keyword, name, fields,
        )));
    }

    fn parse_type(&mut self) -> ParserResult<Type> {
        if self.matches(TokenType::OpenSquareBraceToken) {
            self.consume_token_one([TokenType::OpenSquareBraceToken])?;

            let array_type = self.parse_type()?;

            self.consume_token_one([TokenType::CloseSquareBraceToken])?;

            return Ok(Type::Array(Box::new(array_type)));
        } else if !self.matches(TokenType::IdentifierToken) {
            return Err(self.expected_found_error("type".to_string()));
        }

        let name = self.consume_token_one([TokenType::IdentifierToken])?;

        let tp = match name.lexeme().as_str() {
            "int" | "integer" => Type::Integer,
            "char" | "character" => Type::Character,
            "float" => Type::Float,
            "bool" | "boolean" => Type::Boolean,
            _ => {
                // consume module
                // <
                self.consume_token_one([TokenType::LessThanToken])?;
                // module name
                let module = self.consume_token_one([TokenType::IdentifierToken])?;
                // >
                self.consume_token_one([TokenType::GreaterThanToken])?;

                Type::Struct {
                    name: name.lexeme().clone(),
                    module: module.lexeme().clone(),
                }
            }
        };

        return Ok(tp);
    }

    fn parse_field(&mut self) -> ParserResult<(String, Type)> {
        let name = self.consume_token_one([TokenType::IdentifierToken])?;

        self.consume_token_one([TokenType::OpenRoundBraceToken])?;
        let tp = self.parse_type()?;

        self.consume_token_one([TokenType::CloseRoundBraceToken])?;

        return Ok((name.lexeme().clone(), tp));
    }

    fn parse_statement(&mut self) -> ParserResult<Statement> {
        if self.matches(TokenType::DollarToken) {
            return self.parse_variable_declaration();
        }

        return self.parse_secondary_statement();
    }

    fn parse_variable_declaration(&mut self) -> ParserResult<Statement> {
        let symbol = self.consume_token_one([TokenType::DollarToken])?;
        let name = self.consume_token_one([TokenType::IdentifierToken])?;
        self.consume_token_one([TokenType::LeftArrowToken])?;

        let initializer = self.parse_expression()?;
        self.consume_token_one([TokenType::NewLineToken])?;

        return Ok(new_statement(
            StatementNode::variable_declaration_statement(symbol, name, initializer),
        ));
    }

    fn parse_secondary_statement(&mut self) -> ParserResult<Statement> {
        if self.matches(TokenType::BlockToken) {
            let tok = self.consume_token_one([TokenType::BlockToken])?;

            let stmts = self.parse_block(&tok)?;
            self.parse_end(TokenType::BlockToken)?;

            return Ok(new_statement(StatementNode::block_statement(tok, stmts)));
        }

        if self.matches(TokenType::IfToken) {
            let tok = self.consume_token_one([TokenType::IfToken])?;

            let cond = self.parse_expression()?;
            self.consume_token_one([TokenType::NewLineToken])?;

            let then_branch =
                self.parse_block_with_break(&tok, [TokenType::EndToken, TokenType::ElseToken])?;

            let mut else_branch = None;

            if self.matches(TokenType::ElseToken) {
                self.consume_token_one([TokenType::ElseToken])?;
                self.consume_token_one([TokenType::NewLineToken])?;

                else_branch = Some(self.parse_block(&tok)?);
            }

            self.parse_end(TokenType::IfToken)?;
            return Ok(new_statement(StatementNode::if_statement(
                tok,
                cond,
                then_branch,
                else_branch,
            )));
        }

        if self.matches(TokenType::WhileToken) {
            let tok = self.consume_token_one([TokenType::WhileToken])?;

            let cond = self.parse_expression()?;
            self.consume_token_one([TokenType::NewLineToken])?;

            let body = self.parse_block(&tok)?;

            self.parse_end(TokenType::WhileToken)?;
            return Ok(new_statement(StatementNode::while_statement(
                tok, cond, body,
            )));
        }

        if self.matches(TokenType::ForToken) {
            let tok = self.consume_token_one([TokenType::ForToken])?;
            let iter_name = self.consume_token_one([TokenType::IdentifierToken])?;

            self.consume_token_one([TokenType::ColonToken])?;
            self.consume_token_one([TokenType::OpenSquareBraceToken])?;
            let start = self.parse_expression()?;
            self.consume_token_one([TokenType::SemiColonToken])?;
            let stop = self.parse_expression()?;
            self.consume_token_one([TokenType::SemiColonToken])?;
            let step = self.parse_expression()?;
            self.consume_token_one([TokenType::CloseSquareBraceToken])?;
            self.consume_token_one([TokenType::NewLineToken])?;

            let body = self.parse_block(&tok)?;
            self.parse_end(TokenType::ForToken)?;
            return Ok(new_statement(StatementNode::for_statement(
                tok, iter_name, start, stop, step, body,
            )));
        }

        if self.matches(TokenType::ReturnToken) {
            let tok = self.consume_token_one([TokenType::ReturnToken])?;

            let mut value = None;

            if !self.matches(TokenType::NewLineToken) {
                value = Some(self.parse_expression()?);
            }

            self.consume_token_one([TokenType::NewLineToken])?;

            return Ok(new_statement(StatementNode::return_statement(tok, value)));
        }

        return self.parse_expression_statement();
    }

    fn parse_block(&mut self, block_token: &Token) -> ParserResult<Vec<Statement>> {
        return self.parse_block_with_break(block_token, [TokenType::EndToken]);
    }

    fn parse_block_with_break<const N: usize>(
        &mut self,
        block_token: &Token,
        break_types: [TokenType; N],
    ) -> ParserResult<Vec<Statement>> {
        let mut statements = Vec::new();

        loop {
            while self.matches(TokenType::NewLineToken) {
                self.consume_token_one([TokenType::NewLineToken])?;
            }

            if self.matches_one(break_types) {
                break;
            } else if self.is_at_end() {
                return Err(ParserError::UnterminatedBlock(block_token.clone()));
            }

            statements.push(self.parse_statement()?);
        }

        return Ok(statements);
    }

    fn parse_end(&mut self, expected_type: TokenType) -> ParserResult<()> {
        self.consume_token_one([TokenType::EndToken])?;
        self.consume_token_one([TokenType::OpenRoundBraceToken])?;
        self.consume_token_one([expected_type])?;
        self.consume_token_one([TokenType::CloseRoundBraceToken])?;

        return Ok(());
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Statement> {
        let expression = self.parse_expression()?;
        self.consume_token_one([TokenType::NewLineToken])?;

        return Ok(new_statement(StatementNode::expression_statement(
            expression,
        )));
    }

    fn parse_expression(&mut self) -> ParserResult<Expression> {
        return self.parse_assignment();
    }

    fn parse_assignment(&mut self) -> ParserResult<Expression> {
        let mut expr = self.parse_logical_or()?;

        if self.matches(TokenType::LeftArrowToken) {
            let arrow = self.consume_token_one([TokenType::LeftArrowToken])?;
            let value = self.parse_assignment()?;

            if expr.borrow().is_assignable_expression() {
                expr = new_expression(ExpressionNode::assignment_expression(expr, arrow, value));
            } else {
                return Err(ParserError::InvalidAssignmentTarget(arrow));
            }
        }

        return Ok(expr);
    }

    fn parse_logical_or(&mut self) -> ParserResult<Expression> {
        let mut lhs = self.parse_logical_and()?;

        if self.matches(TokenType::OrToken) {
            let op = self.consume_token_one([TokenType::OrToken])?;
            let rhs = self.parse_logical_or()?;

            lhs = new_expression(ExpressionNode::logical_expression(lhs, op, rhs));
        }

        return Ok(lhs);
    }

    fn parse_logical_and(&mut self) -> ParserResult<Expression> {
        let mut lhs = self.parse_equality()?;

        if self.matches(TokenType::AndToken) {
            let op = self.consume_token_one([TokenType::AndToken])?;
            let rhs = self.parse_logical_or()?;

            lhs = new_expression(ExpressionNode::logical_expression(lhs, op, rhs));
        }

        return Ok(lhs);
    }

    fn parse_equality(&mut self) -> ParserResult<Expression> {
        let mut lhs = self.parse_comparison()?;

        if self.matches_one([TokenType::BangEqualsToken, TokenType::EqualsToken]) {
            let tok =
                self.consume_token_one([TokenType::BangEqualsToken, TokenType::EqualsToken])?;

            let rhs = self.parse_comparison()?;
            lhs = new_expression(ExpressionNode::binary_expression(lhs, tok, rhs));
        }

        return Ok(lhs);
    }

    fn parse_comparison(&mut self) -> ParserResult<Expression> {
        let mut lhs = self.parse_term()?;

        if self.matches_one([
            TokenType::GreaterThanToken,
            TokenType::GreaterThanEqualToken,
            TokenType::LessThanToken,
            TokenType::LessThanEqualToken,
        ]) {
            let tok = self.consume_token_one([
                TokenType::GreaterThanToken,
                TokenType::GreaterThanEqualToken,
                TokenType::LessThanToken,
                TokenType::LessThanEqualToken,
            ])?;

            let rhs = self.parse_term()?;
            lhs = new_expression(ExpressionNode::binary_expression(lhs, tok, rhs));
        }

        return Ok(lhs);
    }

    fn parse_term(&mut self) -> ParserResult<Expression> {
        let mut lhs = self.parse_factor()?;

        while self.matches_one([TokenType::PlusToken, TokenType::MinusToken]) {
            let tok = self.consume_token_one([TokenType::PlusToken, TokenType::MinusToken])?;
            let rhs = self.parse_factor()?;

            lhs = new_expression(ExpressionNode::binary_expression(lhs, tok, rhs));
        }

        return Ok(lhs);
    }

    fn parse_factor(&mut self) -> ParserResult<Expression> {
        let mut lhs = self.parse_unary()?;

        while self.matches_one([TokenType::StarToken, TokenType::BackslashToken]) {
            let tok = self.consume_token_one([TokenType::StarToken, TokenType::BackslashToken])?;
            let rhs = self.parse_unary()?;

            lhs = new_expression(ExpressionNode::binary_expression(lhs, tok, rhs));
        }

        return Ok(lhs);
    }

    fn parse_unary(&mut self) -> ParserResult<Expression> {
        if self.matches_one([TokenType::NotToken, TokenType::MinusToken]) {
            let tok = self.consume_token_one([TokenType::NotToken, TokenType::MinusToken])?;
            let rhs = self.parse_variable()?;

            return Ok(new_expression(ExpressionNode::unary_expression(tok, rhs)));
        }

        return self.parse_variable();
    }

    fn parse_variable(&mut self) -> ParserResult<Expression> {
        if self.matches(TokenType::DollarToken) {
            let indicator = self.consume_token_one([TokenType::DollarToken])?;
            let mut var = Variable::new();

            var.push(self.consume_token_one([TokenType::IdentifierToken])?);

            while self.matches(TokenType::PeriodToken) {
                self.consume_token_one([TokenType::PeriodToken]).unwrap();
                var.push(self.consume_token_one([TokenType::IdentifierToken])?);
            }

            return Ok(new_expression(ExpressionNode::variable_expression(
                indicator, var,
            )));
        }

        return self.parse_function_call();
    }

    fn parse_function_call(&mut self) -> ParserResult<Expression> {
        if self.matches(TokenType::AtToken) {
            let indicator = self.consume_token_one([TokenType::AtToken])?;

            let module_name = if self.matches(TokenType::LessThanToken) {
                self.consume_token_one([TokenType::LessThanToken])?;
                let module_name = self.consume_token_one([TokenType::IdentifierToken])?;
                self.consume_token_one([TokenType::GreaterThanToken])?;

                Some(module_name)
            } else {
                None
            };

            let function_name = self.consume_token_one([TokenType::IdentifierToken])?;
            let mut arguments = Vec::new();

            if self.matches(TokenType::OpenRoundBraceToken) {
                let open_brace = self
                    .consume_token_one([TokenType::OpenRoundBraceToken])
                    .unwrap();

                if !self.matches(TokenType::CloseRoundBraceToken) {
                    loop {
                        arguments.push(self.parse_expression()?);

                        if !self.matches(TokenType::CloseRoundBraceToken) {
                            self.consume_token_one([TokenType::CommaToken])?;
                        } else {
                            break;
                        }
                    }
                }

                if self.is_at_end() {
                    return Err(ParserError::ExpectedFoundEOF(")".to_string()));
                } else {
                    self.consume_token_one([TokenType::CloseRoundBraceToken])
                        .unwrap();
                }

                if arguments.len() > 255 {
                    return Err(ParserError::TooManyFunctionArguments(open_brace));
                }
            }

            return Ok(new_expression(ExpressionNode::call_expression(
                indicator,
                module_name,
                function_name,
                arguments,
            )));
        }

        return self.parse_constructor_call();
    }

    fn parse_constructor_call(&mut self) -> ParserResult<Expression> {
        if self.matches(TokenType::PipeToken) {
            self.consume_token_one([TokenType::PipeToken])?;

            let struct_name = self.consume_token_one([TokenType::IdentifierToken])?;

            let module_name = if self.matches(TokenType::LessThanToken) {
                self.consume_token_one([TokenType::LessThanToken])?;
                let module_name = self.consume_token_one([TokenType::IdentifierToken])?;
                self.consume_token_one([TokenType::GreaterThanToken])?;

                Some(module_name)
            } else {
                None
            };

            let mut arguments = Vec::new();

            while !self.matches(TokenType::PipeToken) {
                arguments.push(self.parse_expression()?);

                if !self.matches(TokenType::PipeToken) {
                    self.consume_token_one([TokenType::CommaToken])?;
                }
            }

            self.consume_token_one([TokenType::PipeToken])?;

            return Ok(new_expression(ExpressionNode::constructor_call_expression(
                struct_name,
                module_name,
                arguments,
            )));
        }

        return self.parse_primary();
    }

    fn parse_primary(&mut self) -> ParserResult<Expression> {
        if self.matches(TokenType::IntegerLiteralToken) {
            let tok = self.consume_token_one([TokenType::IntegerLiteralToken])?;
            let int: i64 = match tok.lexeme().parse() {
                Ok(i) => i,
                Err(_) => return Err(ParserError::InvalidIntegerLiteral(tok)),
            };

            return Ok(new_expression(ExpressionNode::literal_expression(
                Value::Integer(int),
            )));
        }

        if self.matches(TokenType::DoubleLiteralToken) {
            let tok = self.consume_token_one([TokenType::DoubleLiteralToken])?;
            let dbl: f64 = match tok.lexeme().parse() {
                Ok(i) => i,
                Err(_) => return Err(ParserError::InvalidDoubleLiteral(tok)),
            };

            return Ok(new_expression(ExpressionNode::literal_expression(
                Value::Float(dbl),
            )));
        }

        if self.matches(TokenType::CharacterLiteralToken) {
            let tok = self.consume_token_one([TokenType::CharacterLiteralToken])?;
            let ch = tok.lexeme().chars().next().unwrap();

            return Ok(new_expression(ExpressionNode::literal_expression(
                Value::Character(ch),
            )));
        }

        if self.matches(TokenType::StringLiteralToken) {
            let tok = self.consume_token_one([TokenType::StringLiteralToken])?;
            let array = ArrayLiteral::from(tok.lexeme());

            return Ok(new_expression(ExpressionNode::array_allocation_expression(
                array, tok,
            )));
        }

        if self.matches(TokenType::TrueToken) {
            self.consume_token_one([TokenType::TrueToken])?;

            return Ok(new_expression(ExpressionNode::literal_expression(
                Value::Boolean(true),
            )));
        }

        if self.matches(TokenType::FalseToken) {
            self.consume_token_one([TokenType::FalseToken])?;

            return Ok(new_expression(ExpressionNode::literal_expression(
                Value::Boolean(false),
            )));
        }

        if self.matches(TokenType::OpenRoundBraceToken) {
            let tok = self.consume_token_one([TokenType::OpenRoundBraceToken])?;

            let expr = self.parse_expression()?;
            self.consume_token_one([TokenType::CloseRoundBraceToken])?;

            return Ok(new_expression(ExpressionNode::grouping_expression(
                tok, expr,
            )));
        }

        return Err(ParserError::UnexpectedToken(
            self.current_token().unwrap().clone(),
        ));
    }

    fn matches(&self, tp: TokenType) -> bool {
        if let Some(tok) = self.current_token() {
            return tok.token_type() == tp;
        } else {
            return false;
        }
    }

    fn matches_one<const N: usize>(&self, tps: [TokenType; N]) -> bool {
        if let Some(tok) = self.current_token() {
            let tp = tok.token_type();

            for i in 0..N {
                if tp == tps[i] {
                    return true;
                }
            }
        }

        return false;
    }

    fn consume_token_one<const N: usize>(&mut self, tps: [TokenType; N]) -> ParserResult<Token> {
        if self.matches_one(tps) {
            let tok = self.current_token().unwrap().clone();
            self.current_index += 1;

            return Ok(tok);
        } else {
            let mut s = String::new();

            for i in 0..N {
                s += tps[i].to_string().as_str();

                if i != N - 1 {
                    s += ", ";
                }
            }

            if self.is_at_end() {
                return Err(ParserError::ExpectedFoundEOF(s));
            } else {
                return Err(ParserError::ExpectedFound(
                    s,
                    self.current_token().unwrap().lexeme().clone(),
                ));
            }
        }
    }

    fn current_token(&self) -> Option<&Token> {
        return self.tokens.get(self.current_index);
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        return self.current_index >= self.tokens.len();
    }

    #[inline]
    fn expected_found_error(&self, expected: String) -> ParserError {
        if self.is_at_end() {
            return ParserError::ExpectedFoundEOF(expected);
        } else {
            return ParserError::ExpectedFound(
                expected,
                self.current_token().unwrap().lexeme().clone(),
            );
        }
    }
}

#[cfg(test)]
mod tests;
