use std::collections::HashMap;

use super::ast::*;
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
        let mut args = Vec::new();
        let mut return_type = None;

        if self.matches(TokenType::OpenRoundBraceToken) {
            self.consume_token_one([TokenType::OpenRoundBraceToken])?;

            if !self.matches(TokenType::CloseRoundBraceToken) {
                loop {
                    args.push(self.parse_field()?);

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
            let field = self.parse_field()?;
            let field_name = field.name.clone();

            if fields.insert(field_name.clone(), field).is_some() {
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

    fn parse_field(&mut self) -> ParserResult<Field> {
        let name = self.consume_token_one([TokenType::IdentifierToken])?;

        self.consume_token_one([TokenType::OpenRoundBraceToken])?;
        let tp = self.parse_type()?;

        self.consume_token_one([TokenType::CloseRoundBraceToken])?;

        return Ok(Field {
            tp,
            name: name.lexeme().clone(),
        });
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
mod tests {
    use super::{
        new_expression, new_statement, ArrayLiteral, ExpressionNode, Field, Parser, ParserError,
        PreProcessorCommand, StatementNode, Token, TokenType, Type, Value, Variable, AST,
    };

    use std::collections::HashMap;

    macro_rules! hashmap {
        [$($k:expr ; $v:expr),*] => {
            vec![$(($k, $v)),*].into_iter().collect()
        };
    }

    #[inline]
    fn new_tokens<const N: usize>(contents: [(TokenType, &str); N]) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut line = 1;
        let mut col = 1;

        for (tp, lexeme) in std::array::IntoIter::new(contents) {
            tokens.push(Token::new(tp, lexeme.to_string(), line, col, None));

            if tp == TokenType::NewLineToken {
                line += 1;
                col = 1;
            } else {
                col += lexeme.len();
            }
        }

        return tokens;
    }

    #[inline]
    fn new_default_parser(tokens: Vec<Token>) -> Parser {
        return Parser::new(tokens);
    }

    mod test_primary {
        use super::*;

        #[test]
        fn test_string_literal() {
            let tokens = vec![Token::new(
                TokenType::StringLiteralToken,
                "string".to_string(),
                1,
                1,
                None,
            )];

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::array_allocation_expression(
                    ArrayLiteral::from(&"string".to_string()),
                    Token::new(
                        TokenType::StringLiteralToken,
                        "string".to_string(),
                        1,
                        1,
                        None,
                    )
                ))
            );
        }

        #[test]
        fn test_integer_literal() {
            let tokens = vec![Token::new(
                TokenType::IntegerLiteralToken,
                "55".to_string(),
                1,
                1,
                None,
            )];

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::literal_expression(Value::Integer(55)))
            );
        }

        #[test]
        fn test_float_literal() {
            let tokens = vec![Token::new(
                TokenType::DoubleLiteralToken,
                "55.22".to_string(),
                1,
                1,
                None,
            )];

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::literal_expression(Value::Float(55.22)))
            );
        }

        #[test]
        fn test_true_literal() {
            let tokens = vec![Token::new(
                TokenType::TrueToken,
                "true".to_string(),
                1,
                1,
                None,
            )];

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::literal_expression(Value::Boolean(true)))
            );
        }

        #[test]
        fn test_false_literal() {
            let tokens = vec![Token::new(
                TokenType::FalseToken,
                "false".to_string(),
                1,
                1,
                None,
            )];

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::literal_expression(Value::Boolean(false)))
            );
        }

        #[test]
        fn test_character_literal() {
            let tokens = vec![Token::new(
                TokenType::CharacterLiteralToken,
                "c".to_string(),
                1,
                1,
                None,
            )];

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::literal_expression(Value::Character('c')))
            );
        }

        #[test]
        fn test_grouping_character_literal() {
            let tokens = vec![
                Token::new(TokenType::OpenRoundBraceToken, "(".to_string(), 1, 1, None),
                Token::new(
                    TokenType::CharacterLiteralToken,
                    "c".to_string(),
                    1,
                    2,
                    None,
                ),
                Token::new(TokenType::CloseRoundBraceToken, ")".to_string(), 1, 3, None),
            ];

            let literal = new_expression(ExpressionNode::literal_expression(Value::Character('c')));

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::grouping_expression(
                    Token::new(TokenType::OpenRoundBraceToken, "(".to_string(), 1, 1, None,),
                    literal
                ))
            );
        }
    }

    mod test_binary_expression {
        use super::*;

        #[test]
        fn test_addition_1() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::PlusToken, "+"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::PlusToken, "+".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_addition_2() {
            let tokens = new_tokens([
                (TokenType::DoubleLiteralToken, "55.22"),
                (TokenType::PlusToken, "+"),
                (TokenType::DoubleLiteralToken, "52.22"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Float(55.22))),
                    Token::new(TokenType::PlusToken, "+".to_string(), 1, 6, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Float(52.22))),
                ))
            );
        }

        #[test]
        fn test_subtraction_1() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::MinusToken, "-"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::MinusToken, "-".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_subtraction_2() {
            let tokens = new_tokens([
                (TokenType::DoubleLiteralToken, "55.22"),
                (TokenType::MinusToken, "-"),
                (TokenType::DoubleLiteralToken, "52.22"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Float(55.22))),
                    Token::new(TokenType::MinusToken, "-".to_string(), 1, 6, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Float(52.22))),
                ))
            );
        }

        #[test]
        fn test_multiply() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::StarToken, "*"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::StarToken, "*".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_divide() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::BackslashToken, "/"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::BackslashToken, "/".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_greater() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::GreaterThanToken, ">".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_greater_equal() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanEqualToken, ">="),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(
                        TokenType::GreaterThanEqualToken,
                        ">=".to_string(),
                        1,
                        3,
                        None,
                    ),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_less() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::LessThanToken, "<".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_less_equal() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::LessThanEqualToken, "<="),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::LessThanEqualToken, "<=".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_equal() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::EqualsToken, "="),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::EqualsToken, "=".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_bang_equal() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::BangEqualsToken, "!="),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    Token::new(TokenType::BangEqualsToken, "!=".to_string(), 1, 3, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }
    }

    mod test_logical_expression {
        use super::*;

        #[test]
        fn test_single_and() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::AndToken, "and"),
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let cmp = new_expression(ExpressionNode::logical_expression(
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[1].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
                tokens[3].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[5].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_single_or() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::OrToken, "or"),
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let cmp = new_expression(ExpressionNode::logical_expression(
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[1].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
                tokens[3].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[5].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_multiple_and() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::AndToken, "and"),
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::AndToken, "and"),
                (TokenType::IntegerLiteralToken, "3"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "1"),
            ]);

            let cmp = new_expression(ExpressionNode::logical_expression(
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[1].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
                tokens[3].clone(),
                new_expression(ExpressionNode::logical_expression(
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                        tokens[5].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                    )),
                    tokens[7].clone(),
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(3))),
                        tokens[9].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(1))),
                    )),
                )),
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_multiple_or() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::OrToken, "or"),
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::OrToken, "or"),
                (TokenType::IntegerLiteralToken, "3"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "1"),
            ]);

            let cmp = new_expression(ExpressionNode::logical_expression(
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[1].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
                tokens[3].clone(),
                new_expression(ExpressionNode::logical_expression(
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                        tokens[5].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                    )),
                    tokens[7].clone(),
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(3))),
                        tokens[9].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(1))),
                    )),
                )),
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_mixed_and_or_1() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::AndToken, "and"),
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::OrToken, "or"),
                (TokenType::IntegerLiteralToken, "3"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "1"),
            ]);

            let cmp = new_expression(ExpressionNode::logical_expression(
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[1].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
                tokens[3].clone(),
                new_expression(ExpressionNode::logical_expression(
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                        tokens[5].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                    )),
                    tokens[7].clone(),
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(3))),
                        tokens[9].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(1))),
                    )),
                )),
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_mixed_and_or_2() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::OrToken, "or"),
                (TokenType::IntegerLiteralToken, "55"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::AndToken, "and"),
                (TokenType::IntegerLiteralToken, "3"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "1"),
            ]);

            let cmp = new_expression(ExpressionNode::logical_expression(
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                    tokens[1].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                )),
                tokens[3].clone(),
                new_expression(ExpressionNode::logical_expression(
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(55))),
                        tokens[5].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                    )),
                    tokens[7].clone(),
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(3))),
                        tokens[9].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(1))),
                    )),
                )),
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }
    }

    mod test_unary {
        use super::*;

        #[test]
        fn test_minus() {
            let tokens = new_tokens([
                (TokenType::MinusToken, "-"),
                (TokenType::IntegerLiteralToken, "52"),
            ]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::unary_expression(
                    Token::new(TokenType::MinusToken, "-".to_string(), 1, 1, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                ))
            );
        }

        #[test]
        fn test_not() {
            let tokens = new_tokens([(TokenType::NotToken, "not"), (TokenType::TrueToken, "true")]);

            let mut parser = new_default_parser(tokens);
            assert_eq!(
                parser.parse_expression().unwrap(),
                new_expression(ExpressionNode::unary_expression(
                    Token::new(TokenType::NotToken, "not".to_string(), 1, 1, None,),
                    new_expression(ExpressionNode::literal_expression(Value::Boolean(true))),
                ))
            );
        }
    }

    mod test_function_call {
        use super::*;

        #[test]
        fn test_no_arg_call() {
            let tokens = new_tokens([
                (TokenType::AtToken, "@"),
                (TokenType::IdentifierToken, "print"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_expression(ExpressionNode::call_expression(
                tokens[0].clone(),
                None,
                tokens[1].clone(),
                Vec::new(),
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_single_arg_call() {
            let tokens = new_tokens([
                (TokenType::AtToken, "@"),
                (TokenType::IdentifierToken, "print"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_expression(ExpressionNode::call_expression(
                tokens[0].clone(),
                None,
                tokens[1].clone(),
                vec![new_expression(ExpressionNode::literal_expression(
                    Value::Integer(22),
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_multiple_arg_call() {
            let tokens = new_tokens([
                (TokenType::AtToken, "@"),
                (TokenType::IdentifierToken, "print"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::CommaToken, ","),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::CommaToken, ","),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::CommaToken, ","),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_expression(ExpressionNode::call_expression(
                tokens[0].clone(),
                None,
                tokens[1].clone(),
                vec![
                    new_expression(ExpressionNode::literal_expression(Value::Integer(22))),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(22))),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(22))),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(22))),
                ],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_single_arg_module_call() {
            let tokens = new_tokens([
                (TokenType::AtToken, "@"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IdentifierToken, "std_io"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IdentifierToken, "print"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_expression(ExpressionNode::call_expression(
                tokens[0].clone(),
                Some(tokens[2].clone()),
                tokens[4].clone(),
                vec![new_expression(ExpressionNode::literal_expression(
                    Value::Integer(22),
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }
    }

    mod test_constructor_call {
        use super::*;

        #[test]
        fn test_no_arg_call() {
            let tokens = new_tokens([
                (TokenType::PipeToken, "|"),
                (TokenType::IdentifierToken, "container"),
                (TokenType::PipeToken, "|"),
            ]);

            let cmp = new_expression(ExpressionNode::constructor_call_expression(
                tokens[1].clone(),
                None,
                Vec::new(),
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_multi_arg_constructor_call() {
            let tokens = new_tokens([
                (TokenType::PipeToken, "|"),
                (TokenType::IdentifierToken, "container"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::CommaToken, ","),
                (TokenType::IntegerLiteralToken, "53"),
                (TokenType::PipeToken, "|"),
            ]);

            let cmp = new_expression(ExpressionNode::constructor_call_expression(
                tokens[1].clone(),
                None,
                vec![
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(53))),
                ],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_multi_arg_module_constructor_call() {
            let tokens = new_tokens([
                (TokenType::PipeToken, "|"),
                (TokenType::IdentifierToken, "container"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IdentifierToken, "std_conts"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::IntegerLiteralToken, "52"),
                (TokenType::CommaToken, ","),
                (TokenType::IntegerLiteralToken, "53"),
                (TokenType::PipeToken, "|"),
            ]);

            let cmp = new_expression(ExpressionNode::constructor_call_expression(
                tokens[1].clone(),
                Some(tokens[3].clone()),
                vec![
                    new_expression(ExpressionNode::literal_expression(Value::Integer(52))),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(53))),
                ],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }
    }

    mod test_variable {
        use super::*;

        #[test]
        fn test_variable_declaration() {
            let tokens = new_tokens([
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"),
                (TokenType::LeftArrowToken, "<-"),
                (TokenType::IntegerLiteralToken, "22"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::variable_declaration_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                new_expression(ExpressionNode::literal_expression(Value::Integer(22))),
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_variable_access() {
            let tokens = new_tokens([
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"),
            ]);

            let mut variable = Variable::new();
            variable.push(tokens[1].clone());

            let cmp = new_expression(ExpressionNode::variable_expression(
                tokens[0].clone(),
                variable,
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_variable_increment() {
            let tokens = new_tokens([
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"),
                (TokenType::LeftArrowToken, "<-"),
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"),
                (TokenType::PlusToken, "+"),
                (TokenType::IntegerLiteralToken, "1"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let mut variable = Variable::new();
            variable.push(tokens[4].clone());

            let cmp = new_statement(StatementNode::variable_declaration_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::variable_expression(
                        tokens[3].clone(),
                        variable,
                    )),
                    tokens[5].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(1))),
                )),
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_variable_assignment() {
            let tokens = new_tokens([
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"),
                (TokenType::LeftArrowToken, "<-"),
                (TokenType::IntegerLiteralToken, "44"),
            ]);

            let mut variable = Variable::new();
            variable.push(tokens[1].clone());

            let cmp = new_expression(ExpressionNode::assignment_expression(
                new_expression(ExpressionNode::variable_expression(
                    tokens[0].clone(),
                    variable,
                )),
                tokens[2].clone(),
                new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap(), cmp);
        }

        #[test]
        fn test_invalid_variable_assignment() {
            let tokens = new_tokens([
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::LeftArrowToken, "<-"),
                (TokenType::IntegerLiteralToken, "44"),
            ]);

            let e = ParserError::InvalidAssignmentTarget(tokens[1].clone());
            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_expression().unwrap_err(), e);
        }
    }

    mod test_if {
        use super::*;

        #[test]
        fn test_basic_if() {
            let tokens = new_tokens([
                (TokenType::IfToken, "if"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IfToken, "if"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_statement(StatementNode::if_statement(
                tokens[0].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    tokens[2].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                )),
                vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                ))],
                None,
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_basic_if_else() {
            let tokens = new_tokens([
                (TokenType::IfToken, "if"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::ElseToken, "else"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "3"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IfToken, "if"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_statement(StatementNode::if_statement(
                tokens[0].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    tokens[2].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                )),
                vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                ))],
                Some(vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(3))),
                ))]),
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_basic_invalid_if_else() {
            let tokens = new_tokens([
                (TokenType::IfToken, "if"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::ElseToken, "else"),
                (TokenType::IntegerLiteralToken, "3"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IfToken, "if"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = ParserError::ExpectedFound("new line".to_string(), "3".to_string());

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap_err(), cmp);
        }
    }

    mod test_while {
        use super::*;

        #[test]
        fn test_basic_while() {
            let tokens = new_tokens([
                (TokenType::WhileToken, "while"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::WhileToken, "while"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_statement(StatementNode::while_statement(
                tokens[0].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    tokens[2].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                )),
                vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_nested_while() {
            let tokens = new_tokens([
                (TokenType::WhileToken, "while"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::WhileToken, "while"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::WhileToken, "while"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::WhileToken, "while"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_statement(StatementNode::while_statement(
                tokens[0].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    tokens[2].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                )),
                vec![new_statement(StatementNode::while_statement(
                    tokens[5].clone(),
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                        tokens[7].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                    )),
                    vec![new_statement(StatementNode::expression_statement(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                    ))],
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_while_nested_if() {
            let tokens = new_tokens([
                (TokenType::WhileToken, "while"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IfToken, "if"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IfToken, "if"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::WhileToken, "while"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_statement(StatementNode::while_statement(
                tokens[0].clone(),
                new_expression(ExpressionNode::binary_expression(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    tokens[2].clone(),
                    new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                )),
                vec![new_statement(StatementNode::if_statement(
                    tokens[5].clone(),
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                        tokens[7].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(44))),
                    )),
                    vec![new_statement(StatementNode::expression_statement(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                    ))],
                    None,
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_basic_invalid_while_1() {
            let tokens = new_tokens([
                (TokenType::WhileToken, "while"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IfToken, "if"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = ParserError::ExpectedFound("while".to_string(), "if".to_string());

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap_err(), cmp);
        }

        #[test]
        fn test_basic_invalid_while_2() {
            let tokens = new_tokens([
                (TokenType::WhileToken, "while"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IntegerLiteralToken, "44"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::WhileToken, "while"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = ParserError::ExpectedFound("new line".to_string(), "2".to_string());

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap_err(), cmp);
        }
    }

    mod test_for {
        use super::*;

        #[test]
        fn test_basic_for() {
            let tokens = new_tokens([
                (TokenType::ForToken, "for"),
                (TokenType::IdentifierToken, "i"),
                (TokenType::ColonToken, ":"),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::IntegerLiteralToken, "0"),
                (TokenType::SemiColonToken, ";"),
                (TokenType::IntegerLiteralToken, "10"),
                (TokenType::SemiColonToken, ";"),
                (TokenType::IntegerLiteralToken, "1"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::ForToken, "for"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let cmp = new_statement(StatementNode::for_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                new_expression(ExpressionNode::literal_expression(Value::Integer(0))),
                new_expression(ExpressionNode::literal_expression(Value::Integer(10))),
                new_expression(ExpressionNode::literal_expression(Value::Integer(1))),
                vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }

        #[test]
        fn test_for_with_variable() {
            let tokens = new_tokens([
                (TokenType::ForToken, "for"),
                (TokenType::IdentifierToken, "i"),
                (TokenType::ColonToken, ":"),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::IntegerLiteralToken, "0"),
                (TokenType::SemiColonToken, ";"),
                (TokenType::IntegerLiteralToken, "10"),
                (TokenType::SemiColonToken, ";"),
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "step"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "2"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::ForToken, "for"),
                (TokenType::CloseRoundBraceToken, ")"),
            ]);

            let mut var = Variable::new();
            var.push(tokens[9].clone());

            let cmp = new_statement(StatementNode::for_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                new_expression(ExpressionNode::literal_expression(Value::Integer(0))),
                new_expression(ExpressionNode::literal_expression(Value::Integer(10))),
                new_expression(ExpressionNode::variable_expression(tokens[8].clone(), var)),
                vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::literal_expression(Value::Integer(2))),
                ))],
            ));

            let mut parser = new_default_parser(tokens);
            assert_eq!(parser.parse_statement().unwrap(), cmp);
        }
    }

    mod test_function_definition {
        use super::*;

        #[test]
        fn test_empty_function_definition() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                Vec::new(),
                None,
                Vec::new(),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_single_argument_function_definition() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "integer"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                vec![Field {
                    tp: Type::Integer,
                    name: "a".to_string(),
                }],
                None,
                Vec::new(),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_multi_argument_function_definition() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "integer"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "b"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "char"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "c"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "bool"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "d"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "float"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "e"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::IdentifierToken, "integer"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "f"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::IdentifierToken, "integer"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "g"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::OpenSquareBraceToken, "["),
                (TokenType::IdentifierToken, "integer"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::CloseSquareBraceToken, "]"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                vec![
                    Field {
                        tp: Type::Integer,
                        name: "a".to_string(),
                    },
                    Field {
                        tp: Type::Character,
                        name: "b".to_string(),
                    },
                    Field {
                        tp: Type::Boolean,
                        name: "c".to_string(),
                    },
                    Field {
                        tp: Type::Float,
                        name: "d".to_string(),
                    },
                    Field {
                        tp: Type::Array(Box::new(Type::Integer)),
                        name: "e".to_string(),
                    },
                    Field {
                        tp: Type::Array(Box::new(Type::Array(Box::new(Type::Integer)))),
                        name: "f".to_string(),
                    },
                    Field {
                        tp: Type::Array(Box::new(Type::Array(Box::new(Type::Array(Box::new(
                            Type::Integer,
                        )))))),
                        name: "g".to_string(),
                    },
                ],
                None,
                Vec::new(),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_function_definition_with_return() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::RightArrowToken, "->"),
                (TokenType::IdentifierToken, "int"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::ReturnToken, "return"),
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                Vec::new(),
                Some(Type::Integer),
                vec![new_statement(StatementNode::return_statement(
                    tokens[5].clone(),
                    Some(new_expression(ExpressionNode::literal_expression(
                        Value::Integer(12),
                    ))),
                ))],
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_single_arg_function_definition_with_return() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "int"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::RightArrowToken, "<-"),
                (TokenType::IdentifierToken, "int"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                vec![Field {
                    tp: Type::Integer,
                    name: "a".to_string(),
                }],
                Some(Type::Integer),
                Vec::new(),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_function_definition_with_empty_return() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::ReturnToken, "return"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                Vec::new(),
                None,
                vec![new_statement(StatementNode::return_statement(
                    tokens[3].clone(),
                    None,
                ))],
            ));

            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_function_definition_with_body() {
            let tokens = new_tokens([
                (TokenType::FunctionToken, "func"),
                (TokenType::IdentifierToken, "function"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "int"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::RightArrowToken, "<-"),
                (TokenType::IdentifierToken, "int"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IntegerLiteralToken, "5"),
                (TokenType::PlusToken, "+"),
                (TokenType::IntegerLiteralToken, "5"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::function_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                vec![Field {
                    tp: Type::Integer,
                    name: "a".to_string(),
                }],
                Some(Type::Integer),
                vec![new_statement(StatementNode::expression_statement(
                    new_expression(ExpressionNode::binary_expression(
                        new_expression(ExpressionNode::literal_expression(Value::Integer(5))),
                        tokens[12].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(5))),
                    )),
                ))],
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }
    }

    mod test_struct_definition {
        use super::*;

        #[test]
        fn test_empty_struct_definition() {
            let tokens = new_tokens([
                (TokenType::StructToken, "struct"),
                (TokenType::IdentifierToken, "my_struct"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::StructToken, "struct"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::struct_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                HashMap::new(),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_single_struct_definition() {
            let tokens = new_tokens([
                (TokenType::StructToken, "struct"),
                (TokenType::IdentifierToken, "my_struct"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "int"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::StructToken, "struct"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::struct_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                hashmap!["field_a".to_string() ; Field {
                    tp: Type::Integer,
                    name: "field_a".to_string(),
                }],
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_multiple_struct_definition() {
            let tokens = new_tokens([
                (TokenType::StructToken, "struct"),
                (TokenType::IdentifierToken, "my_struct"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "int"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_b"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "bool"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_c"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "char"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_d"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "float"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::StructToken, "struct"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::struct_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                hashmap![
                    "field_a".to_string() ; Field {
                        tp: Type::Integer,
                        name: "field_a".to_string(),
                    },
                    "field_b".to_string() ; Field {
                        tp: Type::Boolean,
                        name: "field_b".to_string(),
                    },
                    "field_c".to_string() ; Field {
                        tp: Type::Character,
                        name: "field_c".to_string(),
                    },
                    "field_d".to_string() ; Field {
                        tp: Type::Float,
                        name: "field_d".to_string(),
                    }
                ],
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_struct_definition_struct_fields() {
            let tokens = new_tokens([
                (TokenType::StructToken, "struct"),
                (TokenType::IdentifierToken, "my_struct"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "int"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_b"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "string"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IdentifierToken, "std"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::StructToken, "struct"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::struct_statement(
                tokens[0].clone(),
                tokens[1].clone(),
                hashmap![
                    "field_a".to_string() ; Field {
                        tp: Type::Integer,
                        name: "field_a".to_string(),
                    },
                    "field_b".to_string() ; Field {
                        tp: Type::Struct {name: "string".to_string(), module: "std".to_string() },
                        name: "field_b".to_string(),
                    }
                ],
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_struct_definition_duplicate_struct_fields() {
            let tokens = new_tokens([
                (TokenType::StructToken, "struct"),
                (TokenType::IdentifierToken, "my_struct"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_a"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "int"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_b"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "string"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IdentifierToken, "std"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::IdentifierToken, "field_b"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::IdentifierToken, "decimals"),
                (TokenType::LessThanToken, "<"),
                (TokenType::IdentifierToken, "math"),
                (TokenType::GreaterThanToken, ">"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::StructToken, "struct"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp =
                ParserError::FieldAlreadyDefinedForStruct("field_b".to_string(), tokens[1].clone());
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap_err(), cmp);
        }
    }

    mod test_preprocessor_command {
        use super::*;

        #[test]
        fn test_begin_module() {
            let tokens = new_tokens([
                (TokenType::PercentToken, "%"),
                (TokenType::IdentifierToken, "begin"),
                (TokenType::IdentifierToken, "my_module"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::pre_processor_command_statement(
                tokens[1].clone(),
                PreProcessorCommand::BeginModuleCommand(tokens[2].clone()),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_single_import() {
            let tokens = new_tokens([
                (TokenType::PercentToken, "%"),
                (TokenType::IdentifierToken, "import"),
                (TokenType::IdentifierToken, "my_function"),
                (TokenType::IdentifierToken, "from"),
                (TokenType::IdentifierToken, "my_module"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::pre_processor_command_statement(
                tokens[1].clone(),
                PreProcessorCommand::ImportCommand(tokens[4].clone(), vec![tokens[2].clone()]),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }

        #[test]
        fn test_multi_import() {
            let tokens = new_tokens([
                (TokenType::PercentToken, "%"),
                (TokenType::IdentifierToken, "import"),
                (TokenType::IdentifierToken, "my_function"),
                (TokenType::CommaToken, ","),
                (TokenType::IdentifierToken, "my_function2"),
                (TokenType::IdentifierToken, "from"),
                (TokenType::IdentifierToken, "my_module"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let cmp = new_statement(StatementNode::pre_processor_command_statement(
                tokens[1].clone(),
                PreProcessorCommand::ImportCommand(
                    tokens[6].clone(),
                    vec![tokens[2].clone(), tokens[4].clone()],
                ),
            ));
            let mut parser = new_default_parser(tokens);

            assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
        }
    }

    mod test_sample_program {
        use super::*;
        use crate::lexer::Lexer;

        #[test]
        fn test_main_with_double() {
            let tokens = new_tokens([
                // New module "main"
                (TokenType::PercentToken, "%"),
                (TokenType::IdentifierToken, "begin"),
                (TokenType::IdentifierToken, "main"),
                (TokenType::NewLineToken, "\n"),
                // Import "double" from "math"
                (TokenType::PercentToken, "%"),
                (TokenType::IdentifierToken, "import"),
                (TokenType::IdentifierToken, "double"),
                (TokenType::IdentifierToken, "from"),
                (TokenType::IdentifierToken, "math"),
                (TokenType::NewLineToken, "\n"),
                (TokenType::NewLineToken, "\n"),
                // New function "main" returns int
                (TokenType::FunctionToken, "func"), // 11
                (TokenType::IdentifierToken, "main"),
                (TokenType::RightArrowToken, "->"),
                (TokenType::IdentifierToken, "int"),
                (TokenType::NewLineToken, "\n"),
                // New variable "a" set to 5
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"),
                (TokenType::LeftArrowToken, "<-"), // 18
                (TokenType::IntegerLiteralToken, "5"),
                (TokenType::NewLineToken, "\n"),
                // New variable "b" set to 12
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "b"),
                (TokenType::LeftArrowToken, "<-"), // 23
                (TokenType::IntegerLiteralToken, "12"),
                (TokenType::NewLineToken, "\n"),
                // Call the double function
                (TokenType::AtToken, "@"),
                (TokenType::IdentifierToken, "double"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "a"), // 30
                (TokenType::PlusToken, "+"),
                (TokenType::DollarToken, "$"),
                (TokenType::IdentifierToken, "b"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
                // End the main function
                (TokenType::EndToken, "end"),
                (TokenType::OpenRoundBraceToken, "("),
                (TokenType::FunctionToken, "func"),
                (TokenType::CloseRoundBraceToken, ")"),
                (TokenType::NewLineToken, "\n"),
            ]);

            let mut cmp = AST::new();
            cmp.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    tokens[1].clone(),
                    PreProcessorCommand::BeginModuleCommand(tokens[2].clone()),
                ),
            ));

            cmp.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    tokens[5].clone(),
                    PreProcessorCommand::ImportCommand(tokens[8].clone(), vec![tokens[6].clone()]),
                ),
            ));

            let mut var_a_2 = Variable::new();
            var_a_2.push(tokens[30].clone());

            let mut var_b_2 = Variable::new();
            var_b_2.push(tokens[33].clone());

            cmp.push_statement(new_statement(StatementNode::function_statement(
                tokens[11].clone(),
                tokens[12].clone(),
                Vec::new(),
                Some(Type::Integer),
                vec![
                    new_statement(StatementNode::variable_declaration_statement(
                        tokens[16].clone(),
                        tokens[17].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(5))),
                    )),
                    new_statement(StatementNode::variable_declaration_statement(
                        tokens[21].clone(),
                        tokens[22].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    )),
                    new_statement(StatementNode::expression_statement(new_expression(
                        ExpressionNode::call_expression(
                            tokens[26].clone(),
                            None,
                            tokens[27].clone(),
                            vec![new_expression(ExpressionNode::binary_expression(
                                new_expression(ExpressionNode::variable_expression(
                                    tokens[29].clone(),
                                    var_a_2,
                                )),
                                tokens[31].clone(),
                                new_expression(ExpressionNode::variable_expression(
                                    tokens[32].clone(),
                                    var_b_2,
                                )),
                            ))],
                        ),
                    ))),
                ],
            )));

            let parser = new_default_parser(tokens);

            assert_eq!(parser.parse().unwrap(), cmp);
        }

        #[test]
        fn test_main_with_double_using_lexer() {
            let input = "%begin main\n\
            %import double from math\n\
            \n\
            func main -> int\n\
                $a <- 5\n\
                $b <- 12\n\
                @double($a + $b)\n\
            end(func)\n\
            ";

            let lexer = Lexer::new(input.chars().collect());
            let tokens = lexer.tokenize().unwrap();

            let mut cmp = AST::new();
            cmp.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    tokens[1].clone(),
                    PreProcessorCommand::BeginModuleCommand(tokens[2].clone()),
                ),
            ));

            cmp.push_statement(new_statement(
                StatementNode::pre_processor_command_statement(
                    tokens[5].clone(),
                    PreProcessorCommand::ImportCommand(tokens[8].clone(), vec![tokens[6].clone()]),
                ),
            ));

            let mut var_a_2 = Variable::new();
            var_a_2.push(tokens[30].clone());

            let mut var_b_2 = Variable::new();
            var_b_2.push(tokens[33].clone());

            cmp.push_statement(new_statement(StatementNode::function_statement(
                tokens[11].clone(),
                tokens[12].clone(),
                Vec::new(),
                Some(Type::Integer),
                vec![
                    new_statement(StatementNode::variable_declaration_statement(
                        tokens[16].clone(),
                        tokens[17].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(5))),
                    )),
                    new_statement(StatementNode::variable_declaration_statement(
                        tokens[21].clone(),
                        tokens[22].clone(),
                        new_expression(ExpressionNode::literal_expression(Value::Integer(12))),
                    )),
                    new_statement(StatementNode::expression_statement(new_expression(
                        ExpressionNode::call_expression(
                            tokens[26].clone(),
                            None,
                            tokens[27].clone(),
                            vec![new_expression(ExpressionNode::binary_expression(
                                new_expression(ExpressionNode::variable_expression(
                                    tokens[29].clone(),
                                    var_a_2,
                                )),
                                tokens[31].clone(),
                                new_expression(ExpressionNode::variable_expression(
                                    tokens[32].clone(),
                                    var_b_2,
                                )),
                            ))],
                        ),
                    ))),
                ],
            )));

            let parser = new_default_parser(tokens);

            assert_eq!(parser.parse().unwrap(), cmp);
        }
    }

    #[test]
    fn test_block() {
        let tokens = vec![
            Token::new(TokenType::BlockToken, "block".to_string(), 1, 1, None),
            Token::new(TokenType::NewLineToken, "\n".to_string(), 1, 2, None),
            Token::new(
                TokenType::StringLiteralToken,
                "string".to_string(),
                2,
                1,
                None,
            ),
            Token::new(TokenType::NewLineToken, "\n".to_string(), 2, 2, None),
            Token::new(TokenType::EndToken, "end".to_string(), 3, 1, None),
            Token::new(TokenType::OpenRoundBraceToken, "(".to_string(), 3, 5, None),
            Token::new(TokenType::BlockToken, "block".to_string(), 1, 6, None),
            Token::new(
                TokenType::CloseRoundBraceToken,
                ")".to_string(),
                3,
                11,
                None,
            ),
            Token::new(TokenType::NewLineToken, "\n".to_string(), 3, 12, None),
        ];

        let cmp = new_statement(StatementNode::block_statement(
            Token::new(TokenType::BlockToken, "block".to_string(), 1, 1, None),
            vec![new_statement(StatementNode::expression_statement(
                new_expression(ExpressionNode::array_allocation_expression(
                    ArrayLiteral::from(&"string".to_string()),
                    Token::new(
                        TokenType::StringLiteralToken,
                        "string".to_string(),
                        2,
                        1,
                        None,
                    ),
                )),
            ))],
        ));

        let mut parser = new_default_parser(tokens);
        assert_eq!(parser.parse_statement().unwrap(), cmp);
    }
}
