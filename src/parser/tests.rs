use super::{
    ArrayLiteral, ExpressionNode, Parser, ParserError, PreProcessorCommand, StatementNode, Token,
    TokenType, Type, Value, Variable, AST,
};

use std::collections::HashMap;

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

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::array_allocation_expression(
                Token::new(
                    TokenType::StringLiteralToken,
                    "string".to_string(),
                    1,
                    1,
                    None,
                ),
                ArrayLiteral::from(&"string".to_string()),
                ExpressionNode::literal_expression(
                    Token::new(
                        TokenType::StringLiteralToken,
                        "string".to_string(),
                        1,
                        1,
                        None,
                    ),
                    Value::Integer(6)
                )
                .wrapped(),
            )
            .wrapped()
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

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::literal_expression(
                Token::new(TokenType::IntegerLiteralToken, "55".to_string(), 1, 1, None,),
                Value::Integer(55)
            )
            .wrapped()
        );
    }

    #[test]
    fn test_float_literal() {
        let tokens = vec![Token::new(
            TokenType::FloatLiteralToken,
            "55.22".to_string(),
            1,
            1,
            None,
        )];

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::literal_expression(
                Token::new(
                    TokenType::FloatLiteralToken,
                    "55.22".to_string(),
                    1,
                    1,
                    None,
                ),
                Value::Float(55.22)
            )
            .wrapped()
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

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::literal_expression(
                Token::new(TokenType::TrueToken, "true".to_string(), 1, 1, None,),
                Value::Boolean(true)
            )
            .wrapped()
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

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::literal_expression(
                Token::new(TokenType::FalseToken, "false".to_string(), 1, 1, None,),
                Value::Boolean(false)
            )
            .wrapped()
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

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::literal_expression(
                Token::new(
                    TokenType::CharacterLiteralToken,
                    "c".to_string(),
                    1,
                    1,
                    None,
                ),
                Value::Character('c')
            )
            .wrapped()
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

        let literal = ExpressionNode::literal_expression(
            Token::new(
                TokenType::CharacterLiteralToken,
                "c".to_string(),
                1,
                2,
                None,
            ),
            Value::Character('c'),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::grouping_expression(
                Token::new(TokenType::OpenRoundBraceToken, "(".to_string(), 1, 1, None,),
                literal
            )
            .wrapped()
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

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();
        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::PlusToken, "+".to_string(), 1, 3, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_addition_2() {
        let tokens = new_tokens([
            (TokenType::FloatLiteralToken, "55.22"),
            (TokenType::PlusToken, "+"),
            (TokenType::FloatLiteralToken, "52.22"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Float(55.22)).wrapped();
        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Float(52.22)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::PlusToken, "+".to_string(), 1, 6, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_subtraction_1() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::MinusToken, "-"),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();
        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::MinusToken, "-".to_string(), 1, 3, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_subtraction_2() {
        let tokens = new_tokens([
            (TokenType::FloatLiteralToken, "55.22"),
            (TokenType::MinusToken, "-"),
            (TokenType::FloatLiteralToken, "52.22"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Float(55.22)).wrapped();
        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Float(52.22)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::MinusToken, "-".to_string(), 1, 6, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_multiply() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::StarToken, "*"),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::StarToken, "*".to_string(), 1, 3, None,),
                rhs
            )
            .wrapped()
        );
    }

    #[test]
    fn test_divide() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::ForwardSlashToken, "/"),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::ForwardSlashToken, "/".to_string(), 1, 3, None,),
                rhs
            )
            .wrapped()
        );
    }

    #[test]
    fn test_greater() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::GreaterThanToken, ">"),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::GreaterThanToken, ">".to_string(), 1, 3, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_greater_equal() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::GreaterThanEqualToken, ">="),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(
                    TokenType::GreaterThanEqualToken,
                    ">=".to_string(),
                    1,
                    3,
                    None,
                ),
                rhs
            )
            .wrapped()
        );
    }

    #[test]
    fn test_less() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::LessThanToken, "<"),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::LessThanToken, "<".to_string(), 1, 3, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_less_equal() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::LessThanEqualToken, "<="),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::LessThanEqualToken, "<=".to_string(), 1, 3, None,),
                rhs
            )
            .wrapped()
        );
    }

    #[test]
    fn test_equal() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::EqualsToken, "="),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::EqualsToken, "=".to_string(), 1, 3, None,),
                rhs
            )
            .wrapped()
        );
    }

    #[test]
    fn test_bang_equal() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "55"),
            (TokenType::BangEqualsToken, "!="),
            (TokenType::IntegerLiteralToken, "52"),
        ]);

        let lhs =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::binary_expression(
                lhs,
                Token::new(TokenType::BangEqualsToken, "!=".to_string(), 1, 3, None,),
                rhs
            )
            .wrapped()
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

        let lhs_1 =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs_1 =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let lhs_2 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(55)).wrapped();

        let rhs_2 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped();

        let cmp = ExpressionNode::logical_expression(
            ExpressionNode::binary_expression(lhs_1, tokens[1].clone(), rhs_1).wrapped(),
            tokens[3].clone(),
            ExpressionNode::binary_expression(lhs_2, tokens[5].clone(), rhs_2).wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let lhs_1 =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs_1 =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let lhs_2 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(55)).wrapped();

        let rhs_2 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped();

        let cmp = ExpressionNode::logical_expression(
            ExpressionNode::binary_expression(lhs_1, tokens[1].clone(), rhs_1).wrapped(),
            tokens[3].clone(),
            ExpressionNode::binary_expression(lhs_2, tokens[5].clone(), rhs_2).wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let lhs_1 =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs_1 =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let lhs_2 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(55)).wrapped();

        let rhs_2 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped();

        let lhs_3 =
            ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(3)).wrapped();

        let rhs_3 =
            ExpressionNode::literal_expression(tokens[10].clone(), Value::Integer(1)).wrapped();

        let cmp = ExpressionNode::logical_expression(
            ExpressionNode::binary_expression(lhs_1, tokens[1].clone(), rhs_1).wrapped(),
            tokens[3].clone(),
            ExpressionNode::logical_expression(
                ExpressionNode::binary_expression(lhs_2, tokens[5].clone(), rhs_2).wrapped(),
                tokens[7].clone(),
                ExpressionNode::binary_expression(lhs_3, tokens[9].clone(), rhs_3).wrapped(),
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let lhs_1 =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs_1 =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let lhs_2 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(55)).wrapped();

        let rhs_2 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped();

        let lhs_3 =
            ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(3)).wrapped();

        let rhs_3 =
            ExpressionNode::literal_expression(tokens[10].clone(), Value::Integer(1)).wrapped();

        let cmp = ExpressionNode::logical_expression(
            ExpressionNode::binary_expression(lhs_1, tokens[1].clone(), rhs_1).wrapped(),
            tokens[3].clone(),
            ExpressionNode::logical_expression(
                ExpressionNode::binary_expression(lhs_2, tokens[5].clone(), rhs_2).wrapped(),
                tokens[7].clone(),
                ExpressionNode::binary_expression(lhs_3, tokens[9].clone(), rhs_3).wrapped(),
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let lhs_1 =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs_1 =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let lhs_2 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(55)).wrapped();

        let rhs_2 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped();

        let lhs_3 =
            ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(3)).wrapped();

        let rhs_3 =
            ExpressionNode::literal_expression(tokens[10].clone(), Value::Integer(1)).wrapped();

        let cmp = ExpressionNode::logical_expression(
            ExpressionNode::binary_expression(lhs_1, tokens[1].clone(), rhs_1).wrapped(),
            tokens[3].clone(),
            ExpressionNode::logical_expression(
                ExpressionNode::binary_expression(lhs_2, tokens[5].clone(), rhs_2).wrapped(),
                tokens[7].clone(),
                ExpressionNode::binary_expression(lhs_3, tokens[9].clone(), rhs_3).wrapped(),
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let lhs_1 =
            ExpressionNode::literal_expression(tokens[0].clone(), Value::Integer(55)).wrapped();

        let rhs_1 =
            ExpressionNode::literal_expression(tokens[2].clone(), Value::Integer(52)).wrapped();

        let lhs_2 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(55)).wrapped();

        let rhs_2 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped();

        let lhs_3 =
            ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(3)).wrapped();

        let rhs_3 =
            ExpressionNode::literal_expression(tokens[10].clone(), Value::Integer(1)).wrapped();

        let cmp = ExpressionNode::logical_expression(
            ExpressionNode::binary_expression(lhs_1, tokens[1].clone(), rhs_1).wrapped(),
            tokens[3].clone(),
            ExpressionNode::logical_expression(
                ExpressionNode::binary_expression(lhs_2, tokens[5].clone(), rhs_2).wrapped(),
                tokens[7].clone(),
                ExpressionNode::binary_expression(lhs_3, tokens[9].clone(), rhs_3).wrapped(),
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let rhs =
            ExpressionNode::literal_expression(tokens[1].clone(), Value::Integer(52)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::unary_expression(
                Token::new(TokenType::MinusToken, "-".to_string(), 1, 1, None,),
                rhs,
            )
            .wrapped()
        );
    }

    #[test]
    fn test_not() {
        let tokens = new_tokens([(TokenType::NotToken, "not"), (TokenType::TrueToken, "true")]);

        let rhs =
            ExpressionNode::literal_expression(tokens[1].clone(), Value::Boolean(true)).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.parse_expression().unwrap(),
            ExpressionNode::unary_expression(
                Token::new(TokenType::NotToken, "not".to_string(), 1, 1, None,),
                rhs,
            )
            .wrapped()
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

        let cmp =
            ExpressionNode::call_expression(tokens[0].clone(), None, tokens[1].clone(), Vec::new())
                .wrapped();

        let mut parser = Parser::new(tokens);
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

        let arg =
            ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(22)).wrapped();

        let cmp =
            ExpressionNode::call_expression(tokens[0].clone(), None, tokens[1].clone(), vec![arg])
                .wrapped();

        let mut parser = Parser::new(tokens);
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

        let a1 =
            ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(22)).wrapped();

        let a2 =
            ExpressionNode::literal_expression(tokens[5].clone(), Value::Integer(22)).wrapped();

        let a3 =
            ExpressionNode::literal_expression(tokens[7].clone(), Value::Integer(22)).wrapped();

        let a4 =
            ExpressionNode::literal_expression(tokens[9].clone(), Value::Integer(22)).wrapped();

        let cmp = ExpressionNode::call_expression(
            tokens[0].clone(),
            None,
            tokens[1].clone(),
            vec![a1, a2, a3, a4],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let a1 =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(22)).wrapped();

        let cmp = ExpressionNode::call_expression(
            tokens[0].clone(),
            Some(tokens[2].clone()),
            tokens[4].clone(),
            vec![a1],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp =
            ExpressionNode::constructor_call_expression(tokens[1].clone(), None, HashMap::new())
                .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }

    #[test]
    fn test_multi_arg_constructor_call() {
        let tokens = new_tokens([
            (TokenType::PipeToken, "|"),
            (TokenType::IdentifierToken, "container"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::ColonToken, ":"),
            (TokenType::IntegerLiteralToken, "52"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "b"),
            (TokenType::ColonToken, ":"),
            (TokenType::IntegerLiteralToken, "53"),
            (TokenType::PipeToken, "|"),
        ]);

        let a1 =
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(52)).wrapped();

        let a2 =
            ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(53)).wrapped();

        let cmp = ExpressionNode::constructor_call_expression(
            tokens[1].clone(),
            None,
            hashmap![
                    tokens[2].clone() ; a1,
                    tokens[6].clone() ; a2
            ],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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
            (TokenType::IdentifierToken, "a"),
            (TokenType::ColonToken, ":"),
            (TokenType::IntegerLiteralToken, "52"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "b"),
            (TokenType::ColonToken, ":"),
            (TokenType::IntegerLiteralToken, "53"),
            (TokenType::PipeToken, "|"),
        ]);

        let a1 =
            ExpressionNode::literal_expression(tokens[7].clone(), Value::Integer(52)).wrapped();

        let a2 =
            ExpressionNode::literal_expression(tokens[11].clone(), Value::Integer(53)).wrapped();

        let cmp = ExpressionNode::constructor_call_expression(
            tokens[1].clone(),
            Some(tokens[3].clone()),
            hashmap![
                    tokens[5].clone() ; a1,
                    tokens[9].clone() ; a2

            ],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }
}

mod test_variable {
    use super::*;

    #[test]
    fn test_variable_declaration() {
        let tokens = new_tokens([
            (TokenType::HashToken, "#"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::LeftArrowToken, "<-"),
            (TokenType::IntegerLiteralToken, "22"),
            (TokenType::NewLineToken, "\n"),
        ]);

        let cmp = StatementNode::variable_declaration_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(22)).wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = ExpressionNode::variable_expression(tokens[0].clone(), variable).wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }

    #[test]
    fn test_variable_increment() {
        let tokens = new_tokens([
            (TokenType::HashToken, "#"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::LeftArrowToken, "<-"),
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::PlusToken, "+"),
            (TokenType::IntegerLiteralToken, "1"),
            (TokenType::NewLineToken, "\n"),
        ]);

        let rhs =
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(1)).wrapped();

        let mut variable = Variable::new();
        variable.push(tokens[4].clone());

        let cmp = StatementNode::variable_declaration_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            ExpressionNode::binary_expression(
                ExpressionNode::variable_expression(tokens[3].clone(), variable).wrapped(),
                tokens[5].clone(),
                rhs,
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = ExpressionNode::assignment_expression(
            ExpressionNode::variable_expression(tokens[0].clone(), variable).wrapped(),
            tokens[2].clone(),
            ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(44)).wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }

    #[test]
    fn test_invalid_variable_assignment() {
        let tokens = new_tokens([
            (TokenType::IntegerLiteralToken, "44"),
            (TokenType::LeftArrowToken, "<-"),
            (TokenType::IntegerLiteralToken, "44"),
        ]);

        let e = ParserError::invalid_assignment_target(tokens[1].clone());
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap_err(), e);
    }
}

mod test_array_index {
    use super::*;

    #[test]
    fn test_array_index_variable() {
        let tokens = new_tokens([
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IntegerLiteralToken, "52"),
            (TokenType::CloseSquareBraceToken, "]"),
        ]);

        let mut variable = Variable::new();
        variable.push(tokens[1].clone());

        let cmp = ExpressionNode::array_index_expression(
            tokens[2].clone(),
            ExpressionNode::variable_expression(tokens[0].clone(), variable).wrapped(),
            ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(52)).wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }

    #[test]
    fn test_array_index_double() {
        let tokens = new_tokens([
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IntegerLiteralToken, "0"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IntegerLiteralToken, "52"),
            (TokenType::CloseSquareBraceToken, "]"),
        ]);

        let mut variable = Variable::new();
        variable.push(tokens[1].clone());

        let cmp = ExpressionNode::array_index_expression(
            tokens[5].clone(),
            ExpressionNode::array_index_expression(
                tokens[2].clone(),
                ExpressionNode::variable_expression(tokens[0].clone(), variable).wrapped(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(0)).wrapped(),
            )
            .wrapped(),
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(52)).wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }

    #[test]
    fn test_array_index_array_index() {
        let tokens = new_tokens([
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "b"),
            (TokenType::OpenSquareBraceToken, "["), // 5
            (TokenType::IntegerLiteralToken, "0"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::CloseSquareBraceToken, "]"),
        ]);

        let mut variable_a = Variable::new();
        variable_a.push(tokens[1].clone());

        let mut variable_b = Variable::new();
        variable_b.push(tokens[4].clone());

        let cmp = ExpressionNode::array_index_expression(
            tokens[2].clone(),
            ExpressionNode::variable_expression(tokens[0].clone(), variable_a).wrapped(),
            ExpressionNode::array_index_expression(
                tokens[5].clone(),
                ExpressionNode::variable_expression(tokens[3].clone(), variable_b).wrapped(),
                ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(0)).wrapped(),
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
    }

    #[test]
    fn test_array_index_assign() {
        let tokens = new_tokens([
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IntegerLiteralToken, "1"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::LeftArrowToken, "<-"),
            (TokenType::DollarToken, "$"),
            (TokenType::IdentifierToken, "b"),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IntegerLiteralToken, "0"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::NewLineToken, "\n"),
        ]);

        let mut variable_a = Variable::new();
        variable_a.push(tokens[1].clone());

        let mut variable_b = Variable::new();
        variable_b.push(tokens[7].clone());

        let cmp = ExpressionNode::assignment_expression(
            ExpressionNode::array_index_expression(
                tokens[2].clone(),
                ExpressionNode::variable_expression(tokens[0].clone(), variable_a).wrapped(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(1)).wrapped(),
            )
            .wrapped(),
            tokens[5].clone(),
            ExpressionNode::array_index_expression(
                tokens[8].clone(),
                ExpressionNode::variable_expression(tokens[6].clone(), variable_b).wrapped(),
                ExpressionNode::literal_expression(tokens[9].clone(), Value::Integer(0)).wrapped(),
            )
            .wrapped(),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
        assert_eq!(parser.parse_expression().unwrap(), cmp);
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

        let cmp = StatementNode::if_statement(
            tokens[0].clone(),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(tokens[1].clone(), Value::Integer(12)).wrapped(),
                tokens[2].clone(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(44)).wrapped(),
            )
            .wrapped(),
            vec![StatementNode::expression_statement(
                ExpressionNode::literal_expression(tokens[5].clone(), Value::Integer(2)).wrapped(),
            )
            .wrapped()],
            None,
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::if_statement(
            tokens[0].clone(),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(tokens[1].clone(), Value::Integer(12)).wrapped(),
                tokens[2].clone(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(44)).wrapped(),
            )
            .wrapped(),
            vec![StatementNode::expression_statement(
                ExpressionNode::literal_expression(tokens[5].clone(), Value::Integer(2)).wrapped(),
            )
            .wrapped()],
            Some(vec![StatementNode::expression_statement(
                ExpressionNode::literal_expression(tokens[9].clone(), Value::Integer(3)).wrapped(),
            )
            .wrapped()]),
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = ParserError::expected_found("new line".to_string(), "3".to_string());

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::while_statement(
            tokens[0].clone(),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(tokens[1].clone(), Value::Integer(12)).wrapped(),
                tokens[2].clone(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(44)).wrapped(),
            )
            .wrapped(),
            vec![StatementNode::expression_statement(
                ExpressionNode::literal_expression(tokens[5].clone(), Value::Integer(2)).wrapped(),
            )
            .wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::while_statement(
            tokens[0].clone(),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(tokens[1].clone(), Value::Integer(12)).wrapped(),
                tokens[2].clone(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(44)).wrapped(),
            )
            .wrapped(),
            vec![StatementNode::while_statement(
                tokens[5].clone(),
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(12))
                        .wrapped(),
                    tokens[7].clone(),
                    ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(44))
                        .wrapped(),
                )
                .wrapped(),
                vec![StatementNode::expression_statement(
                    ExpressionNode::literal_expression(tokens[10].clone(), Value::Integer(2))
                        .wrapped(),
                )
                .wrapped()],
            )
            .wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::while_statement(
            tokens[0].clone(),
            ExpressionNode::binary_expression(
                ExpressionNode::literal_expression(tokens[1].clone(), Value::Integer(12)).wrapped(),
                tokens[2].clone(),
                ExpressionNode::literal_expression(tokens[3].clone(), Value::Integer(44)).wrapped(),
            )
            .wrapped(),
            vec![StatementNode::if_statement(
                tokens[5].clone(),
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(12))
                        .wrapped(),
                    tokens[7].clone(),
                    ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(44))
                        .wrapped(),
                )
                .wrapped(),
                vec![StatementNode::expression_statement(
                    ExpressionNode::literal_expression(tokens[10].clone(), Value::Integer(2))
                        .wrapped(),
                )
                .wrapped()],
                None,
            )
            .wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = ParserError::expected_found("while".to_string(), "if".to_string());

        let mut parser = Parser::new(tokens);
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

        let cmp = ParserError::expected_found("new line".to_string(), "2".to_string());

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::for_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(0)).wrapped(),
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(10)).wrapped(),
            ExpressionNode::literal_expression(tokens[8].clone(), Value::Integer(1)).wrapped(),
            vec![StatementNode::expression_statement(
                ExpressionNode::literal_expression(tokens[11].clone(), Value::Integer(2)).wrapped(),
            )
            .wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::for_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            ExpressionNode::literal_expression(tokens[4].clone(), Value::Integer(0)).wrapped(),
            ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(10)).wrapped(),
            ExpressionNode::variable_expression(tokens[8].clone(), var).wrapped(),
            vec![StatementNode::expression_statement(
                ExpressionNode::literal_expression(tokens[12].clone(), Value::Integer(2)).wrapped(),
            )
            .wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);
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

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            Vec::new(),
            None,
            Vec::new(),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
    }

    #[test]
    fn test_single_argument_function_definition() {
        let tokens = new_tokens([
            (TokenType::FunctionToken, "func"),
            (TokenType::IdentifierToken, "function"),
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::IdentifierToken, "a"), // 3
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

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            vec![(tokens[3].clone(), Type::Integer)],
            None,
            Vec::new(),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_top_level_statement().unwrap(), cmp);
    }

    #[test]
    fn test_multi_argument_function_definition() {
        let tokens = new_tokens([
            (TokenType::FunctionToken, "func"),
            (TokenType::IdentifierToken, "function"),
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::IdentifierToken, "a"), // 3
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::IdentifierToken, "integer"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "b"), // 8
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::IdentifierToken, "char"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "c"), // 13
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::IdentifierToken, "bool"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "d"), // 18
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::IdentifierToken, "float"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "e"), // 23
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IdentifierToken, "integer"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "f"), // 28
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::OpenSquareBraceToken, "["),
            (TokenType::IdentifierToken, "integer"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::CloseSquareBraceToken, "]"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::CommaToken, ","),
            (TokenType::IdentifierToken, "g"), // 37
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

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            vec![
                (tokens[3].clone(), Type::Integer),
                (tokens[8].clone(), Type::Character),
                (tokens[13].clone(), Type::Boolean),
                (tokens[18].clone(), Type::Float),
                (tokens[23].clone(), Type::Array(Box::new(Type::Integer))),
                (
                    tokens[30].clone(),
                    Type::Array(Box::new(Type::Array(Box::new(Type::Integer)))),
                ),
                (
                    tokens[39].clone(),
                    Type::Array(Box::new(Type::Array(Box::new(Type::Array(Box::new(
                        Type::Integer,
                    )))))),
                ),
            ],
            None,
            Vec::new(),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            Vec::new(),
            Some(Type::Integer),
            vec![StatementNode::return_statement(
                tokens[5].clone(),
                Some(
                    ExpressionNode::literal_expression(tokens[6].clone(), Value::Integer(12))
                        .wrapped(),
                ),
            )
            .wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            vec![(tokens[3].clone(), Type::Integer)],
            Some(Type::Integer),
            Vec::new(),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            Vec::new(),
            None,
            vec![StatementNode::return_statement(tokens[3].clone(), None).wrapped()],
        )
        .wrapped();

        let mut parser = Parser::new(tokens);

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
            (TokenType::IntegerLiteralToken, "5"), // 11
            (TokenType::PlusToken, "+"),
            (TokenType::IntegerLiteralToken, "5"),
            (TokenType::NewLineToken, "\n"),
            (TokenType::EndToken, "end"),
            (TokenType::OpenRoundBraceToken, "("),
            (TokenType::FunctionToken, "func"),
            (TokenType::CloseRoundBraceToken, ")"),
            (TokenType::NewLineToken, "\n"),
        ]);

        let cmp = StatementNode::function_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            vec![(tokens[3].clone(), Type::Integer)],
            Some(Type::Integer),
            vec![StatementNode::expression_statement(
                ExpressionNode::binary_expression(
                    ExpressionNode::literal_expression(tokens[11].clone(), Value::Integer(5))
                        .wrapped(),
                    tokens[12].clone(),
                    ExpressionNode::literal_expression(tokens[13].clone(), Value::Integer(5))
                        .wrapped(),
                )
                .wrapped(),
            )
            .wrapped()],
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp =
            StatementNode::struct_statement(tokens[0].clone(), tokens[1].clone(), HashMap::new())
                .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::struct_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            hashmap!["field_a".to_string() ; Type::Integer],
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::struct_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            hashmap![
                "field_a".to_string() ; Type::Integer,
                "field_b".to_string() ; Type::Boolean,
                "field_c".to_string() ; Type::Character,
                "field_d".to_string() ; Type::Float
            ],
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::struct_statement(
            tokens[0].clone(),
            tokens[1].clone(),
            hashmap![
                "field_a".to_string() ; Type::Integer,
                "field_b".to_string() ; Type::Struct {name: "string".to_string(), module: "std".to_string() }
            ],
        ).wrapped();
        let mut parser = Parser::new(tokens);

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
            ParserError::field_already_defined_for_struct("field_b".to_string(), tokens[1].clone());
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::pre_processor_command_statement(
            tokens[1].clone(),
            PreProcessorCommand::BeginModuleCommand(tokens[2].clone()),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::pre_processor_command_statement(
            tokens[1].clone(),
            PreProcessorCommand::ImportCommand(tokens[4].clone(), vec![tokens[2].clone()]),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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

        let cmp = StatementNode::pre_processor_command_statement(
            tokens[1].clone(),
            PreProcessorCommand::ImportCommand(
                tokens[6].clone(),
                vec![tokens[2].clone(), tokens[4].clone()],
            ),
        )
        .wrapped();
        let mut parser = Parser::new(tokens);

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
            (TokenType::HashToken, "#"),
            (TokenType::IdentifierToken, "a"),
            (TokenType::LeftArrowToken, "<-"), // 18
            (TokenType::IntegerLiteralToken, "5"),
            (TokenType::NewLineToken, "\n"),
            // New variable "b" set to 12
            (TokenType::HashToken, "#"),
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
        cmp.push_statement(
            StatementNode::pre_processor_command_statement(
                tokens[1].clone(),
                PreProcessorCommand::BeginModuleCommand(tokens[2].clone()),
            )
            .wrapped(),
        );

        cmp.push_statement(
            StatementNode::pre_processor_command_statement(
                tokens[5].clone(),
                PreProcessorCommand::ImportCommand(tokens[8].clone(), vec![tokens[6].clone()]),
            )
            .wrapped(),
        );

        let mut var_a_2 = Variable::new();
        var_a_2.push(tokens[30].clone());

        let mut var_b_2 = Variable::new();
        var_b_2.push(tokens[33].clone());

        cmp.push_statement(
            StatementNode::function_statement(
                tokens[11].clone(),
                tokens[12].clone(),
                Vec::new(),
                Some(Type::Integer),
                vec![
                    StatementNode::variable_declaration_statement(
                        tokens[16].clone(),
                        tokens[17].clone(),
                        ExpressionNode::literal_expression(tokens[19].clone(), Value::Integer(5))
                            .wrapped(),
                    )
                    .wrapped(),
                    StatementNode::variable_declaration_statement(
                        tokens[21].clone(),
                        tokens[22].clone(),
                        ExpressionNode::literal_expression(tokens[24].clone(), Value::Integer(12))
                            .wrapped(),
                    )
                    .wrapped(),
                    StatementNode::expression_statement(
                        ExpressionNode::call_expression(
                            tokens[26].clone(),
                            None,
                            tokens[27].clone(),
                            vec![ExpressionNode::binary_expression(
                                ExpressionNode::variable_expression(tokens[29].clone(), var_a_2)
                                    .wrapped(),
                                tokens[31].clone(),
                                ExpressionNode::variable_expression(tokens[32].clone(), var_b_2)
                                    .wrapped(),
                            )
                            .wrapped()],
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                ],
            )
            .wrapped(),
        );

        let parser = Parser::new(tokens);

        assert_eq!(parser.parse().unwrap(), cmp);
    }

    #[test]
    fn test_main_with_double_using_lexer() {
        let input = "%begin main\n\
        %import double from math\n\
        \n\
        func main -> int\n\
            #a <- 5\n\
            #b <- 12\n\
            @double($a + $b)\n\
        end(func)\n\
        ";

        let lexer = Lexer::new(input.chars().collect());
        let tokens = lexer.tokenize().unwrap();

        let mut cmp = AST::new();
        cmp.push_statement(
            StatementNode::pre_processor_command_statement(
                tokens[1].clone(),
                PreProcessorCommand::BeginModuleCommand(tokens[2].clone()),
            )
            .wrapped(),
        );

        cmp.push_statement(
            StatementNode::pre_processor_command_statement(
                tokens[5].clone(),
                PreProcessorCommand::ImportCommand(tokens[8].clone(), vec![tokens[6].clone()]),
            )
            .wrapped(),
        );

        let mut var_a_2 = Variable::new();
        var_a_2.push(tokens[30].clone());

        let mut var_b_2 = Variable::new();
        var_b_2.push(tokens[33].clone());

        cmp.push_statement(
            StatementNode::function_statement(
                tokens[11].clone(),
                tokens[12].clone(),
                Vec::new(),
                Some(Type::Integer),
                vec![
                    StatementNode::variable_declaration_statement(
                        tokens[16].clone(),
                        tokens[17].clone(),
                        ExpressionNode::literal_expression(tokens[19].clone(), Value::Integer(5))
                            .wrapped(),
                    )
                    .wrapped(),
                    StatementNode::variable_declaration_statement(
                        tokens[21].clone(),
                        tokens[22].clone(),
                        ExpressionNode::literal_expression(tokens[24].clone(), Value::Integer(12))
                            .wrapped(),
                    )
                    .wrapped(),
                    StatementNode::expression_statement(
                        ExpressionNode::call_expression(
                            tokens[26].clone(),
                            None,
                            tokens[27].clone(),
                            vec![ExpressionNode::binary_expression(
                                ExpressionNode::variable_expression(tokens[29].clone(), var_a_2)
                                    .wrapped(),
                                tokens[31].clone(),
                                ExpressionNode::variable_expression(tokens[32].clone(), var_b_2)
                                    .wrapped(),
                            )
                            .wrapped()],
                        )
                        .wrapped(),
                    )
                    .wrapped(),
                ],
            )
            .wrapped(),
        );

        let parser = Parser::new(tokens);

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

    let cmp = StatementNode::block_statement(
        Token::new(TokenType::BlockToken, "block".to_string(), 1, 1, None),
        vec![StatementNode::expression_statement(
            ExpressionNode::array_allocation_expression(
                Token::new(
                    TokenType::StringLiteralToken,
                    "string".to_string(),
                    2,
                    1,
                    None,
                ),
                ArrayLiteral::from(&"string".to_string()),
                ExpressionNode::literal_expression(
                    Token::new(
                        TokenType::StringLiteralToken,
                        "string".to_string(),
                        2,
                        1,
                        None,
                    ),
                    Value::Integer(6),
                )
                .wrapped(),
            )
            .wrapped(),
        )
        .wrapped()],
    )
    .wrapped();

    let mut parser = Parser::new(tokens);
    assert_eq!(parser.parse_statement().unwrap(), cmp);
}
