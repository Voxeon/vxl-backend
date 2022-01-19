use super::*;

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

mod hex_to_unicode_char_tests {
    use super::*;

    #[test]
    fn test_hex_to_unicode_char_1() {
        let chars = ['f', '0', '9', 'f', 'a', '5', 'b', '0'];
        let char = Lexer::hex_to_unicode_char(chars, 0, 0, &None).unwrap();

        assert_eq!(char, '🥰');
    }

    #[test]
    fn test_hex_to_unicode_char_2() {
        let chars = ['6', '1', '0', '0', '0', '0', '0', '0'];
        let char = Lexer::hex_to_unicode_char(chars, 0, 0, &None).unwrap();

        assert_eq!(char, 'a');
    }
}

mod lexer_tests {
    use super::*;

    mod symbol_tokens {
        use super::*;

        macro_rules! new_char_test {
                ($($token_type:ident),*) => {
                    $(
                        paste::paste! {
                            #[test]
                            fn [<test_symbol_ $token_type:snake:lower>]() {
                                let input = TokenType::$token_type.to_string();
                                let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

                                assert_eq!(
                                    tokens,
                                    vec![Token::new(
                                        TokenType::$token_type,
                                        input,
                                        1,
                                        1,
                                        None
                                    )]
                                );
                            }
                        }
                    )*
                }
            }

        new_char_test!(
            EqualsToken,
            BangEqualsToken,
            GreaterThanToken,
            GreaterThanEqualToken,
            LessThanToken,
            LessThanEqualToken,
            NotToken,
            AndToken,
            OrToken,
            IdentifierToken,
            DollarToken,
            HashToken,
            AtToken,
            PlusToken,
            MinusToken,
            ForwardSlashToken,
            StarToken,
            PercentToken,
            LeftArrowToken,
            RightArrowToken,
            OpenRoundBraceToken,
            CloseRoundBraceToken,
            OpenSquareBraceToken,
            CloseSquareBraceToken,
            PipeToken,
            PeriodToken,
            CommaToken,
            ColonToken,
            SemiColonToken
        );
    }

    mod strings {
        use super::*;

        #[test]
        fn test_basic_string() {
            let input = "\"my string\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "my string".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_unterminated() {
            let input = "\"my string";
            let error = Lexer::new(input.chars().collect()).tokenize().unwrap_err();

            assert_eq!(
                error,
                LexerError::new(LexerErrorType::UnterminatedString, 1, 1, None)
            );
        }

        #[test]
        fn test_unicode_escape_1() {
            let input = "\"\\u{f09fa5b0}\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "🥰".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_unicode_escape_2() {
            let input = "\"\\u{61000000}\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "a".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_double_quote_escape() {
            let input = "\"\\\"\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "\"".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_single_quote_escape() {
            let input = "\"\\'\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "\'".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_newline_escape() {
            let input = "\"\\n\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "\n".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_tab_escape() {
            let input = "\"\\t\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "\t".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }
    }

    mod characters {
        use super::*;

        #[test]
        fn test_basic_character() {
            let input = "\'m\'";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::CharacterLiteralToken,
                    "m".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_unterminated() {
            let input = "\'";
            let error = Lexer::new(input.chars().collect()).tokenize().unwrap_err();

            assert_eq!(
                error,
                LexerError::new(LexerErrorType::UnterminatedCharacterLiteral, 1, 1, None)
            );
        }

        #[test]
        fn test_unicode_escape_1() {
            let input = "\'\\u{f09fa5b0}\'";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::CharacterLiteralToken,
                    "🥰".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_unicode_escape_2() {
            let input = "\'\\u{61000000}\'";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::CharacterLiteralToken,
                    "a".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_double_quote_escape() {
            let input = "\'\\\"\'";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::CharacterLiteralToken,
                    "\"".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_single_quote_escape() {
            let input = "\'\\'\'";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::CharacterLiteralToken,
                    "\'".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_newline_escape() {
            let input = "\"\\n\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "\n".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_tab_escape() {
            let input = "\"\\t\"";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::StringLiteralToken,
                    "\t".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }
    }

    mod numerics {
        use super::*;

        #[test]
        fn test_integer_literal() {
            let input = "512";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::IntegerLiteralToken,
                    "512".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_double_literal() {
            let input = "512.0";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::FloatLiteralToken,
                    "512.0".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_integer_literal_with_underscores() {
            let input = "512_000";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::IntegerLiteralToken,
                    "512000".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }

        #[test]
        fn test_double_literal_with_underscores() {
            let input = "512_000.0";
            let tokens = Lexer::new(input.chars().collect()).tokenize().unwrap();

            assert_eq!(
                tokens,
                vec![Token::new(
                    TokenType::FloatLiteralToken,
                    "512000.0".to_string(),
                    1,
                    1,
                    None,
                )]
            );
        }
    }

    mod identifiers {
        use super::*;
        use paste::paste;
        use TokenType::*;

        macro_rules! define_identifier_tests {
                ($(($tp:expr, $name:ident)),*) => {
                    $(
                        paste! {
                            #[test]
                            fn [<test_token_$name>]() {
                                let input = stringify!($name);
                                let tokens = Lexer::new(input.chars().collect())
                                    .tokenize()
                                    .unwrap();

                                assert_eq!(
                                    tokens,
                                    vec![Token::new(
                                        $tp,
                                        stringify!($name).to_string(),
                                        1,
                                        1,
                                        None,

                                    )]
                                );
                            }
                        }
                    )*
                };
            }

        define_identifier_tests!(
            (FunctionToken, func),
            (StructToken, struct),
            (AndToken, and),
            (OrToken, or),
            (EndToken, end),
            (WhileToken, while),
            (IfToken, if),
            (ElseToken, else),
            (ForToken, for),
            (ConstToken, const),
            (NotToken, not),
            (TrueToken, true),
            (FalseToken, false),
            (BlockToken, block),
            (ReturnToken, return)
        );
    }

    mod larger_inputs {
        use std::cmp::max;

        use super::*;

        #[test]
        fn test_main_with_call() {
            let input = "%begin main\n\
                %import double from math\n\
                // comment \n\
                \n\
                func main -> int\n\
                    #a <- 5\n\
                    #b <- 12\n\
                    @double($a + $b)\n\
                end(func)\n\
                ";

            let lexer = Lexer::new(input.chars().collect());
            let tokens: Vec<TokenType> = lexer
                .tokenize()
                .unwrap()
                .into_iter()
                .map(|t| t.token_type())
                .collect();

            let cmp: Vec<TokenType> = new_tokens([
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
            ])
            .into_iter()
            .map(|t| t.token_type())
            .collect();

            for i in 0..max(tokens.len(), cmp.len()) {
                if i > tokens.len() {
                    panic!(
                        "cmp > tokens. i: {}\nTokens: {:?}\n Comparison: {:?}",
                        i, tokens, cmp
                    );
                } else if i > cmp.len() {
                    panic!(
                        "tokens > cmp. i: {}\nTokens: {:?}\n Comparison: {:?}",
                        i, tokens, cmp
                    );
                } else {
                    assert_eq!(
                        tokens[i], cmp[i],
                        "tokens[{}] = {:?}, cmp[{}] = {:?}",
                        i, tokens[i], i, cmp[i]
                    );
                }
            }

            assert_eq!(tokens, cmp);
        }

        #[test]
        fn test_main_with_call_bottom_comment() {
            let input = "%begin main\n\
                %import double from math\n\
                \n\
                func main -> int\n\
                    #a <- 5\n\
                    #b <- 12\n\
                    @double($a + $b)\n\
                end(func)\n\
                // comment \n\
                ";

            let lexer = Lexer::new(input.chars().collect());
            let tokens: Vec<TokenType> = lexer
                .tokenize()
                .unwrap()
                .into_iter()
                .map(|t| t.token_type())
                .collect();

            let cmp: Vec<TokenType> = new_tokens([
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
            ])
            .into_iter()
            .map(|t| t.token_type())
            .collect();

            for i in 0..max(tokens.len(), cmp.len()) {
                if i > tokens.len() {
                    panic!(
                        "cmp > tokens. i: {}\nTokens: {:?}\n Comparison: {:?}",
                        i, tokens, cmp
                    );
                } else if i > cmp.len() {
                    panic!(
                        "tokens > cmp. i: {}\nTokens: {:?}\n Comparison: {:?}",
                        i, tokens, cmp
                    );
                } else {
                    assert_eq!(
                        tokens[i], cmp[i],
                        "tokens[{}] = {:?}, cmp[{}] = {:?}",
                        i, tokens[i], i, cmp[i]
                    );
                }
            }

            assert_eq!(tokens, cmp);
        }
    }
}
