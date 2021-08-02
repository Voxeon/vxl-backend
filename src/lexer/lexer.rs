use std::convert::TryInto;

use super::token::*;
use crate::error::LexerError;6

#[derive(Debug)]
pub struct Lexer {
    characters: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
    file: Option<String>,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new_string(input: &str) -> Self {
        return Self::new(input.chars().collect());
    }

    pub fn new(characters: Vec<char>) -> Self {
        return Self {
            characters,
            index: 0,
            line: 1,
            column: 1,
            file: None,
            tokens: Vec::new(),
        };
    }

    pub fn set_file(&mut self, file: String) {
        self.file = Some(file);
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexerError> {
        while self.index < self.characters.len() {
            let character = self.characters[self.index];

            match character {
                '\n' => {
                    self.add_token::<1>(TokenType::NewLineToken);
                    self.line += 1;
                    self.column = 1;
                }
                '.' => self.add_token::<1>(TokenType::PeriodToken),
                ':' => self.add_token::<1>(TokenType::ColonToken),
                ';' => self.add_token::<1>(TokenType::SemiColonToken),
                ',' => self.add_token::<1>(TokenType::CommaToken),
                '@' => self.add_token::<1>(TokenType::AtToken),
                '%' => self.add_token::<1>(TokenType::PercentToken),
                '$' => self.add_token::<1>(TokenType::DollarToken),
                '(' => self.add_token::<1>(TokenType::OpenRoundBraceToken),
                ')' => self.add_token::<1>(TokenType::CloseRoundBraceToken),
                '[' => self.add_token::<1>(TokenType::OpenSquareBraceToken),
                ']' => self.add_token::<1>(TokenType::CloseSquareBraceToken),
                '+' => self.add_token::<1>(TokenType::PlusToken),
                '-' => match self.peek().map(|c| *c) {
                    Some('>') => {
                        self.add_token::<2>(TokenType::RightArrowToken);
                    }
                    Some(_) | None => {
                        self.add_token::<1>(TokenType::MinusToken);
                    }
                },
                '/' => self.add_token::<1>(TokenType::BackslashToken),
                '*' => self.add_token::<1>(TokenType::StarToken),
                '>' => match self.peek().map(|c| *c) {
                    Some('=') => {
                        self.add_token::<2>(TokenType::GreaterThanEqualToken);
                    }
                    Some(_) | None => {
                        self.add_token::<1>(TokenType::GreaterThanToken);
                    }
                },
                '<' => match self.peek().map(|c| *c) {
                    Some('=') => {
                        self.add_token::<2>(TokenType::LessThanEqualToken);
                    }
                    Some('-') => {
                        self.add_token::<2>(TokenType::LeftArrowToken);
                    }
                    Some(_) | None => {
                        self.add_token::<1>(TokenType::LessThanToken);
                    }
                },
                '!' => match self.peek().map(|c| *c) {
                    Some('=') => {
                        self.add_token::<2>(TokenType::BangEqualsToken);
                    }
                    Some(_) | None => {
                        return Err(LexerError::Expected(
                            "=".to_string(),
                            "!".to_string(),
                            self.line,
                            self.column,
                            self.file,
                        ));
                    }
                },
                '=' => self.add_token::<1>(TokenType::EqualsToken),
                '_' => self.handle_identifier(),
                '"' => self.handle_string()?,
                '\'' => self.handle_character()?,
                _ => {
                    if character.is_alphabetic() {
                        self.handle_identifier();
                    } else if character.is_numeric() {
                        self.handle_numeric()?;
                    } else if character.is_whitespace() {
                        self.increment_col::<1>();
                    } else {
                        return Err(LexerError::UnexpectedCharacter(
                            character,
                            self.line,
                            self.column,
                            self.file,
                        ));
                    }
                }
            }
        }

        return Ok(self.tokens);
    }

    fn add_token<const N: usize>(&mut self, tp: TokenType) {
        let chars = self.consume_characters::<N>();

        if tp == TokenType::NewLineToken {
            self.tokens.push(Token::new(
                tp,
                chars.iter().collect(),
                self.line,
                self.column - N,
                self.file.clone(),
            ));

            self.line += 1;
            self.column = 1;
        } else {
            self.tokens.push(Token::new(
                tp,
                chars.iter().collect(),
                self.line,
                self.column - N,
                self.file.clone(),
            ));
        }
    }

    fn consume_characters<const N: usize>(&mut self) -> [char; N] {
        let mut res = [' '; N];

        for i in 0..N {
            res[i] = self.characters[self.index + i];
        }

        self.increment_col::<N>();

        return res;
    }

    fn handle_identifier(&mut self) {
        let mut ident = String::new();
        let col = self.column;

        loop {
            let current_char = match self.current_char() {
                Some(ch) => *ch,
                None => break,
            };

            if !current_char.is_alphanumeric() && current_char != '_' {
                break;
            }

            ident.push(current_char);
            self.consume_characters::<1>();
        }

        self.tokens.push(Token::new_identifier(
            ident,
            self.line,
            col,
            self.file.clone(),
        ));
    }

    fn handle_string(&mut self) -> Result<(), LexerError> {
        let mut s = String::new();

        let line = self.line;
        let col = self.column;
        self.increment_col::<1>(); // Consume opening "

        loop {
            if let Some(next) = self.current_char() {
                let next = *next;

                if next == '\n' {
                    return Err(LexerError::UnterminatedString(line, col, self.file.clone()));
                } else if next == '"' {
                    self.increment_col::<1>();
                    break;
                } else if next == '\\' {
                    s.push(self.consume_escape_sequence()?);
                    continue;
                }

                self.increment_col::<1>();
                s.push(next);
            } else {
                return Err(LexerError::UnterminatedString(line, col, self.file.clone()));
            }
        }

        self.tokens.push(Token::new(
            TokenType::StringLiteralToken,
            s,
            line,
            col,
            self.file.clone(),
        ));

        return Ok(());
    }

    fn handle_character(&mut self) -> Result<(), LexerError> {
        let s;

        let line = self.line;
        let col = self.column;
        self.increment_col::<1>(); // Consume opening '

        match self.current_char() {
            Some('\'') => {
                return Err(LexerError::EmptyCharacterLiteral(
                    line,
                    col,
                    self.file.clone(),
                ));
            }
            Some('\\') => {
                s = self.consume_escape_sequence()?.to_string();
            }
            Some(ch) => {
                s = ch.to_string();
                self.consume_characters::<1>();
            }
            None => {
                return Err(LexerError::UnterminatedCharacterLiteral(
                    line,
                    col,
                    self.file.clone(),
                ));
            }
        }

        match self.current_char() {
            Some('\'') => {
                self.consume_characters::<1>();
            }
            Some(ch) => {
                return Err(LexerError::UnexpectedCharacterExpected(
                    *ch,
                    "\'".to_string(),
                    self.line,
                    self.column,
                    self.file.clone(),
                ));
            }
            None => {
                return Err(LexerError::UnterminatedCharacterLiteral(
                    line,
                    col,
                    self.file.clone(),
                ));
            }
        }

        self.tokens.push(Token::new(
            TokenType::CharacterLiteralToken,
            s,
            line,
            col,
            self.file.clone(),
        ));

        return Ok(());
    }

    fn handle_numeric(&mut self) -> Result<(), LexerError> {
        let line = self.line;
        let col = self.column;

        let mut s: String = self.consume_characters::<1>().iter().collect();
        let mut double = false;

        while let Some(nxt) = self.current_char() {
            if nxt.is_numeric() {
                s.push(*nxt);
                self.consume_characters::<1>();
            } else if nxt == &'.' && !double {
                double = true;
                s.push(*nxt);
                self.consume_characters::<1>();
            } else if nxt == &'_' {
                self.consume_characters::<1>();
            } else {
                break;
            }
        }

        let tp = if double {
            TokenType::DoubleLiteralToken
        } else {
            TokenType::IntegerLiteralToken
        };

        self.tokens
            .push(Token::new(tp, s, line, col, self.file.clone()));

        return Ok(());
    }

    fn consume_escape_sequence(&mut self) -> Result<char, LexerError> {
        let line = self.line;
        let col = self.column;

        self.increment_col::<1>();

        return Ok(match self.current_char() {
            Some('n') => {
                self.increment_col::<1>();
                '\n'
            }
            Some('t') => {
                self.increment_col::<1>();
                '\t'
            }
            Some('\'') => {
                self.increment_col::<1>();
                '\''
            }
            Some('"') => {
                self.increment_col::<1>();
                '"'
            }
            Some('r') => {
                self.increment_col::<1>();
                '\r'
            }
            Some('u') => {
                self.increment_col::<1>();

                if self.index + 10 >= self.characters.len() {
                    return Err(LexerError::InvalidUnicodeEscapeSequenceLength(
                        line,
                        col,
                        self.file.clone(),
                    ));
                }

                let chars = self.consume_characters::<10>();
                if chars[0] != '{' {
                    panic!();
                } else if chars[9] != '}' {
                    panic!();
                } else {
                    let characters = chars[1..=8].try_into().unwrap();
                    Self::hex_to_unicode_char(characters, line, col, &self.file)?
                }
            }
            Some(_) | None => panic!(),
        });
    }

    fn hex_to_unicode_char(
        chars: [char; 8],
        line: usize,
        col: usize,
        file: &Option<String>,
    ) -> Result<char, LexerError> {
        let string = chars.iter().collect::<String>();

        let unicode;

        if let Ok(u) = hex::decode(&string) {
            unicode = u;
        } else {
            return Err(LexerError::InvalidUnicodeEscapeSequence(
                string,
                line,
                col,
                file.clone(),
            ));
        }

        let string = std::str::from_utf8(&unicode).map_err(|_| {
            LexerError::InvalidUnicodeEscapeSequence(string, line, col, file.clone())
        })?;

        if string.chars().size_hint().0 != 1 {
            return Err(LexerError::InvalidUnicodeEscapeSequence(
                string.to_string(),
                line,
                col,
                file.clone(),
            ));
        } else {
            return Ok(string.chars().nth(0).unwrap());
        }
    }

    fn increment_col<const N: usize>(&mut self) {
        self.index += N;
        self.column += N;
    }

    fn current_char(&self) -> Option<&char> {
        if self.index >= self.characters.len() {
            return None;
        }

        return Some(&self.characters[self.index]);
    }

    fn peek(&self) -> Option<&char> {
        if self.index + 1 >= self.characters.len() {
            return None;
        }

        return Some(&self.characters[self.index + 1]);
    }
}

#[cfg(test)]
mod tests {
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

                assert_eq!(error, LexerError::UnterminatedString(1, 1, None));
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

                assert_eq!(error, LexerError::UnterminatedCharacterLiteral(1, 1, None));
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
                        TokenType::DoubleLiteralToken,
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
                        TokenType::DoubleLiteralToken,
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
            use super::*;

            #[test]
            fn test_main_with_call() {
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
                ])
                .into_iter()
                .map(|t| t.token_type())
                .collect();

                assert_eq!(tokens, cmp);
            }
        }
    }
}
