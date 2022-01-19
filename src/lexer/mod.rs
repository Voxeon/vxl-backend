use std::convert::TryInto;

use crate::error::{LexerError, LexerErrorType};
use crate::{Token, TokenType};

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
                '#' => self.add_token::<1>(TokenType::HashToken),
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
                '|' => self.add_token::<1>(TokenType::PipeToken),
                '+' => self.add_token::<1>(TokenType::PlusToken),
                '-' => match self.peek().map(|c| *c) {
                    Some('>') => {
                        self.add_token::<2>(TokenType::RightArrowToken);
                    }
                    Some(_) | None => {
                        self.add_token::<1>(TokenType::MinusToken);
                    }
                },
                '/' => {
                    if self.peek().map(|c| *c) == Some('/') {
                        self.increment_col::<1>();

                        //process comment
                        while !matches!(self.current_char().map(|c| *c), Some('\n') | None) {
                            self.increment_col::<1>();
                        }

                        if self.current_char().map(|c| *c) == Some('\n') {
                            self.increment_col::<1>();
                            self.line += 1;
                            self.column = 1;
                        }
                    } else {
                        self.add_token::<1>(TokenType::ForwardSlashToken)
                    }
                }
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
                        return Err(LexerError::new(
                            LexerErrorType::expected("=".to_string(), "!".to_string()),
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
                        return Err(LexerError::new(
                            LexerErrorType::unexpected_character(character),
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
                    return Err(LexerError::new(
                        LexerErrorType::UnterminatedString,
                        line,
                        col,
                        self.file.clone(),
                    ));
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
                return Err(LexerError::new(
                    LexerErrorType::UnterminatedString,
                    line,
                    col,
                    self.file.clone(),
                ));
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
                return Err(LexerError::new(
                    LexerErrorType::EmptyCharacterLiteral,
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
                return Err(LexerError::new(
                    LexerErrorType::UnterminatedCharacterLiteral,
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
                return Err(LexerError::new(
                    LexerErrorType::unexpected_character_expected(*ch, "\'".to_string()),
                    self.line,
                    self.column,
                    self.file.clone(),
                ));
            }
            None => {
                return Err(LexerError::new(
                    LexerErrorType::UnterminatedCharacterLiteral,
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
            TokenType::FloatLiteralToken
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
                    return Err(LexerError::new(
                        LexerErrorType::InvalidUnicodeEscapeSequenceLength,
                        line,
                        col,
                        self.file.clone(),
                    ));
                }

                let chars = self.consume_characters::<10>();
                if chars[0] != '{' {
                    todo!();
                } else if chars[9] != '}' {
                    todo!();
                } else {
                    let characters = chars[1..=8].try_into().unwrap();
                    Self::hex_to_unicode_char(characters, line, col, &self.file)?
                }
            }
            Some(_) | None => todo!(),
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
            return Err(LexerError::new(
                LexerErrorType::invalid_unicode_escape_sequence(string),
                line,
                col,
                file.clone(),
            ));
        }

        let string = std::str::from_utf8(&unicode).map_err(|_| {
            LexerError::new(
                LexerErrorType::invalid_unicode_escape_sequence(string),
                line,
                col,
                file.clone(),
            )
        })?;

        if string.chars().size_hint().0 != 1 {
            return Err(LexerError::new(
                LexerErrorType::invalid_unicode_escape_sequence(string.to_string()),
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
mod tests;
