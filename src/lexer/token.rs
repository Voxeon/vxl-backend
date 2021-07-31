use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
    column: usize,
    file: Option<String>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum TokenType {
    EqualsToken,
    BangEqualsToken,
    GreaterThanToken,
    GreaterThanEqualToken,
    LessThanToken,
    LessThanEqualToken,
    CharacterLiteralToken,
    StringLiteralToken,
    IntegerLiteralToken,
    DoubleLiteralToken,
    NotToken,
    AndToken,
    OrToken,
    IdentifierToken,
    DollarToken,
    AtToken,
    PlusToken,
    MinusToken,
    BackslashToken,
    StarToken,
    PercentToken,
    FunctionToken,
    StructToken,
    EndToken,
    BlockToken,
    IfToken,
    ElseToken,
    WhileToken,
    ForToken,
    LeftArrowToken,
    RightArrowToken,
    OpenRoundBraceToken,
    CloseRoundBraceToken,
    OpenSquareBraceToken,
    CloseSquareBraceToken,
    ConstToken,
    TrueToken,
    FalseToken,
    PeriodToken,
    CommaToken,
    ColonToken,
    SemiColonToken,
    NewLineToken,
    ReturnToken,
}

impl Token {
    pub fn new_identifier(
        lexeme: String,
        line: usize,
        column: usize,
        file: Option<String>,
    ) -> Self {
        return Self::new(TokenType::from(&lexeme), lexeme, line, column, file);
    }

    pub fn new(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        column: usize,
        file: Option<String>,
    ) -> Self {
        return Self {
            token_type,
            lexeme,
            line,
            column,
            file,
        };
    }

    pub fn lexeme(&self) -> &String {
        return &self.lexeme;
    }

    pub fn line(&self) -> usize {
        return self.line;
    }

    pub fn column(&self) -> usize {
        return self.column;
    }

    pub fn file(&self) -> &Option<String> {
        return &self.file;
    }

    pub fn token_type(&self) -> TokenType {
        return self.token_type;
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(file) = &self.file {
            return write!(
                f,
                "{}\n{} ({}, {})",
                self.lexeme, file, self.line, self.column
            );
        } else {
            return write!(f, "{} ({}, {})", self.lexeme, self.line, self.column);
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TokenType::EqualsToken => "=",
            TokenType::BangEqualsToken => "!=",
            TokenType::GreaterThanToken => ">",
            TokenType::GreaterThanEqualToken => ">=",
            TokenType::LessThanToken => "<",
            TokenType::LessThanEqualToken => "<=",
            TokenType::CharacterLiteralToken => "character",
            TokenType::StringLiteralToken => "string",
            TokenType::IntegerLiteralToken => "integer",
            TokenType::DoubleLiteralToken => "double",
            TokenType::NotToken => "not",
            TokenType::AndToken => "and",
            TokenType::OrToken => "or",
            TokenType::IdentifierToken => "identifier",
            TokenType::DollarToken => "$",
            TokenType::AtToken => "@",
            TokenType::PlusToken => "+",
            TokenType::MinusToken => "-",
            TokenType::BackslashToken => "\\",
            TokenType::StarToken => "*",
            TokenType::PercentToken => "%",
            TokenType::FunctionToken => "func",
            TokenType::StructToken => "struct",
            TokenType::EndToken => "end",
            TokenType::BlockToken => "block",
            TokenType::IfToken => "if",
            TokenType::ElseToken => "else",
            TokenType::WhileToken => "while",
            TokenType::ForToken => "for",
            TokenType::LeftArrowToken => "<-",
            TokenType::RightArrowToken => "->",
            TokenType::OpenRoundBraceToken => "(",
            TokenType::CloseRoundBraceToken => ")",
            TokenType::OpenSquareBraceToken => "[",
            TokenType::CloseSquareBraceToken => "]",
            TokenType::ConstToken => "const",
            TokenType::TrueToken => "true",
            TokenType::FalseToken => "false",
            TokenType::ColonToken => ":",
            TokenType::SemiColonToken => ";",
            TokenType::PeriodToken => ".",
            TokenType::CommaToken => ",",
            TokenType::NewLineToken => "new line",
            TokenType::ReturnToken => "return",
        };

        return write!(f, "{}", s);
    }
}

impl From<&str> for TokenType {
    fn from(s: &str) -> Self {
        return match s {
            "and" => Self::AndToken,
            "or" => Self::OrToken,
            "struct" => Self::StructToken,
            "end" => Self::EndToken,
            "func" => Self::FunctionToken,
            "while" => Self::WhileToken,
            "if" => Self::IfToken,
            "else" => Self::ElseToken,
            "for" => Self::ForToken,
            "const" => Self::ConstToken,
            "not" => Self::NotToken,
            "false" => Self::FalseToken,
            "true" => Self::TrueToken,
            "block" => Self::BlockToken,
            "return" => Self::ReturnToken,
            _ => Self::IdentifierToken,
        };
    }
}

impl From<&String> for TokenType {
    fn from(s: &String) -> Self {
        return Self::from(s.as_str());
    }
}
