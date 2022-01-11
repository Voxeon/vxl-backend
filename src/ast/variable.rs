use crate::Token;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Variable {
    path: Vec<Token>,
}

impl Variable {
    pub fn new() -> Self {
        return Self { path: Vec::new() };
    }

    pub fn push(&mut self, token: Token) {
        self.path.push(token);
    }

    pub fn path(&self) -> &Vec<Token> {
        return &self.path;
    }
}

impl Into<Vec<Token>> for Variable {
    fn into(self) -> Vec<Token> {
        return self.path;
    }
}
