use super::Type;
use std::convert::TryFrom;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Character(char),
    Boolean(bool),
    Array(Box<ArrayLiteral>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayLiteral {
    tp: Type,
    contents: Vec<Value>,
}

impl Value {
    pub fn get_type(&self) -> Type {
        return match self {
            Value::Integer(_) => Type::Integer,
            Value::Float(_) => Type::Float,
            Value::Character(_) => Type::Character,
            Value::Boolean(_) => Type::Boolean,
            Value::Array(c) => Type::Array(Box::new(c.get_type())),
        };
    }
}

impl ArrayLiteral {
    pub fn new(tp: Type) -> Self {
        return Self {
            tp,
            contents: Vec::new(),
        };
    }

    pub fn get_type(&self) -> Type {
        return self.tp.clone();
    }

    pub fn get_type_ref(&self) -> &Type {
        return &self.tp;
    }

    pub fn contents(&self) -> &Vec<Value> {
        return &self.contents;
    }
}

impl TryFrom<Vec<Value>> for ArrayLiteral {
    type Error = &'static str;

    fn try_from(values: Vec<Value>) -> Result<Self, Self::Error> {
        if values.is_empty() {
            return Err("Empty array literal");
        }

        let tp = values[0].get_type();

        for i in 1..values.len() {
            if !values[i].get_type().is_same_type(&tp) {
                return Err("All elements of an array literal must be matching.");
            }
        }

        return Ok(Self {
            tp,
            contents: values,
        });
    }
}

impl From<&String> for ArrayLiteral {
    fn from(s: &String) -> Self {
        return Self {
            contents: s.chars().map(|ch| Value::Character(ch)).collect(),
            tp: Type::Character,
        };
    }
}
