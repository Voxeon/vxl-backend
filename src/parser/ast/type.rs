#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Character,
    Struct(String, String), // name, module
    Array(Box<Type>),
}

pub trait TypeSize {
    /// Return the size of a value in bytes.
    fn size(&self) -> u64;
}

const POINTER_SIZE: u64 = 8; // bytes

impl Type {
    pub fn is_same_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Integer, Type::Integer) => {
                return true;
            }
            (Type::Float, Type::Float) => {
                return true;
            }
            (Type::Character, Type::Character) => {
                return true;
            }
            (Type::Boolean, Type::Boolean) => {
                return true;
            }
            (Type::Struct(self_val, self_module), Type::Struct(other_val, other_module)) => {
                return self_val == other_val && self_module == other_module;
            }
            (Type::Array(self_val), Type::Array(other_val)) => {
                return self_val.is_same_type(other_val);
            }
            _ => return false,
        }
    }
}

impl TypeSize for Type {
    fn size(&self) -> u64 {
        return match self {
            Type::Boolean => 1,
            Type::Integer => 8,
            Type::Float => 8,
            Type::Character => 4,
            Type::Struct(_, _) => POINTER_SIZE,
            Type::Array(_) => POINTER_SIZE,
        };
    }
}
