use crate::Token;
use crate::ROOT_MODULE_NAME;

use std::error::Error as ErrorTrait;
use std::fmt;

struct_enum_with_functional_inits! {
    pub
    [Clone, Debug, PartialEq, Eq, Hash]
    PreProcessorError {
        NoRootModuleDefined
        ModuleAlreadyDefined {
            new_module: Token,
            original_module: Token
        }
        NoCurrentModuleDefined {
            reference_token: Token
        }
    }
}

impl ErrorTrait for PreProcessorError {}

impl fmt::Display for PreProcessorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            PreProcessorError::NoRootModuleDefined => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart the first file with `%begin {}`",
                    ROOT_MODULE_NAME, ROOT_MODULE_NAME
                )
            }
            PreProcessorError::ModuleAlreadyDefined {
                new_module,
                original_module,
            } => {
                write!(
                    f,
                    "Module \"{}\" already defined here {}.",
                    new_module, original_module
                )
            }
            PreProcessorError::NoCurrentModuleDefined { reference_token } => {
                write!(
                    f,
                    "No \"{}\" module defined.\nStart a new module with `%begin`. {}",
                    ROOT_MODULE_NAME, reference_token
                )
            }
        };
    }
}
