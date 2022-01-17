use super::*;

use std::collections::VecDeque;

use crate::ast::{StatementNode, Value, AST};
use crate::pre_processor::{PreProcessor, PreProcessorCommand};
use crate::Token;
use crate::ROOT_MODULE_NAME;

//TODO: Determine where to enforce a main function requirement

#[cfg(not(feature = "test-intensive-short"))]
#[cfg(feature = "test-intensive")]
const INTENSIVE_TEST_CYCLES: usize = 100_000;

#[cfg(feature = "test-intensive-short")]
const INTENSIVE_TEST_CYCLES: usize = 1000;

#[cfg(feature = "test-intensive")]
macro_rules! define_intensive_test {
    ($method_name:ident) => {
        paste::paste! {
            #[test]
            fn [<intensive_ $method_name>]() {
                for i in 0..INTENSIVE_TEST_CYCLES {
                    println!("{} cycle {}", stringify!([<intensive_ $method_name>]), i);
                    $method_name();
                }
            }
        }
    };
}

mod logic_tests;
mod statement_tests;
