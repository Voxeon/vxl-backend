macro_rules! struct_enum_with_functional_inits {
    {$visibility:vis $([$($derive:ident),*])?  $name:ident { $($variant:ident $({ $($field_name:ident : $field_type:ty),* })?)* } } => {
        $(#[derive($($derive),*)])?

        $visibility enum $name {
            $(
                $variant $({
                    $(
                        $field_name : $field_type
                    ),*
                })?
            ),*
        }

        impl $name {
            paste::paste! {
                $(
                    #[allow(dead_code)]
                    pub fn [<$variant:snake:lower>]($($($field_name : $field_type),*)?) -> Self {
                        return Self::$variant $({
                                $(
                                    $field_name
                                ),*
                        })?;
                    }

                    #[allow(dead_code)]
                    pub fn [<is_ $variant:snake:lower>](&self) -> bool {
                        return match self {
                            Self::$variant { .. } => true,
                            _ => false,
                        };
                    }
                )*
            }
        }
    };
}

macro_rules! generate_is_variants {
    ($($($variant_name:ident),*,)? $(($($tuple_variant_name:ident),*),)? $({$($struct_variant_name:ident),*})?) => {
        $($(
            paste::paste! {
                pub fn [<is_ $variant_name:snake:lower>](&self) -> bool {
                    return match self {
                        Self::$variant_name => true,
                        _ => false,
                    };
                }
            }
        )*)?

        $($(
            paste::paste! {
                pub fn [<is_ $tuple_variant_name:snake:lower>](&self) -> bool {
                    return match self {
                        Self::$tuple_variant_name( .. ) => true,
                        _ => false,
                    };
                }
            }
        )*)?

        $($(
            paste::paste! {
                pub fn [<is_ $struct_variant_name:snake:lower>](&self) -> bool {
                    return match self {
                        Self::$struct_variant_name { .. } => true,
                        _ => false,
                    };
                }
            }
        )*)?
    }
}

macro_rules! with_wrapper {
    ($method_name:ident, $n:ident, $tp:ty) => {
        pub fn $method_name(mut self, $n: $tp) -> Self {
            self.$n = $n;

            return self;
        }
    };
}

/// Wrapper for panic.
macro_rules! internal_error {
    ($msg:expr) => {
        panic!("Internal Compiler Panic: {}", $msg)
    };
}

// Used in testing
#[allow(unused_macros)]
macro_rules! hashmap {
    [$($k:expr ; $v:expr),*] => {
        vec![$(($k, $v)),*].into_iter().collect()
    };
}

// Used in testing
#[allow(unused_macros)]
macro_rules! assert_eq_one {
    ($lhs:expr, $($rhs:expr),+) => {
        if !($(
            $lhs == $rhs
        ) || +) {
            panic!("assertion failed: `(left == right) && (right == left)` \n\tleft: `{:?}`", $lhs);
        }
    };
}
