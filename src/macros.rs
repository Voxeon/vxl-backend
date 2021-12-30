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

macro_rules! with_wrapper {
    ($method_name:ident, $n:ident, $tp:ty) => {
        pub fn $method_name(mut self, $n: $tp) -> Self {
            self.$n = $n;

            return self;
        }
    };
}

// Used in testing
#[allow(unused_macros)]
macro_rules! hashmap {
    [$($k:expr ; $v:expr),*] => {
        vec![$(($k, $v)),*].into_iter().collect()
    };
}
