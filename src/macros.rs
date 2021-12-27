macro_rules! struct_enum_with_functional_inits {
    {$visibility:vis $([$($derive:ident),*])?  $name:ident { $($variant:ident $({ $($field_name:ident : $field_type:ty),* })?)* } } => {
        use paste::paste;

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
            paste! {
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
