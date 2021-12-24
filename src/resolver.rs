use std::collections::{HashMap, HashSet};

use crate::error::ResolverError;
use crate::parser::ast::{Statement, Type};
use crate::pre_processor::{CompilableModule, ObjectName};
use crate::ROOT_MODULE_NAME;

type ResolverResult<T> = Result<T, ResolverError>;

/// Performs type checking and importing of the standard library.
pub struct Resolver {
    modules: HashMap<String, CompilableModule>,
    processed_modules: HashMap<String, CompilableModule>,
    type_checking: bool,
    name_checking: bool,
    definition_checking: bool,
    include_std_library: bool,
}

pub struct Resolution;

macro_rules! with_wrapper {
    ($method_name:ident, $n:ident, $tp:ty) => {
        pub fn $method_name(mut self, $n: $tp) -> Self {
            self.$n = $n;

            return self;
        }
    };
}

impl Resolver {
    pub fn new() -> Self {
        return Self {
            modules: HashMap::new(),
            processed_modules: HashMap::new(),
            type_checking: false,
            name_checking: false,
            definition_checking: false,
            include_std_library: false,
        };
    }

    with_wrapper!(with_type_checking, type_checking, bool);
    with_wrapper!(with_name_checking, name_checking, bool);
    with_wrapper!(with_definition_checking, definition_checking, bool);
    with_wrapper!(with_standard_library, include_std_library, bool);

    pub fn run(mut self) -> ResolverResult<Resolution> {
        if self.include_std_library {
            self.import_std_library_modules();
        }

        if self.name_checking || self.type_checking || self.definition_checking {
            return self.run_resolution();
        } else {
            return Ok(self.merge());
        }
    }

    fn run_resolution(mut self) -> ResolverResult<Resolution> {
        if self.modules.contains_key(ROOT_MODULE_NAME) {
            self.process_module(ROOT_MODULE_NAME.to_string())?;

            return Ok(self.merge());
        } else {
            return Err(ResolverError::NoRootModuleDefined);
        }
    }

    fn merge(mut self) -> Resolution {
        todo!();
    }

    fn process_module(&mut self, module_name: String) -> ResolverResult<()> {
        if let Some(module) = self.modules.remove(&module_name) {
            // Check high level imports
            let imports = self.import_check(&module)?;

            // Check every variable reference and function call types and names
            self.depth_resolution(&module, &imports)?;

            self.processed_modules
                .insert(module_name.to_string(), module);

            for import in imports {
                self.process_module(import)?;
            }
        } else {
            panic!();
        }

        todo!();
    }

    fn import_check(&mut self, module: &CompilableModule) -> ResolverResult<HashSet<String>> {
        fn handle_module_found(
            missing_modules_errors: &mut Vec<ResolverError>,
            required_modules: &mut HashSet<String>,
            import: &ObjectName,
            referenced_module: &CompilableModule,
        ) {
            if referenced_module
                .functions
                .contains_key(import.name.lexeme())
                && referenced_module.structs.contains_key(import.name.lexeme())
            {
                required_modules.insert(import.name.lexeme().clone());
            } else {
                missing_modules_errors.push(ResolverError::NoObjectDefined(
                    import.name.clone(),
                    import.module.clone(),
                ));
            }
        }

        let mut missing_modules_errors = Vec::new();
        let mut required_modules = HashSet::new();

        // Check the imports
        for import in &module.imports {
            if let Some(referenced_module) = self.modules.get(import.module.lexeme()) {
                handle_module_found(
                    &mut missing_modules_errors,
                    &mut required_modules,
                    import,
                    referenced_module,
                );
            } else if let Some(referenced_module) =
                self.processed_modules.get(import.module.lexeme())
            {
                handle_module_found(
                    &mut missing_modules_errors,
                    &mut required_modules,
                    import,
                    referenced_module,
                );
            } else {
                missing_modules_errors.push(ResolverError::NoModuleDefined(import.module.clone()));
            }
        }

        return Ok(required_modules);
    }

    fn encoded_fn_name(&mut self, fn_name: &String, module_name: &String) -> String {
        return format!(
            "__fn_{}_{}_{}",
            module_name.len() + fn_name.len(),
            &module_name,
            fn_name
        );
    }

    fn encoded_struct_name(&mut self, struct_name: &String, module_name: &String) -> String {
        return format!(
            "__struct_{}_{}_{}",
            module_name.len() + struct_name.len(),
            &module_name,
            struct_name
        );
    }

    fn import_std_library_modules(&mut self) {
        todo!();
    }

    fn depth_resolution(
        &mut self,
        module: &CompilableModule,
        imports: &HashSet<String>,
    ) -> ResolverResult<()> {
        // Check struct field types
        for (struct_name, struct_definition) in &module.structs {
            for (_name, field) in &struct_definition.fields {
                self.is_valid_type(&field.tp, Some(struct_name), &module.name, Some(imports))?;
            }
        }

        for (name, function_statement) in &module.functions {
            self.check_function(name, function_statement, &module.name)?;
        }

        return Ok(());
    }

    fn is_valid_type(
        &self,
        tp: &Type,
        current_struct: Option<&String>,
        current_module: &str,
        imports: Option<&HashSet<String>>,
    ) -> ResolverResult<()> {
        if let Type::Array(nested) = tp {
            return self.is_valid_type(nested, current_struct, current_module, imports);
        } else if let Type::Struct(name, module) = tp {
            let found_module = {
                if let Some(imports) = imports {
                    imports.contains(module) || module == current_module
                } else {
                    module == current_module
                }
            };

            if found_module {
                let m = self.lookup_module(module).unwrap();

                if m.structs.contains_key(name) {
                    return Ok(());
                } else {
                    if let Some(s) = current_struct {
                        return Err(ResolverError::NoModuleDefinedWithNameInStruct(
                            module.clone(),
                            s.clone(),
                        ));
                    } else {
                        return Err(ResolverError::NoModuleDefinedWithName(module.clone()));
                    }
                }
            } else {
                return Err(ResolverError::ModuleNotImported(
                    module.clone(),
                    current_module.to_string(),
                ));
            }
        } else {
            return Ok(());
        }
    }

    fn check_function(
        &self,
        function_name: &str,
        function_statement: &Statement,
        current_module: &str,
    ) -> ResolverResult<()> {
        todo!();

        return Ok(());
    }

    fn lookup_module(&self, name: &str) -> Option<&CompilableModule> {
        let mut m = self.processed_modules.get(name);

        if m.is_none() {
            m = self.modules.get(name);
        }

        return m;
    }
}

impl Default for Resolver {
    fn default() -> Self {
        return Self {
            modules: HashMap::new(),
            processed_modules: HashMap::new(),
            type_checking: true,
            name_checking: true,
            definition_checking: true,
            include_std_library: true,
        };
    }
}
