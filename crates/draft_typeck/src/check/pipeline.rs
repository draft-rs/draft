/// Imports
use crate::cx::module::ModuleCx;
use crate::typ::typ::Module;
use draft_ast::untyped::Item;
use tracing::info;

/// Implementation
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Runs pipelined check on the module
    ///
    /// Pipeline stages:
    /// 1. Perform imports.
    /// 2. Early define types by name.
    /// 3. Early define and analyze functions.
    /// 4. Late analyze declarations.
    ///
    /// After this call, the module is fully type-checked.
    ///
    pub(crate) fn pipeline(&mut self) -> Module {
        // 1. Performing imports
        info!("Performing imports...");
        for import in self.module.imports.clone() {
            self.perform_import(import)
        }

        // 2. Early definitions of types
        info!("Performing early type definitions.");
        for item in &self.module.items {
            if let Item::Struct(t) = item {
                self.early_define_struct(t)
            } else if let Item::Enum(t) = item {
                self.early_define_enum(t)
            }
        }

        // 3. Early functions definitions
        info!("Performing early function definitions.");
        for item in &self.module.items {
            if let Item::Fn(f) = item {
                self.early_define_fn(f)
            } else if let Item::ExternFn(f) = item {
                self.early_define_extern_fn(f)
            }
        }

        // 4. Late analysis
        info!("Performing late analysis...");
        for item in self.module.items.clone() {
            self.late_analyze_item(item);
        }

        // Pipeline result
        Module {
            source: self.module.source.clone(),
            name: self.module_name.clone(),
            fields: self.resolver.collect(),
        }
    }
}
