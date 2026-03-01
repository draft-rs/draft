/// Modules
mod functions;
mod types;

/// Imports
use crate::{
    cx::module::ModuleCx,
    errors::TypeckError,
    inference::{
        cause::Cause,
        coercion::{self, Coercion},
    },
    typ::{def::ModuleDef, typ::WithPublicity},
};
use draft_ast::untyped::{self, Import, ImportKind, Item};
use draft_common::bail;

/// Late declaration analysis pass for the module.
///
/// This pass completes the semantic analysis of declarations (structs, enums,
/// functions, constants) after their names and initial shells
/// have been registered during the early phase.
///
/// In this stage:
/// - Generic parameters are reinstated into the inference context.
/// - All type annotations are resolved into `Typ`.
/// - Function bodies are type-checked.
/// - Struct and enum fields are fully typed.
/// - Constants are inferred and unified against their annotations.
/// - All definitions are finalized and registered into the resolver.
///
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Performs late analysis of a constant definition.
    ///
    /// A constant has:
    /// - A required type annotation.
    /// - A value expression which is inferred independently.
    ///
    /// ## Procedure:
    /// 1. Resolve the type annotation.
    /// 2. Infer the type of the value expression.
    /// 3. Emit a unification constraint requiring the expression type to match
    ///    the annotated type.
    /// 4. Register the constant in the module namespace.
    ///
    /// ## Constants do not:
    /// - Introduce generics.
    /// - Create scopes.
    /// - Participate in inference outside their own value.
    ///
    fn late_define_const(&mut self, item: untyped::Const) {
        // Const inference
        let annotated = self.infer_type_hint(item.hint);
        let inferred = self.infer_expr(item.value);
        coercion::coerce(
            &mut self.icx,
            Cause::Assignment(&item.span),
            Coercion::Eq(annotated.clone(), inferred),
        );

        // Defining constant
        self.resolver.define_top_level(
            &item.span,
            &item.name,
            ModuleDef::Const(WithPublicity {
                publicity: item.publicity,
                value: annotated,
            }),
        );
    }

    /// Each declaration variant is fully processed here:
    /// - Const → `late_defint_const`
    /// - Struct → `late_analyze_struct`
    /// - Enum → `late_analyze_enum`
    /// - Fn → `late_analyze_fn`
    ///
    /// After this call, each declaration is fully type-analyzed and integrated
    /// into the module’s type environment.
    ///
    pub fn late_analyze_item(&mut self, item: Item) {
        match item {
            Item::Struct(s) => self.late_analyze_struct(s),
            Item::Enum(e) => self.late_analyze_enum(e),
            Item::Fn(f) => self.late_analyze_fn(f),
            Item::Const(c) => self.late_define_const(c),
            _ => {}
        }
    }

    /// Performs an import of another module into the current resolver scope.
    ///
    /// ## Supports:
    /// - `use foo as name`  → import with renamed binding.
    /// - `use foo for a,b`  → import selected names.
    ///
    /// On success, the referenced module is integrated into the current scope
    /// according to the chosen `UseKind`.
    ///
    /// ## Errors
    /// - [`TypeckError::ImportOfUnknownModule`]: if module doesn't exist.
    ///
    pub fn perform_import(&mut self, import: Import) {
        match self.package.root.query_module(&import.path.module) {
            Some(module) => match import.kind {
                ImportKind::As(name) => {
                    self.resolver
                        .import_as(self.package.root, &import.span, name, module)
                }
                ImportKind::Just => self.resolver.import_as(
                    self.package.root,
                    &import.span,
                    import.path.module.split('/').last().unwrap().to_string(),
                    module,
                ),
                ImportKind::For(names) => self.resolver.import_for(
                    self.package.root,
                    &mut self.icx,
                    &import.span,
                    names,
                    module,
                ),
            },
            None => bail!(TypeckError::ImportOfUnknownModule {
                src: import.span.0,
                span: import.span.1.into(),
                m: import.path.module
            }),
        };
    }
}
