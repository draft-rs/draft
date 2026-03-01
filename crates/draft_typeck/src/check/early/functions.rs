/// Imports
use crate::cx::module::ModuleCx;
use crate::typ::def::ModuleDef;
use crate::typ::typ::{Function, Typ, WithPublicity};
use draft_ast::untyped;

/// Performs the “early” pass of module analysis.
///
/// The early phase registers symbols (types, enums, functions, externals)
/// in the module scope *by signature and generics only*, without inspecting their internals.
///
/// This ensures that forward references are allowed:
/// all types and functions with inferred signatures
/// become visible before later semantic analysis begins.
///
/// No fields, parameters or bodies are analyzed here.
/// Only namespace entry creation happens.
///
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Registers a function
    /// symbol in the module before its body is analyzed.
    pub(crate) fn early_define_fn(&mut self, function: &untyped::Fn) {
        // Pushing generics
        self.icx.generics.push_scope(function.generics.clone());

        // Generating function
        let def = Function {
            span: function.span.clone(),
            name: function.name.clone(),
            generics: function.generics.clone(),
            params: function
                .params
                .iter()
                .cloned()
                .map(|p| self.infer_type_hint(p.hint))
                .collect(),
            ret: function
                .ret
                .clone()
                .map_or(Typ::Unit, |it| self.infer_type_hint(it)),
        };
        let id = self.icx.tcx.insert_function(def);

        // Popping generics
        self.icx.generics.pop_scope();

        // Defining function
        self.resolver.define_top_level(
            &function.span,
            &function.name,
            ModuleDef::Function(WithPublicity {
                publicity: function.publicity.clone(),
                value: id,
            }),
        );
    }

    /// Registers an extern function
    /// symbol in the module before its body is analyzed.
    pub(crate) fn early_define_extern_fn(&mut self, function: &untyped::ExternFn) {
        // Pushing generics
        self.icx.generics.push_scope(function.generics.clone());

        // Generating function
        let def = Function {
            span: function.span.clone(),
            name: function.name.clone(),
            generics: function.generics.clone(),
            params: function
                .params
                .iter()
                .cloned()
                .map(|p| self.infer_type_hint(p.hint))
                .collect(),
            ret: function
                .ret
                .clone()
                .map_or(Typ::Unit, |it| self.infer_type_hint(it)),
        };
        let id = self.icx.tcx.insert_function(def);

        // Popping generics
        self.icx.generics.pop_scope();

        // Defining function
        self.resolver.define_top_level(
            &function.span,
            &function.name,
            ModuleDef::Function(WithPublicity {
                publicity: function.publicity.clone(),
                value: id,
            }),
        );
    }
}
