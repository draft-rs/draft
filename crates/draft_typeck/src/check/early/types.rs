use draft_ast::untyped;

/// Imports
use crate::cx::module::ModuleCx;
use crate::typ::def::{ModuleDef, TypeDef};
use crate::typ::typ::{Enum, Struct, WithPublicity};

/// Implementation of the type declarations early analyse.
///
/// Enums and structs are defined *just by name* without any
/// semantic analyse pass. Enums and structs semantic pass
/// will be performed in the last phase.
///
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Registers a struct name in the module before its fields are analyzed.
    ///
    /// This creates a placeholder [`Struct`] with:
    /// - empty fields,
    /// - generic params,
    /// - fresh `uid`,
    ///   but without performing any semantic checks.
    ///
    /// The full struct body will later be populated in
    /// [`late_analyze_struct`].
    ///
    pub(crate) fn early_define_struct(&mut self, strct: &untyped::Struct) {
        // Pushing generics
        self.icx.generics.push_scope(strct.generics.clone());

        // Generating struct
        let struct_ = Struct {
            span: strct.span.clone(),
            name: strct.name.clone(),
            generics: strct.generics.clone(),
            fields: Vec::new(),
        };

        let id = self.icx.tcx.insert_struct(struct_);

        // Popping generics
        self.icx.generics.pop_scope();

        // Defining struct
        self.resolver.define_top_level(
            &strct.span,
            &strct.name,
            ModuleDef::Type(WithPublicity {
                publicity: strct.publicity.clone(),
                value: TypeDef::Struct(id),
            }),
        );
    }

    /// Registers an enum name in the module before its variants are analyzed.
    ///
    /// The enum is inserted as a placeholder containing:
    /// - generics params,
    /// - no variants,
    /// - fresh `uid`.
    ///
    /// Variants and their fields are added later during
    /// [`late_analyze_enum`].
    ///
    pub(crate) fn early_define_enum(&mut self, en: &untyped::Enum) {
        // Pushing generics
        self.icx.generics.push_scope(en.generics.clone());

        // Generating enum
        let enum_ = Enum {
            span: en.span.clone(),
            name: en.name.clone(),
            generics: en.generics.clone(),
            variants: Vec::new(),
        };
        let id = self.icx.tcx.insert_enum(enum_);

        // Popping generics
        self.icx.generics.pop_scope();

        // Defining enum
        self.resolver.define_top_level(
            &en.span,
            &en.name,
            ModuleDef::Type(WithPublicity {
                publicity: en.publicity.clone(),
                value: TypeDef::Enum(id),
            }),
        );
    }
}
