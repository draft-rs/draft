use draft_ast::untyped::{self, TypeHint};

/// Imports
use crate::{
    cx::module::ModuleCx,
    typ::{
        def::TypeDef,
        typ::{EnumVariant, Field},
    },
};

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
    /// Performs late analysis of a struct declaration.
    ///
    /// ## Responsibilities:
    /// - Infer the types of all fields using `infer_type_annotation`.
    /// - Rebuild the `Struct` def with resolved field types.
    /// - Overwrite the existing struct definition with the completed one.
    ///
    /// This operation mutates the struct in place, finalizing its type
    /// structure for the rest of type checking.
    ///
    pub(crate) fn late_analyze_struct(&mut self, item: untyped::Struct) {
        // Requesting struct
        let id = match self.resolver.resolve_type(&item.span, &item.name) {
            TypeDef::Struct(ty) => ty,
            _ => unreachable!(),
        };

        // Inferring fields
        let fields = {
            // Re pushing generics
            self.icx.generics.push_scope(item.generics);

            let fields = item
                .fields
                .into_iter()
                .map(|f| Field {
                    name: f.name,
                    span: f.span,
                    typ: self.infer_type_hint(f.hint),
                })
                .collect::<Vec<Field>>();

            // Popping generics
            self.icx.generics.pop_scope();

            fields
        };

        // Setting fields
        let struct_mut = self.icx.tcx.struct_mut(id);
        struct_mut.fields = fields;
    }

    /// Performs late analysis of an enum declaration.
    ///
    /// ## Responsibilities:
    /// - Infer the types of all variant fields.
    /// - Rebuild the `Enum` def with resolved variant field types.
    /// - Overwrite the existing enum definition with the completed one.
    ///
    /// Enum variant fields are treated similarly to struct fields: each
    /// parameter is analyzed using `infer_type_annotation`.
    ///
    pub(crate) fn late_analyze_enum(&mut self, item: untyped::Enum) {
        // Requesting enum
        let id = match self.resolver.resolve_type(&item.span, &item.name) {
            TypeDef::Enum(en) => en,
            _ => unreachable!(),
        };

        // Inferring variants
        let variants = {
            // Repushing generics
            self.icx.generics.push_scope(item.generics);

            let variants = item
                .variants
                .into_iter()
                .map(|v| EnumVariant {
                    span: v.span,
                    name: v.name,
                    fields: v
                        .fields
                        .into_iter()
                        .map(|p: TypeHint| self.infer_type_hint(p))
                        .collect(),
                })
                .collect::<Vec<EnumVariant>>();

            // Popping generics
            self.icx.generics.pop_scope();

            variants
        };

        // Setting variants
        let enum_mut = self.icx.tcx.enum_mut(id);
        enum_mut.variants = variants;
    }
}
