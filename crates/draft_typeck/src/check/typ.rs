/// Imports
use crate::{
    cx::module::ModuleCx,
    errors::{TypeckError, TypeckRelated},
    pretty::Pretty,
    typ::{
        def::{ModuleDef, TypeDef},
        typ::{Enum, PreludeType, Struct, Typ},
    },
};
use draft_ast::{common::Publicity, untyped::TypeHint};
use draft_common::{bail, span::Span};
use id_arena::Id;

/// Implementation
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Ensures no generic parameters given
    fn ensure_no_generics<F>(&self, span: &Span, got: usize, typ: F) -> Typ
    where
        F: FnOnce() -> Typ,
    {
        if got == 0 {
            typ()
        } else {
            bail!(TypeckError::ArityMissmatch {
                related: vec![TypeckRelated::Here {
                    src: span.0.clone(),
                    span: span.1.clone().into()
                }],
                expected: 0,
                got
            })
        }
    }

    /// Checks generic parameters arity
    fn check_generic_params_arity(&self, span: &Span, expected: usize, got: usize) {
        if expected != got {
            bail!(TypeckError::ArityMissmatch {
                related: vec![TypeckRelated::Here {
                    src: span.0.clone(),
                    span: span.1.clone().into()
                }],
                expected,
                got
            })
        }
    }

    /// Infers a local hint (built-in or user-defined).
    fn infer_local_type_hint(&mut self, span: Span, name: String, generics: Vec<TypeHint>) -> Typ {
        match name.as_str() {
            // Prelude types
            "int" => {
                self.ensure_no_generics(&span, generics.len(), || Typ::Prelude(PreludeType::Int))
            }
            "float" => {
                self.ensure_no_generics(&span, generics.len(), || Typ::Prelude(PreludeType::Float))
            }
            "bool" => {
                self.ensure_no_generics(&span, generics.len(), || Typ::Prelude(PreludeType::Bool))
            }
            "string" => {
                self.ensure_no_generics(&span, generics.len(), || Typ::Prelude(PreludeType::String))
            }
            "unit" => self.ensure_no_generics(&span, generics.len(), || Typ::Unit),

            // User-defined types
            _ => match self.icx.generics.index_of(&name) {
                Some(idx) => self.ensure_no_generics(&span, generics.len(), || Typ::Generic(idx)),
                None => match self.resolver.resolve_type(&span, &name) {
                    TypeDef::Enum(en) => self.instantiate_enum_type(&span, en, generics),
                    TypeDef::Struct(st) => self.instantiate_struct_type(&span, st, generics),
                },
            },
        }
    }

    /// Infers a type imported from another module.
    ///
    /// Performs visibility checks and handles both enums and structs.
    fn infer_module_type_hint(
        &mut self,
        span: Span,
        module: String,
        name: String,
        generics: Vec<TypeHint>,
    ) -> Typ {
        let m = self.resolver.resolve_module(self.package.root, &module);

        let def = match m.fields.get(&name) {
            Some(ModuleDef::Type(def)) if def.publicity != Publicity::Priv => def.clone(),
            Some(ModuleDef::Type(def)) => bail!(TypeckError::TypeIsPrivate {
                src: span.0.clone(),
                span: span.1.clone().into(),
                def: def.value.pretty(&mut self.icx)
            }),
            Some(ModuleDef::Const(_)) | Some(ModuleDef::Function(_)) => {
                bail!(TypeckError::CouldNotUseValueAsType {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    v: name
                })
            }
            None => bail!(TypeckError::TypeIsNotDefined {
                src: span.0.clone(),
                span: span.1.clone().into(),
                t: format!("{module}.{name}").into()
            }),
        };

        match &def.value {
            TypeDef::Enum(en) => self.instantiate_enum_type(&span, *en, generics),
            TypeDef::Struct(st) => self.instantiate_struct_type(&span, *st, generics),
        }
    }

    /// Infers a function type annotation like `fn(int, string): bool`.
    fn infer_function_type_hint(
        &mut self,
        params: Vec<TypeHint>,
        ret: Option<Box<TypeHint>>,
    ) -> Typ {
        let params = params
            .into_iter()
            .map(|p| self.infer_type_hint(p))
            .collect();
        let ret = Box::new(ret.map_or(Typ::Unit, |t| self.infer_type_hint(*t)));

        Typ::FnRef(params, ret)
    }

    /// Instantiates an enum type with its generic parameters.
    fn instantiate_enum_type(&mut self, span: &Span, id: Id<Enum>, generics: Vec<TypeHint>) -> Typ {
        let generic_params = self.icx.tcx.enum_(id).generics.clone();
        self.check_generic_params_arity(span, generic_params.len(), generics.len());

        let substitutions = generic_params
            .iter()
            .zip(generics)
            .map(|(_, arg)| {
                let ty = self.infer_type_hint(arg);
                ty
            })
            .collect();

        Typ::Enum(id, substitutions)
    }

    /// Instantiates a struct type with its generic parameters.
    fn instantiate_struct_type(
        &mut self,
        span: &Span,
        id: Id<Struct>,
        generics: Vec<TypeHint>,
    ) -> Typ {
        let generic_params = self.icx.tcx.struct_(id).generics.clone();
        self.check_generic_params_arity(span, generic_params.len(), generics.len());

        let substitutions = generic_params
            .iter()
            .zip(generics)
            .map(|(_, arg)| {
                let ty = self.infer_type_hint(arg);
                ty
            })
            .collect();

        Typ::Struct(id, substitutions)
    }

    /// Infers a type hint.
    ///
    /// ## This function handles:
    /// - Prelude (built-in) types: `int`, `float`, `bool`, `string`, `()`
    /// - User-defined types (enums and structs)
    /// - Module-qualified types (e.g. `math.Vector`)
    /// - Function type expressions (e.g. `(int, float) -> bool`)
    /// - Unit type
    ///
    /// Each branch validates generic parameters count and ensures
    /// access visibility for types imported from other modules.
    ///
    pub(crate) fn infer_type_hint(&mut self, path: TypeHint) -> Typ {
        match path.clone() {
            TypeHint::Local {
                span,
                name,
                generics,
            } => self.infer_local_type_hint(span, name, generics),
            TypeHint::Module {
                span,
                module,
                name,
                generics,
            } => self.infer_module_type_hint(span, module, name, generics),
            TypeHint::Function { params, ret, .. } => self.infer_function_type_hint(params, ret),
            TypeHint::Unit { .. } => Typ::Unit,
        }
    }
}
