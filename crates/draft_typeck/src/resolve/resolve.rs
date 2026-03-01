/// Imports
use crate::{
    cx::root::RootCx,
    errors::TypeckError,
    pretty::Pretty,
    resolve::rib::{Rib, RibsStack},
    typ::{
        cx::InferCx,
        def::{ModuleDef, TypeDef},
        res::Res,
        typ::{Module, Typ},
    },
};
use draft_common::{bail, span::Span};
use id_arena::Id;
use std::collections::HashMap;
use tracing::instrument;

/// Resolves names and types within a module.
///
/// `ModuleResolver` is responsible for managing module-level scope, tracking
/// definitions, and handling imports. It is used during type checking and
/// name resolution to determine what each identifier refers to.
///
/// # Fields
///
/// - `ribs_stack: RibsStack`
///   A stack of "ribs", representing nested scopes inside the module.
///   Each rib holds bindings for a single lexical scope. The stack structure
///   allows proper shadowing and scope resolution.
///
/// - `mod_defs: HashMap<String, ModuleDef>`
///   The definitions present directly in the module, keyed by their names.
///   This includes user-defined types (`Type`) and constants (`Const`).
///
/// - `imported_modules: HashMap<String, Id<Module>>`
///   Modules that have been imported into the current module.
///   Allows resolution of identifiers that are qualified with module paths.
///
/// - `imported_defs: HashMap<String, ModuleDef>`
///   Definitions imported from other modules, keyed by their local names.
///   Enables access to external types and constants without fully qualifying them.
///
#[derive(Default, Debug)]
pub struct ModuleResolver {
    /// Ribs stack of module
    ribs_stack: RibsStack,
    /// Module definitions
    module_defs: HashMap<String, ModuleDef>,
    /// Imported modules
    pub imported_modules: HashMap<String, Id<Module>>,
    /// Imported definitions
    pub imported_defs: HashMap<String, ModuleDef>,
}

/// Implementation
impl ModuleResolver {
    /// Defines a module-level item (type or constant) if it is not already defined.
    ///
    /// This method inserts a new definition into the module's namespace. It performs
    /// checks to ensure that no conflicting definition exists unless `redefine` is true.
    ///
    /// # Parameters
    ///
    /// - `span: &Span`
    ///   The source span of the definition, used for error reporting.
    ///
    /// - `name: &String`
    ///   The identifier name for the definition.
    ///
    /// - `def: ModuleDef`
    ///   The definition to insert (type or constant).
    ///
    /// # Important
    ///
    /// - This method ensures that the module maintains a consistent namespace.
    ///
    pub fn define_top_level(&mut self, span: &Span, name: &String, def: ModuleDef) {
        match self.module_defs.get(name) {
            Some(found) => match found {
                ModuleDef::Type(_) => {
                    bail!(TypeckError::TypeIsAlreadyDefined {
                        src: span.0.clone(),
                        span: span.1.clone().into(),
                        t: name.clone()
                    })
                }
                ModuleDef::Const(_) | ModuleDef::Function(_) => {
                    bail!(TypeckError::VariableIsAlreadyDefined {
                        src: span.0.clone(),
                        span: span.1.clone().into(),
                        name: name.clone()
                    })
                }
            },
            None => {
                self.module_defs.insert(name.clone(), def);
            }
        }
    }

    /// Defines a local-level item (local variable) if it is not already defined.
    ///
    /// This method inserts a new definition into the last rib's scope. It performs
    /// checks to ensure that no conflicting definition exists unless `redefine` is true.
    ///
    /// # Parameters
    ///
    /// - `span: &Span`
    ///   The source span of the definition, used for error reporting.
    ///
    /// - `name: &String`
    ///   The identifier name for the definition.
    ///
    /// - `def: ModuleDef`
    ///   The definition to insert (type or constant).
    ///   during the **early analysis pass**.
    ///
    /// # Important
    ///
    /// - This method ensures that the rib maintains a consistent scope.
    ///
    pub fn define_local(&mut self, span: &Span, name: &String, typ: Typ) {
        self.ribs_stack.define(span, name, typ);
    }

    /// Resolves an identifier to its corresponding value, type, or module.
    ///
    /// This method looks up the given `name` in the current module's namespace
    /// and imported modules/definitions. It follows a structured lookup order
    /// to ensure correct scoping and resolution of identifiers. If the identifier
    /// cannot be found, it raises a `TypeckError::CouldNotResolve`.
    ///
    /// # Parameters
    ///
    /// - `span: &Span`
    ///   The source code span of the identifier being resolved, used for
    ///   error reporting.
    ///
    /// - `name: &String`
    ///   The identifier to resolve.
    ///
    /// # Resolution Flow
    ///
    /// 1. **Ribs stack lookup (local and nested scopes)**
    ///    The method first searches the `ribs_stack`, which represents nested
    ///    lexical scopes. If a binding is found here, it is returned as
    ///    `Res::Value(typ)`. This ensures that local variables shadow module-level
    ///    definitions.
    ///
    /// 2. **Module definitions lookup**
    ///    If the identifier is not found in the local ribs, the resolver checks
    ///    `module_defs`, which contains definitions directly declared in the module:
    ///    - `ModuleDef::Type` -> returned as `Res::Custom(TypeDef)`
    ///    - `ModuleDef::Const` -> returned as `Res::Value(Typ)`
    ///
    /// 3. **Imported definitions lookup**
    ///    If the identifier is not present in local module definitions, the resolver
    ///    checks `imported_defs`, which contains definitions imported from other
    ///    modules. The resolution behaves similarly to module definitions:
    ///    - `ModuleDef::Type` -> `Res::Custom(TypeDef)`
    ///    - `ModuleDef::Const` -> `Res::Value(Typ)`
    ///
    /// 4. **Imported modules lookup**
    ///    If the identifier is not found in definitions, the resolver checks
    ///    `imported_modules`. If found, the identifier resolves to a module:
    ///    - returned as `Res::Module(name.clone())`
    ///
    /// 5. **Error if not found**
    ///    If the identifier cannot be found in any of the above cases, the
    ///    resolver raises a `TypeckError::CouldNotResolve` with the given source
    ///    span and the unresolved name.
    ///
    /// # Returns
    ///
    /// A `Res` enum indicating the resolved entity:
    /// - `Res::Module(String)` -> a module
    /// - `Res::Custom(TypeDef)` -> a user-defined type
    /// - `Res::Value(Typ)` -> a value
    /// - `Res::Const(Typ)` -> a constant value
    ///
    /// `Res::Variant(Rc<Enum>, EnumVariant)` will be never returned
    ///
    pub fn resolve(&self, icx: &mut InferCx, span: &Span, name: &String) -> Res {
        // Checking existence in ribs
        match self.ribs_stack.lookup(name) {
            Some(typ) => Res::Value(typ),
            None => match self.module_defs.get(name) {
                // Checking existence in module definitions
                Some(typ) => match typ {
                    ModuleDef::Type(ty) => Res::Custom(ty.value.clone()),
                    ModuleDef::Const(ty) => Res::Const(ty.value.clone()),
                    ModuleDef::Function(ty) => {
                        let args =
                            icx.mk_fresh_generics(&icx.tcx.function(ty.value).generics.clone());
                        Res::Value(Typ::FnDef(ty.value, args))
                    }
                },
                None => match self.imported_defs.get(name) {
                    // Checking existence in imported defs
                    Some(typ) => match typ {
                        ModuleDef::Type(ty) => Res::Custom(ty.value.clone()),
                        ModuleDef::Const(ty) => Res::Const(ty.value.clone()),
                        ModuleDef::Function(ty) => {
                            let args =
                                icx.mk_fresh_generics(&icx.tcx.function(ty.value).generics.clone());
                            Res::Value(Typ::FnDef(ty.value, args))
                        }
                    },
                    None => match self.imported_modules.get(name) {
                        // Checking existence in modules
                        Some(_) => Res::Module(name.clone()),
                        None => bail!(TypeckError::CouldNotResolve {
                            src: span.0.clone(),
                            span: span.1.clone().into(),
                            name: name.clone()
                        }),
                    },
                },
            },
        }
    }

    /// Resolves an identifier to its corresponding type.
    ///
    /// This method looks up the given `name` in the current module's namespace
    /// and imported definitions. It follows a structured lookup order
    /// to ensure correct scoping and resolution of identifiers.
    ///
    /// # Parameters
    ///
    /// - `span: &Span`
    ///   The source code span of the identifier being resolved, used for
    ///   error reporting.
    ///
    /// - `name: &String`
    ///   The type name to resolve.
    ///
    /// # Resolution Flow
    ///
    /// 1. **Module definitions**
    ///    The method first checks `module_defs`
    ///    for the type existence (`TypeDef`).
    ///
    /// 2. **Imported definitions lookup**
    ///    If the identifier is not present in module definitions, the resolver
    ///    checks `imported_defs` for the type existence (`TypeDef`), which contains
    ///    definitions imported from other modules.
    ///
    /// # Errors
    ///
    /// - Raises `TypeckError::TypeIsNotDefined` if the type cannot be resolved.
    /// - Raises `TypeckError::CouldNotUseValueAsType` if the const shadows the type name.
    ///
    pub fn resolve_type(&self, span: &Span, name: &String) -> TypeDef {
        // Checking existence in module definitions
        match self.module_defs.get(name) {
            Some(typ) => match typ {
                ModuleDef::Type(ty) => ty.value.clone(),
                ModuleDef::Const(_) | ModuleDef::Function(_) => {
                    bail!(TypeckError::CouldNotUseValueAsType {
                        src: span.0.clone(),
                        span: span.1.clone().into(),
                        v: name.clone()
                    })
                }
            },
            None => match self.imported_defs.get(name) {
                // Checking existence in imported defs
                Some(typ) => match typ {
                    ModuleDef::Type(ty) => ty.value.clone(),
                    ModuleDef::Const(_) | ModuleDef::Function(_) => {
                        bail!(TypeckError::CouldNotUseValueAsType {
                            src: span.0.clone(),
                            span: span.1.clone().into(),
                            v: name.clone()
                        })
                    }
                },
                None => bail!(TypeckError::TypeIsNotDefined {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    t: name.clone()
                }),
            },
        }
    }

    /// Resolves a module by its name in the imported modules.
    ///
    /// # Parameters
    /// - `name: &String` — The name of the module to resolve.
    ///
    /// # Returns
    /// - A reference to the `Module` if it exists in `imported_modules`.
    ///
    /// # Errors
    /// - Raises `TypeckError::ModuleIsNotDefined` if the module is not imported.
    ///
    pub fn resolve_module<'a>(&self, cx: &'a RootCx, name: &String) -> &'a Module {
        match self.imported_modules.get(name) {
            Some(id) => cx.module(*id),
            None => bail!(TypeckError::ModuleIsNotDefined { m: name.clone() }),
        }
    }

    /// Pushes a new rib onto the ribs stack.
    ///
    /// Ribs represent lexical scopes (e.g., for function bodies or blocks).
    /// Pushing a rib creates a new nested scope for variable bindings.
    ///
    pub fn push_rib(&mut self) {
        self.ribs_stack.push();
    }

    /// Pops the top rib from the ribs stack.
    ///
    /// Returns the popped `Rib` if it exists. Popping a rib exits
    /// the current scope and removes all bindings defined in that scope.
    ///
    pub fn pop_rib(&mut self) -> Option<Rib> {
        self.ribs_stack.pop()
    }

    /// Collects and drains all module definitions.
    ///
    /// This method removes all current definitions from `module_defs` and
    /// returns them as a `HashMap`. Useful for exporting definitions after
    /// analysis or for module construction.
    ///
    /// # Returns
    /// A `HashMap<String, ModuleDef>` containing all collected definitions.
    ///
    pub fn collect(&mut self) -> HashMap<String, ModuleDef> {
        self.module_defs.drain().collect()
    }

    /// Imports a module under a given alias.
    ///
    /// # Parameters
    /// - `span: &Span` — The source span for error reporting.
    /// - `name: String` — The alias under which the module should be imported.
    /// - `module: Id<Module>` — The module to import.
    ///
    /// # Errors
    /// - Raises `TypeckError::ModuleIsAlreadyImportedAs` if the alias is already used.
    ///
    #[instrument(skip(span, cx), level = "trace")]
    pub fn import_as(&mut self, cx: &RootCx, span: &Span, name: String, module: Id<Module>) {
        match self.imported_modules.get(&name) {
            Some(module) => bail!(TypeckError::ModuleIsAlreadyImportedAs {
                src: span.0.clone(),
                span: span.1.clone().into(),
                m: cx.module(*module).name.clone(),
                name: name.clone()
            }),
            None => self.imported_modules.insert(name, module),
        };
    }

    /// Imports specific names (definitions) from a module.
    ///
    /// # Parameters
    /// - `icx: &mut InferCx` — Inference context used for pretty printing definitions.
    /// - `span: &Span` — Source span for error reporting.
    /// - `names: Vec<String>` — Names of definitions to import from the module.
    /// - `module: Id<Module>` — The module to import from.
    ///
    /// # Behavior
    /// For each name:
    /// 1. Checks if the name exists in the module's fields.
    /// 2. Checks if the name was already imported from another module.
    /// 3. Inserts the definition into `imported_defs` if both checks pass.
    ///
    /// # Errors
    /// - `TypeckError::ModuleFieldIsNotDefined` if the name does not exist in the module.
    /// - `TypeckError::DefIsAlreadyImported` if the name has already been imported.
    ///
    #[instrument(skip(icx, rcx, span), level = "trace")]
    pub fn import_for(
        &mut self,
        rcx: &RootCx,
        icx: &mut InferCx,
        span: &Span,
        names: Vec<String>,
        module: Id<Module>,
    ) {
        let module = rcx.module(module);
        for name in names {
            match module.fields.get(&name) {
                Some(def) => match self.imported_defs.get(&name) {
                    Some(already) => bail!(TypeckError::DefIsAlreadyImported {
                        src: span.0.clone(),
                        span: span.1.clone().into(),
                        name: name.clone(),
                        def: already.pretty(icx),
                    }),
                    None => {
                        self.imported_defs.insert(name, def.clone());
                    }
                },
                None => {
                    bail!(TypeckError::ModuleFieldIsNotDefined {
                        src: span.0.clone(),
                        span: span.1.clone().into(),
                        m: module.name.clone(),
                        field: name
                    })
                }
            }
        }
    }
}
