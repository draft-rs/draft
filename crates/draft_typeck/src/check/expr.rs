/// Imports
use crate::{
    cx::module::ModuleCx,
    errors::{TypeckError, TypeckRelated},
    ex::ExMatchCx,
    inference::{
        cause::Cause,
        coercion::{self, Coercion},
    },
    pretty::Pretty,
    typ::{
        def::{ModuleDef, TypeDef},
        res::Res,
        typ::{PreludeType, Typ},
    },
};
use draft_ast::{
    common::{BinOp, Lit, Publicity, UnOp},
    untyped::{Case, Expr, Param, Pat, TypeHint, UnwrapField},
};
use draft_common::{bail, span::Span};

/// Exprs inferring
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Infers the type of concat Expr.
    ///
    /// This function:
    /// - Checks that both the left and right operands are strings.
    /// - Produces the resulting type, or emits a `TypeckError::InvalidBinaryOp`.
    ///
    ///
    /// # Parameters
    /// - `span`: Source code span of the binary operator.
    /// - `left`: Left-hand side type.
    /// - `right`: Right-hand side type.
    ///
    /// # Returns
    /// -`Typ::String`
    ///
    fn infer_binary_concat(&mut self, span: Span, left: Typ, right: Typ) -> Typ {
        // Checking prelude types
        match left {
            Typ::Prelude(PreludeType::String) => match right {
                Typ::Prelude(PreludeType::String) => Typ::Prelude(PreludeType::String),
                _ => bail!(TypeckError::InvalidBinaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    a: left.pretty(&mut self.icx),
                    b: right.pretty(&mut self.icx),
                    op: BinOp::Concat
                }),
            },
            _ => bail!(TypeckError::InvalidBinaryOp {
                src: span.0.clone(),
                span: span.1.clone().into(),
                a: left.pretty(&mut self.icx),
                b: right.pretty(&mut self.icx),
                op: BinOp::Concat
            }),
        }
    }

    /// Infers the type of arithmetical Expr.
    ///
    /// This function:
    /// - Checks that both the left and right operands are numeric.
    /// - Produces the resulting type, or emits a `TypeckError::InvalidBinaryOp`.
    ///
    /// # Parameters
    /// - `span`: Source code span of the binary operator.
    /// - `left`: Left-hand side type.
    /// - `op`: Binary operator used for the diagnostics.
    /// - `right`: Right-hand side type.
    ///
    /// # Returns
    /// - The resulting `Typ` after applying the operator.
    ///
    /// # Notes
    /// Numeric operators automatically promote `Int × Float` or `Float × Int` to `Float`.
    ///
    fn infer_binary_arithmetical(&mut self, span: Span, left: Typ, op: BinOp, right: Typ) -> Typ {
        // Checking prelude types
        match left {
            Typ::Prelude(PreludeType::Int) => match right {
                Typ::Prelude(PreludeType::Int) => Typ::Prelude(PreludeType::Int),
                Typ::Prelude(PreludeType::Float) => Typ::Prelude(PreludeType::Float),
                _ => bail!(TypeckError::InvalidBinaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    a: left.pretty(&mut self.icx),
                    b: right.pretty(&mut self.icx),
                    op
                }),
            },
            Typ::Prelude(PreludeType::Float) => match right {
                Typ::Prelude(PreludeType::Int) => Typ::Prelude(PreludeType::Float),
                Typ::Prelude(PreludeType::Float) => Typ::Prelude(PreludeType::Float),
                _ => bail!(TypeckError::InvalidBinaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    a: left.pretty(&mut self.icx),
                    b: right.pretty(&mut self.icx),
                    op
                }),
            },
            _ => bail!(TypeckError::InvalidBinaryOp {
                src: span.0.clone(),
                span: span.1.clone().into(),
                a: left.pretty(&mut self.icx),
                b: right.pretty(&mut self.icx),
                op
            }),
        }
    }

    /// Infers the type of logical Expr.
    ///
    /// This function:
    /// - Checks that both the left and right operands are `Typ::Bool`.
    /// - Produces the resulting type, or emits a `TypeckError::InvalidBinaryOp`.
    ///
    /// # Parameters
    /// - `span`: Source code span of the binary operator.
    /// - `left`: Left-hand side type.
    /// - `op`: Binary operator used for the diagnostics.
    /// - `right`: Right-hand side type.
    ///
    /// # Returns
    /// - `Typ::Bool`
    ///
    fn infer_binary_logical(&mut self, span: Span, left: Typ, op: BinOp, right: Typ) -> Typ {
        // Checking prelude types
        match left {
            Typ::Prelude(PreludeType::Bool) => match right {
                Typ::Prelude(PreludeType::Bool) => Typ::Prelude(PreludeType::Bool),
                _ => bail!(TypeckError::InvalidBinaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    a: left.pretty(&mut self.icx),
                    b: right.pretty(&mut self.icx),
                    op
                }),
            },
            _ => bail!(TypeckError::InvalidBinaryOp {
                src: span.0.clone(),
                span: span.1.clone().into(),
                a: left.pretty(&mut self.icx),
                b: right.pretty(&mut self.icx),
                op
            }),
        }
    }

    /// Infers the type of compare Expr.
    ///
    /// This function:
    /// - Checks that both the left and right operands are numerics.
    /// - Produces the resulting type, or emits a `TypeckError::InvalidBinaryOp`.
    ///
    /// # Parameters
    /// - `span`: Source code span of the binary operator.
    /// - `left`: Left-hand side type.
    /// - `op`: Binary operator used for the diagnostics.
    /// - `right`: Right-hand side type.
    ///
    /// # Returns
    /// - `Typ::Bool`
    ///
    fn infer_binary_compare(&mut self, span: Span, left: Typ, op: BinOp, right: Typ) -> Typ {
        // Checking prelude types
        match left {
            Typ::Prelude(PreludeType::Int) | Typ::Prelude(PreludeType::Float) => match right {
                Typ::Prelude(PreludeType::Int) | Typ::Prelude(PreludeType::Float) => {
                    Typ::Prelude(PreludeType::Bool)
                }
                _ => bail!(TypeckError::InvalidBinaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    a: left.pretty(&mut self.icx),
                    b: right.pretty(&mut self.icx),
                    op
                }),
            },
            _ => bail!(TypeckError::InvalidBinaryOp {
                src: span.0.clone(),
                span: span.1.clone().into(),
                a: left.pretty(&mut self.icx),
                b: right.pretty(&mut self.icx),
                op
            }),
        }
    }

    /// Infers the type of binary Expr.
    ///
    /// This function:
    /// - Infers types of both the left and right operands.
    /// - Checks whether the operator is applicable to the operand types.
    /// - Performs type-level computation (e.g., boolean logic).
    /// - Produces the resulting type, or emits a `TypeckError::InvalidBinaryOp`
    ///   if operands are incompatible with the operator.
    ///
    /// # Parameters
    /// - `span`: Source code span of the binary operator.
    /// - `op`: Binary operator being applied.
    /// - `left`: Left-hand side Expr.
    /// - `right`: Right-hand side Expr.
    ///
    /// # Returns
    /// - The resulting `Typ` after applying the operator.
    ///
    /// # Errors
    /// - [`InvalidBinaryOp`]: when operand types do not match operator requirements.
    ///
    /// # Notes
    /// This function handles:
    /// - String concatenation (`<>`)
    /// - Arithmetic operators (`+`, `-`, `*`, `/`, `%`, `&`, `|`)
    /// - Logical operators (`&&`, `||`, `^`)
    /// - Comparison operators (`<`, `<=`, `>`, `>=`)
    /// - Equality (`==`, `!=`)
    ///
    fn infer_binary(&mut self, span: Span, op: BinOp, left: Expr, right: Expr) -> Typ {
        // Inferencing left and right types
        let left = self.infer_expr(left);
        let right = self.infer_expr(right);

        // Matching operator
        match op {
            // Concat
            BinOp::Concat => self.infer_binary_concat(span, left, right),
            // Arithmetical
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::Mod => self.infer_binary_arithmetical(span, left, op, right),
            // Logical
            BinOp::Xor | BinOp::And | BinOp::Or => self.infer_binary_logical(span, left, op, right),
            // Compare
            BinOp::Ge | BinOp::Gt | BinOp::Le | BinOp::Lt => {
                self.infer_binary_compare(span, left, op, right)
            }
            // Equality
            BinOp::Eq | BinOp::Ne => Typ::Prelude(PreludeType::Bool),
        }
    }

    /// Infers the type of unary Expr.
    ///
    /// This function:
    /// - Infers the type of the operand.
    /// - Checks whether the operator is applicable to the operand types.
    /// - Returns the resulting type, or emits a `TypeckError::InvalidUnaryOp`
    ///   if the operator cannot be applied.
    ///
    /// # Parameters
    /// - `span`: Source span of the unary operator.
    /// - `op`: Unary operator (`-` or `!`).
    /// - `value`: Operand Expr.
    ///
    /// # Returns
    /// - The resulting `Typ` after applying the operator.
    ///
    /// # Errors
    /// - [`InvalidUnaryOp`]: operand type does not match operator expectation.
    ///
    /// # Notes
    /// - `-` is valid only for `Int` and `Float`.
    /// - `!` is valid only for `Bool`.
    ///
    fn infer_unary(&mut self, span: Span, op: UnOp, value: Expr) -> Typ {
        // Inferencing value
        let inferred_value = self.infer_expr(value);

        // Checking type is prelude
        let value_typ = match &inferred_value {
            Typ::Prelude(t) => t,
            _ => bail!(TypeckError::InvalidUnaryOp {
                src: span.0.clone(),
                span: span.1.clone().into(),
                t: inferred_value.pretty(&mut self.icx),
                op
            }),
        };

        // Matching operator
        match op {
            // Negate `-`
            UnOp::Neg => match value_typ {
                PreludeType::Int => Typ::Prelude(PreludeType::Int),
                PreludeType::Float => Typ::Prelude(PreludeType::Float),
                _ => bail!(TypeckError::InvalidUnaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    t: inferred_value.pretty(&mut self.icx),
                    op
                }),
            },
            // Bool negate / bang `!`
            UnOp::Bang => match value_typ {
                PreludeType::Bool => Typ::Prelude(PreludeType::Bool),
                _ => bail!(TypeckError::InvalidUnaryOp {
                    src: span.0.clone(),
                    span: span.1.clone().into(),
                    t: inferred_value.pretty(&mut self.icx),
                    op
                }),
            },
        }
    }

    /// Resolves a variable or module symbol by name.
    ///
    /// # Parameters
    /// - `span`: span of the variable reference.
    /// - `name`: Identifier being resolved.
    ///
    /// # Returns
    /// - A `Res` representing a fully resolved symbol (value, type, module, etc.).
    ///
    /// # Errors
    /// Emitted indirectly through `resolver.resolve` when a symbol is not found.
    ///
    fn infer_get(&mut self, span: Span, name: String) -> Res {
        self.resolver.resolve(&mut self.icx, &span, &name)
    }

    /// Resolves a field access on a module (e.g. `Module.field`).
    ///
    /// This function:
    /// - Locates the target module.
    /// - Locates the requested field inside the module.
    /// - Checks visibility (`Public`, `Private`).
    /// - Produces the correct `Res` variant depending on the field kind:
    ///     - `Type`  → `Res::Custom`
    ///     - `Const` → `Res::Value`
    ///     - `Function` → `Res::Value` containing a function type.
    ///
    /// # Parameters
    /// - `field_module`: Name of the module.
    /// - `field_span`: Source span of the field access.
    /// - `field_name`: Name of the field inside the module.
    ///
    /// # Returns
    /// - Resolved field as `Res`.
    ///
    /// # Errors
    /// - [`ModuleIsNotDefined`]: when the module could not be resolved.
    /// - [`ModuleFieldIsNotDefined`]: when the module field is not defined.
    /// - [`ModuleFieldIsPrivate`]: when the module field is private.
    ///
    fn infer_module_field_access(
        &mut self,
        field_module: String,
        field_span: Span,
        field_name: String,
    ) -> Res {
        // Getting module
        match self.resolver.imported_modules.get(&field_module) {
            // Getting module
            Some(module) => match self.package.root.module(*module).fields.get(&field_name) {
                // If field exists
                // checking its publicity
                Some(def) => match def {
                    ModuleDef::Type(ty) => {
                        match ty.publicity {
                            // If field is public, we resolved field
                            Publicity::Pub => Res::Custom(ty.value.clone()),
                            // Else, raising `module field is private`
                            _ => bail!(TypeckError::ModuleFieldIsPrivate {
                                src: field_span.0,
                                span: field_span.1.into(),
                                name: field_name
                            }),
                        }
                    }
                    ModuleDef::Const(var) => {
                        match var.publicity {
                            // If constant is public, we resolved field
                            Publicity::Pub => Res::Value(var.value.clone()),
                            // Else, raising `module field is private`
                            _ => bail!(TypeckError::ModuleFieldIsPrivate {
                                src: field_span.0,
                                span: field_span.1.into(),
                                name: field_name
                            }),
                        }
                    }
                    ModuleDef::Function(f) => {
                        match f.publicity {
                            // If constant is public, we resolved field
                            Publicity::Pub => Res::Value(Typ::FnDef(
                                f.value,
                                self.icx.mk_fresh_generics(
                                    &self.icx.tcx.function(f.value).generics.clone(),
                                ),
                            )),
                            // Else, raising `module field is private`
                            _ => bail!(TypeckError::ModuleFieldIsPrivate {
                                src: field_span.0,
                                span: field_span.1.into(),
                                name: field_name
                            }),
                        }
                    }
                },
                // Else, raising `module field is not defined`
                None => bail!(TypeckError::ModuleFieldIsNotDefined {
                    src: field_span.0,
                    span: field_span.1.into(),
                    m: field_module,
                    field: field_name
                }),
            },
            // If module is not defined
            None => bail!(TypeckError::ModuleIsNotDefined { m: field_module }),
        }
    }

    /// Resolves a field access on an enum type (variant lookup).
    ///
    /// This function:
    /// - Retrieves the list of variants from the enum definition.
    /// - Searches for a variant with the requested name.
    /// - Returns `Res::Variant` on success.
    ///
    /// # Parameters
    /// - `ty`: Fully instantiated enum type.
    /// - `name`: Name of the enum type (used for error reporting).
    /// - `field_span`: Source span.
    /// - `field_name`: Name of the variant being accessed.
    ///
    /// # Returns
    /// - `Res::Variant(ty, variant)`
    ///
    /// # Errors
    /// - [`FieldIsNotDefined`]: the variant does not exist in the enum.
    ///
    fn infer_enum_field_access(
        &mut self,
        ty: Typ,
        name: String,
        field_span: Span,
        field_name: String,
    ) -> Res {
        // Finding field
        match ty
            .variants(&mut self.icx)
            .into_iter()
            .find(|f| f.name == field_name)
        {
            Some(f) => Res::Variant(ty, f),
            None => bail!(TypeckError::FieldIsNotDefined {
                src: field_span.0,
                span: field_span.1.into(),
                t: name,
                field: field_name
            }),
        }
    }

    /// Resolves a field access on a struct type.
    ///
    /// This function:
    /// - Retrieves struct fields via the hydrator.
    /// - Searches for a field with the requested name.
    /// - Returns the type of the field inside `Res::Value`.
    ///
    /// # Parameters
    /// - `ty`: Fully instantiated struct type.
    /// - `name`: Struct name for error reporting.
    /// - `field_span`: Source code span.
    /// - `field_name`: Field name.
    ///
    /// # Returns
    /// - `Res::Value(f.typ)` if field exists.
    ///
    /// # Errors
    /// - [`FieldIsNotDefined`]: the field does not exist in the struct.
    ///
    fn infer_struct_field_access(
        &mut self,
        ty: Typ,
        name: String,
        field_span: Span,
        field_name: String,
    ) -> Res {
        // Finding field
        match ty
            .fields(&mut self.icx)
            .iter()
            .find(|f| f.name == field_name)
        {
            Some(f) => Res::Value(f.typ.clone()),
            None => bail!(TypeckError::FieldIsNotDefined {
                src: field_span.0,
                span: field_span.1.into(),
                t: name,
                field: field_name
            }),
        }
    }

    /// Infers any kind of field access Expr.
    ///
    /// Depending on what the container resolves to, this function does this:
    ///
    /// - calls                        `infer_module_field_access`  for module fields
    /// - instantiates enum and calls  `infer_enum_field_access`    for enum variants
    /// - calls                        `infer_struct_field_access`  for struct value fields
    ///
    /// # Parameters
    /// - `field_span`: span of the field access.
    /// - `container`: Expr on the left-hand side of `.`.
    /// - `field_name`: Requested field.
    ///
    /// # Returns
    /// - `Res` representing the resolved field.
    ///
    /// # Errors
    /// - [`CouldNotResolveFieldsIn`]: container is not a module/struct/enum.
    ///
    fn infer_field_access(&mut self, field_span: Span, container: Expr, field_name: String) -> Res {
        // Inferring container
        let container_inferred = self.infer_res(container);
        match &container_inferred {
            // Module field access
            Res::Module(name) => {
                // Inferring module field access
                self.infer_module_field_access(name.clone(), field_span, field_name)
            }
            // Enum field access
            Res::Custom(TypeDef::Enum(id)) => {
                // Instantiating enum
                let enum_ = self.icx.tcx.enum_(*id);
                let generics = enum_.generics.clone();
                let name = enum_.name.clone();
                let instantiated = Typ::Enum(*id, self.icx.mk_fresh_generics(&generics));

                // Inferring enum field access
                self.infer_enum_field_access(instantiated, name, field_span, field_name)
            }
            // Type field access
            Res::Value(it @ Typ::Struct(id, _)) => self.infer_struct_field_access(
                it.clone(),
                self.icx.tcx.struct_(*id).name.clone(),
                field_span,
                field_name,
            ),
            // Else
            _ => bail!(TypeckError::CouldNotResolveFieldsIn {
                src: field_span.0,
                span: field_span.1.into(),
                t: container_inferred.pretty(&mut self.icx),
            }),
        }
    }

    /// Ensures arity of parameters and arguments.
    ///
    /// # Parameters
    /// - `span`: span of the field access.
    /// - `expected`: Expected amount of parameters.
    /// - `got`: Amount of passed parameters.
    fn ensure_arity(&self, span: Span, expected: usize, got: usize) {
        if expected != got {
            bail!(TypeckError::ArityMissmatch {
                related: vec![TypeckRelated::Here {
                    src: span.0,
                    span: span.1.into(),
                }],
                expected,
                got
            })
        }
    }

    /// Infers the type of function or constructor call.
    ///
    /// This routine performs three major tasks:
    /// 1. Resolves the callee (`what`) via [`infer_resolution`] to determine whether it is:
    ///    - a function,
    ///    - a struct constructor,
    ///    - an enum variant,
    ///    - or an invalid Expr,
    /// 2. Infers the types of all argument Exprs,
    /// 3. Produces unification constraints between the expected and provided argument types.
    ///
    /// ### Struct constructor call
    /// If the callee resolves to a custom struct type (`Res::Custom(TypeDef::Struct)`),
    /// the hydrator instantiates its generic parameters with fresh variables.
    /// Then each struct field type is unified with the corresponding argument.
    ///
    /// ### Function call
    /// If the callee resolves to a function (`Typ::Function`), the function signature
    /// is instantiated via [`Hydrator::mk_function`] and each parameter is unified with
    /// the corresponding argument Expr.
    ///
    /// ### Enum variant construction
    /// If the callee is an enum variant (`Res::Variant`), each variant field is unified
    /// with its corresponding argument Expr. We don't instantiate the enum,
    /// because it was already instantiated during enum variant / enum field lookup.
    ///
    /// ### Errors
    /// - [`TypeckError::CouldNotCall`]: the callee is not callable (e.g. an integer).
    /// - ['TypeckError::ArityMismatch`]: or type mismatches are detected via solver unification.
    ///
    /// Returns a resolved `Res::Value` with the instantiated type of the Expr.
    ///
    pub(crate) fn infer_call(&mut self, span: Span, what: Expr, args: Vec<Expr>) -> Res {
        let function = self.infer_res(what);
        let args = args
            .into_iter()
            .map(|a| (a.span(), self.infer_expr(a)))
            .collect::<Vec<(Span, Typ)>>();

        match function.clone() {
            // Custom type
            Res::Custom(TypeDef::Struct(id)) => {
                // Instantiating struct
                let struct_ = self.icx.tcx.struct_(id);
                let generics = struct_.generics.clone();
                self.ensure_arity(span, struct_.fields.len(), args.len());
                let instantiated = Typ::Struct(id, self.icx.mk_fresh_generics(&generics));

                // Unifying fields and args
                instantiated
                    .fields(&mut self.icx)
                    .into_iter()
                    .zip(args)
                    .for_each(|(p, a)| {
                        coercion::coerce(
                            &mut self.icx,
                            Cause::StructArgument(&a.0),
                            Coercion::Eq(p.typ, a.1),
                        );
                    });

                Res::Value(instantiated)
            }
            // Calling function definition
            Res::Value(Typ::FnDef(id, generic_args)) => {
                // Instantiating function
                let function = self.icx.tcx.function(id);
                self.ensure_arity(span, function.params.len(), args.len());
                let typ = Typ::FnDef(id, generic_args);

                // Unifying params and args
                typ.params(&mut self.icx)
                    .into_iter()
                    .zip(args)
                    .for_each(|(p, a)| {
                        coercion::coerce(
                            &mut self.icx,
                            Cause::FunctionArgument(&a.0),
                            Coercion::Eq(p, a.1),
                        );
                    });

                Res::Value(typ.ret(&mut self.icx))
            }
            // Calling function value
            Res::Value(Typ::FnRef(params, ret)) => {
                // Unifying params and args
                params.into_iter().zip(args).for_each(|(p, a)| {
                    coercion::coerce(
                        &mut self.icx,
                        Cause::FunctionArgument(&a.0),
                        Coercion::Eq(p, a.1),
                    );
                });

                Res::Value(*ret)
            }
            // Variant
            Res::Variant(en, variant) => {
                variant.fields.iter().cloned().zip(args).for_each(|(p, a)| {
                    coercion::coerce(
                        &mut self.icx,
                        Cause::VariantArgument(&a.0),
                        Coercion::Eq(p, a.1),
                    );
                });

                Res::Value(en)
            }
            _ => bail!(TypeckError::CouldNotCall {
                src: span.0.clone(),
                span: span.1.clone().into(),
                t: function.pretty(&mut self.icx),
            }),
        }
    }

    /// Performs name/field resolution on an Expr that appears in a "call position".
    ///
    /// This function is responsible only for *resolving what the Expr refers to*.
    /// It does **not** infer full Expr types (that's [`infer_expr`]).
    ///
    /// Supported resolution forms:
    /// - `PrefixVar`: simple variable access,
    /// - `SuffixVar`: field access (`a.b`),
    /// - nested calls (`f(x)(y)`), which recursively call [`infer_call`].
    ///
    /// Any other Expr that cannot denote a callable value or a namespace entry
    /// triggers [`TypeckError::UnexpectedExprInResolution`].
    ///
    /// This function is typically used at the entry point of call inference
    /// and pattern matching, where the compiler needs to know *what* is being referenced.
    ///
    pub(crate) fn infer_res(&mut self, expr: Expr) -> Res {
        match expr {
            Expr::Var(span, name) => self.infer_get(span, name),
            Expr::Suffix(span, container, name) => self.infer_field_access(span, *container, name),
            Expr::Call(span, what, args) => self.infer_call(span.clone(), *what, args),
            expr => bail!(TypeckError::UnexpectedExprInResolution {
                expr: format!("{expr:?}").into()
            }),
        }
    }

    /// Infers the type of anonymous function literal.
    ///
    /// This creates a temporary local scope, binds parameters with their declared
    /// annotated types, and infers the type of the function body.
    ///
    /// ### Return type
    /// - If an explicit return type is provided, it is used.
    /// - Otherwise the return type defaults to `Unit`, but is unified with the inferred body.
    ///
    /// ### Parameters
    /// Parameter types must always be annotated; the inference engine does not attempt
    /// to infer parameter types from usage (similar to Rust).
    ///
    /// ### Scoping
    /// A new rib (scope) is pushed for the function parameters. After the body is
    /// inferred and unified, the rib is popped.
    ///
    /// Returns a fully constructed `Typ::Function` containing:
    /// - inferred parameter list,
    /// - inferred return type,
    /// - captured generics (**Always** empty for anonymous functions).
    ///
    fn infer_anonymous_fn(
        &mut self,
        span: Span,
        params: Vec<Param>,
        body: Expr,
        ret_type: Option<TypeHint>,
    ) -> Typ {
        // inferring return type
        let ret = ret_type.map_or(Typ::Unit, |t| self.infer_type_hint(t));

        // inferred params
        let params = params
            .into_iter()
            .map(|p| (p.name, self.infer_type_hint(p.hint)))
            .collect::<Vec<(String, Typ)>>();

        // pushing new scope
        self.resolver.push_rib();

        // defining params in new scope
        params
            .iter()
            .for_each(|p| self.resolver.define_local(&span, &p.0, p.1.clone()));

        // inferring body
        let (block_span, inferred_block) = (body.span(), self.infer_expr(body));
        coercion::coerce(
            &mut self.icx,
            Cause::Return(&block_span, &span),
            Coercion::Eq(inferred_block, ret.clone()),
        );
        self.resolver.pop_rib();

        // result
        Typ::FnRef(params.iter().map(|p| p.1.clone()).collect(), Box::new(ret))
    }

    /// Performs semantic/type analysis of a single match arm pattern.
    ///
    /// Validates the correctness of a pattern
    /// against the expected type of the matched value (`inferred_what`).
    ///
    /// ### Responsibilities:
    /// - Verifies enum variant constructors used in patterns,
    /// - Verifies the correctness of fields in an `Unwrap` pattern,
    /// - Ensures literals (`Int`, `Float`, etc.) match the expected type,
    /// - Handles wildcards (`_`) and variable binding patterns,
    /// - Recursively validates `pat1 | pat2`
    ///
    /// ### Errors:
    /// - [`TypeckError::TypesMissmatch`] — literal or variant does not match the scrutinee type.
    /// - [`TypeckError::WrongUnwrapPattern`] — using `.field` pattern on non-variant.
    /// - [`TypeckError::EnumVariantFieldIsNotDefined`] — non-existent field in variant.
    /// - [`TypeckError::WrongVariantPattern`] — non-variant used where variant pattern expected.
    ///
    /// This function may introduce new local bindings (for `BindTo`) into the current rib.
    ///
    fn analyze_pattern(&mut self, what_span: Span, inferred_what: Typ, case: &Case, pat: &Pat) {
        // matching pattern
        match pat.clone() {
            Pat::Unwrap(span, en, fields) => {
                // inferring resolution, and checking
                // that is an enum variant
                let res = self.infer_res(en);
                match &res {
                    Res::Variant(en, variant) => {
                        // Checking types equality
                        coercion::coerce(
                            &mut self.icx,
                            Cause::Pattern(&what_span, &span),
                            Coercion::Eq(inferred_what, en.clone()),
                        );

                        // Checking fields arity
                        if fields.len() != variant.fields.len() {
                            bail!(TypeckError::EnumVariantFieldsArityMissmatch {
                                src: span.0,
                                span: span.1.into(),
                                arity: variant.fields.len(),
                                expected: fields.len(),
                            })
                        }

                        // If types equal, checking fields existence
                        fields
                            .into_iter()
                            .zip(&variant.fields)
                            .for_each(|(f, vf)| match f {
                                UnwrapField::Field(s, f) => {
                                    self.resolver.define_local(&s, &f, vf.clone())
                                }
                                UnwrapField::Wildcard(_) => {}
                            });
                    }
                    _ => bail!(TypeckError::WrongUnwrapPattern {
                        src: case.span.0.clone(),
                        span: case.span.1.clone().into(),
                        got: res.pretty(&mut self.icx),
                    }),
                }
            }
            Pat::Int(span, _) => {
                let typ = Typ::Prelude(PreludeType::Int);
                // Checking types equality
                coercion::coerce(
                    &mut self.icx,
                    Cause::Pattern(&what_span, &span),
                    Coercion::Eq(inferred_what, typ.clone()),
                );
            }
            Pat::Float(span, _) => {
                let typ = Typ::Prelude(PreludeType::Float);
                // Checking types equality
                coercion::coerce(
                    &mut self.icx,
                    Cause::Pattern(&what_span, &span),
                    Coercion::Eq(inferred_what, typ.clone()),
                );
            }
            Pat::String(span, _) => {
                let typ = Typ::Prelude(PreludeType::String);
                // Checking types equality
                coercion::coerce(
                    &mut self.icx,
                    Cause::Pattern(&what_span, &span),
                    Coercion::Eq(inferred_what, typ.clone()),
                );
            }
            Pat::Bool(span, _) => {
                let typ = Typ::Prelude(PreludeType::Bool);
                // Checking types equality
                coercion::coerce(
                    &mut self.icx,
                    Cause::Pattern(&what_span, &span),
                    Coercion::Eq(inferred_what, typ.clone()),
                );
            }
            Pat::Wildcard => {}
            Pat::Variant(span, var) => {
                // inferring resolution, and checking
                // that is an enum variant
                let res = self.infer_res(var);
                match &res {
                    Res::Variant(en, _) => {
                        // Checking types equality
                        coercion::coerce(
                            &mut self.icx,
                            Cause::Pattern(&what_span, &span),
                            Coercion::Eq(inferred_what, en.clone()),
                        );
                    }
                    _ => bail!(TypeckError::WrongVariantPattern {
                        src: case.span.0.clone(),
                        span: case.span.1.clone().into(),
                        got: res.pretty(&mut self.icx),
                    }),
                }
            }
            Pat::BindTo(span, name) => {
                self.resolver
                    .define_local(&span, &name, inferred_what.clone());
            }
            Pat::Or(pat1, pat2) => {
                self.analyze_pattern(what_span.clone(), inferred_what.clone(), case, &pat1);
                self.analyze_pattern(what_span, inferred_what, case, &pat2);
            }
        }
    }

    /// Infers the result type of `match` Expr.
    ///
    /// Steps performed:
    /// 1. Infer the matchable type (`inferred_what`).
    /// 2. For each case:
    ///    - push a new rib,
    ///    - analyze its pattern via [`analyze_pattern`],
    ///    - infer the type of its body,
    ///    - collect all case body types for unification,
    ///    - pop the rib.
    /// 3. Unify all case body types yielding the final type of the `match`.
    /// 4. Perform exhaustiveness checking using [`ExMatchCx::check`].
    ///
    /// ### Exhaustiveness
    /// If the match is not exhaustive:
    /// - Emit a warning (`TypeckWarning::NonExhaustive`),
    /// - The whole match Expr is typed as `Unit`.
    ///
    /// Otherwise, return the unified type of all branches.
    ///
    pub(crate) fn infer_pattern_matching(
        &mut self,
        span: Span,
        what: Expr,
        cases: Vec<Case>,
    ) -> Typ {
        // inferring matchable
        let what_span = what.span();
        let inferred_what = self.infer_expr(what);
        // to unify
        let mut to_unify = Vec::new();
        // type checking cases
        for case in cases.clone() {
            // pattern scope start
            self.resolver.push_rib();
            // analyzing pattern
            self.analyze_pattern(what_span.clone(), inferred_what.clone(), &case, &case.pat);
            // analyzing body
            let (case_span, inferred_case) = (case.body.span(), self.infer_expr(case.body));
            to_unify.push((case_span, inferred_case));
            // pattern scope end
            self.resolver.pop_rib();
        }
        // solving types
        let fresh = Typ::Var(self.icx.fresh());
        for branch in to_unify {
            coercion::coerce(
                &mut self.icx,
                Cause::Branch(&span, &branch.0),
                Coercion::Eq(fresh.clone(), branch.1),
            );
        }
        let checked = ExMatchCx::check(self, inferred_what, cases);
        // checking all cases covered
        if checked {
            self.icx.apply(fresh)
        } else {
            bail!(TypeckError::NonExhaustive {
                src: span.0,
                span: span.1.into()
            });
        }
    }

    /// Infers the type of `if`/`elif`/`else` chain.
    ///
    /// ### Logical Expr
    /// Ensures that each `if` and `elif` condition has type `Bool`.
    /// Otherwise, emits [`TypeckError::ExpectedLogicalInIf`].
    ///
    /// ### Branch types
    /// All reachable branches are collected into a list and unified together.
    /// If the final `else` branch is missing, the whole `if` Expr evaluates to `Unit`.
    ///
    /// ### Scoping
    /// Each `if` branch introduces a new rib; scoping is handled consistently like in blocks.
    ///
    /// Returns:
    /// - The unified type of all branches if an `else` exists,
    /// - Otherwise `Unit`.
    ///
    fn infer_if(&mut self, span: Span, logical: Expr, body: Expr, else_: Option<Expr>) -> Typ {
        // pushing rib
        self.resolver.push_rib();
        // inferring logical
        let inferred_logical = self.infer_expr(logical);
        match inferred_logical {
            Typ::Prelude(PreludeType::Bool) => {}
            _ => {
                bail!(TypeckError::ExpectedLogicalInIf {
                    src: span.0,
                    span: span.1.into()
                })
            }
        }
        // inferring block
        let (if_span, inferred_if) = (body.span(), self.infer_expr(body));
        // popping rib
        self.resolver.pop_rib();

        // unifying with else
        if let Some(expr) = else_ {
            let (else_span, else_typ) = (expr.span(), self.infer_expr(expr));
            coercion::coerce(
                &mut self.icx,
                Cause::Branch(&if_span, &else_span),
                Coercion::Eq(inferred_if.clone(), else_typ),
            );
            inferred_if
        } else {
            // todo: error
            Typ::Unit
        }
    }

    /// Infers an assignment (`x = value`).
    ///
    /// ## Steps:
    /// - Resolve the left-hand side (`what`) and check that it is not a constant.
    /// - Infer the type of the assign value and instantiate its type.
    /// - Emit an coercion unifying the variable's type and the value's type.
    ///
    /// ## Errors:
    /// - [`TypeckError::CouldNotAssignConstant`] if the left-hand side refers to a constant.
    ///
    fn infer_assign(&mut self, span: Span, what: Expr, value: Expr) -> Typ {
        let inferred_what = self.infer_res(what);
        if let Res::Const(_) = inferred_what {
            bail!(TypeckError::CouldNotAssignConstant {
                src: span.0.clone(),
                span: span.1.clone().into(),
            })
        }
        let inferred_value = self.infer_expr(value);
        let coercion = Coercion::Eq(
            inferred_what.unwrap_typ(&mut self.icx, &span),
            inferred_value,
        );
        coercion::coerce(&mut self.icx, Cause::Assignment(&span), coercion);
        Typ::Unit
    }

    /// The central entry point for Expr type inference.
    ///
    /// Dispatches to specialized inference routines depending on Expr kind:
    /// - literals → primitive `PreludeType`,
    /// - variable and field access,
    /// - calls (`infer_call`),
    /// - anonymous functions (`infer_anonymous_fn`),
    /// - binary and unary ops,
    /// - match/if constructs.
    ///
    /// After the initial inference, the result is passed through the hydrator
    /// (`Hydrator::apply`) to resolve any pending substitutions of unbounds.
    ///
    /// This guarantees that the final type is always normalized.
    ///
    pub(crate) fn infer_expr(&mut self, expr: Expr) -> Typ {
        // Inferencing Expr
        let result = match expr {
            Expr::Lit(_, Lit::Float(_)) => Typ::Prelude(PreludeType::Float),
            Expr::Lit(_, Lit::Int(_)) => Typ::Prelude(PreludeType::Int),
            Expr::Lit(_, Lit::String(_)) => Typ::Prelude(PreludeType::String),
            Expr::Lit(_, Lit::Bool(_)) => Typ::Prelude(PreludeType::Bool),
            Expr::Todo(..) => Typ::Var(self.icx.fresh()),
            Expr::Panic(..) => Typ::Var(self.icx.fresh()),
            Expr::Unary(span, value, op) => self.infer_unary(span, op, *value),
            Expr::Bin(span, lhs, rhs, op) => self.infer_binary(span, op, *lhs, *rhs),
            Expr::Var(span, name) => self
                .infer_get(span.clone(), name)
                .unwrap_typ(&mut self.icx, &span),
            Expr::Suffix(span, container, name) => self
                .infer_field_access(span.clone(), *container, name)
                .unwrap_typ(&mut self.icx, &span),
            Expr::Call(span, what, args) => self
                .infer_call(span.clone(), *what, args)
                .unwrap_typ(&mut self.icx, &span),
            Expr::Function(_, _, _) => todo!(),
            Expr::Match(span, value, cases, ..) => self.infer_pattern_matching(span, *value, cases),
            Expr::If(span, logical, body, else_) => {
                self.infer_if(span, *logical, *body, else_.map(|it| *it))
            }
            Expr::Paren(_, expr) => self.infer_expr(*expr),
            Expr::Assign(span, what, value, _todo) => self.infer_assign(span, *what, *value),
            Expr::Block(_, stmts) => self.infer_block(stmts),
            Expr::None(_) => Typ::Unit,
        };
        // Applying substs
        self.icx.apply(result)
    }
}
