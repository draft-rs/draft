/// Imports
use crate::{
    cx::module::ModuleCx,
    inference::{
        cause::Cause,
        coercion::{self, Coercion},
    },
    typ::{res::Res, typ::Typ},
};
use draft_ast::untyped;

/// Late declaration analysis pass for the module.
///
/// This pass completes the semantic analysis of declarations (structs, enums,
/// functions, extern functions, constants) after their names and initial shells
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
    /// Performs late analysis of a user-defined function.
    ///
    /// ## Steps:
    /// - Look up the function shell previously registered by the early pass.
    /// - Re-push generic parameters into the hydrator.
    /// - Resolve the return type if annotated; otherwise assume `Unit`.
    /// - Resolve the types of all parameters, constructing a typed signature.
    /// - Publish the function signature into the module (so it is visible to
    ///   recursive calls within its own body).
    /// - Create a new scope (rib) for local variables.
    /// - Insert parameters as locals into that scope.
    /// - Infer the function body (block or expression).
    /// - Emit a unification equation requiring: `inferred_body_type == return_type`.
    /// - Pop the local scope.
    /// - Pop the generic parameter scope.
    ///
    /// At the end of this method the function is fully type-checked.
    pub(crate) fn late_analyze_fn(&mut self, item: untyped::Fn) {
        // Requesting function
        let id = match self.resolver.resolve(&mut self.icx, &item.span, &item.name) {
            Res::Value(Typ::FnDef(f, _)) => f,
            _ => unreachable!(),
        };
        let def = self.icx.tcx.function_mut(id);
        let params: Vec<Typ> = def.params.clone();
        let ret = def.ret.clone();

        // Pushing generics
        self.icx.generics.push_scope(item.generics);

        // pushing new scope
        self.resolver.push_rib();

        // Defining params in new scope
        item.params
            .iter()
            .zip(params)
            .for_each(|(p, t)| self.resolver.define_local(&item.span, &p.name, t));

        // Inferring body
        let (block_span, inferred_block) = (item.block.span(), self.infer_expr(item.block));
        coercion::coerce(
            &mut self.icx,
            Cause::Return(&block_span, &item.span),
            Coercion::Eq(inferred_block, ret),
        );
        self.resolver.pop_rib();

        // Popping generics
        self.icx.generics.pop_scope();
    }
}
