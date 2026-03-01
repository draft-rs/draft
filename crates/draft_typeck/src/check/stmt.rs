/// Imports
use crate::{
    cx::module::ModuleCx,
    inference::{
        cause::Cause,
        coercion::{self, Coercion},
    },
    typ::typ::Typ,
};
use draft_ast::untyped::{Expr, Stmt, TypeHint};
use draft_common::span::Span;

/// Statements inferencing
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Analyzes a `let` variable definition.
    ///
    /// ## Steps:
    /// - Infer the type of the initialization expression (`value`) and instantiate it.
    /// - If the declaration includes a type annotation:
    ///     - Infer the annotation type.
    ///     - Emit a unification equation requiring the annotated and inferred
    ///       types to be equal.
    ///     - Define the variable with the annotated type.
    /// - If no annotation was provided:
    ///     - Define the variable using the inferred type.
    ///
    pub(crate) fn analyze_let(
        &mut self,
        span: Span,
        name: String,
        typ: Option<TypeHint>,
        value: Expr,
    ) {
        let inferred_value = self.infer_expr(value);
        match typ {
            Some(annotated_path) => {
                let annotated = self.infer_type_hint(annotated_path);
                let coercion = Coercion::Eq(annotated.clone(), inferred_value);
                coercion::coerce(&mut self.icx, Cause::Assignment(&span), coercion);
                self.resolver.define_local(&span, &name, annotated)
            }
            None => self.resolver.define_local(&span, &name, inferred_value),
        }
    }

    /// Infers the type of statement.
    ///
    /// ## Behavior by statement kind:
    /// - `Expr` — evaluates to the expression’s type.
    /// - `VarDef` — delegates to [`analyze_let_define`] and returns `Unit`.
    /// - `VarAssign` — delegates to [`analyze_assignment`] and returns `Unit`.
    /// - `Loop` — delegates to [`analyze_loop`] and returns `Unit`.
    /// - `For` — delegates to [`analyze_for`] and returns `Unit`.
    /// - `Semi(expr)` — infers the expression, discards its value, returns `Unit`.
    ///
    fn infer_stmt(&mut self, stmt: Stmt) -> Typ {
        match stmt {
            Stmt::Expr(expression) => self.infer_expr(expression),
            Stmt::Let(span, name, typ, value) => {
                self.analyze_let(span, name, typ, value);
                Typ::Unit
            }
            Stmt::Semi(expr) => {
                self.infer_expr(expr);
                Typ::Unit
            }
        }
    }

    /// Infers the type of block.
    ///
    /// ## Rules:
    /// - All statements except the last are treated as having type `Unit`.
    /// - The type of the block is the type of the last statement.
    /// - Empty blocks evaluate to `Unit`.
    ///
    /// ## Implementation:
    /// - Pop the last statement.
    /// - Infer all previous statements via [`infer_stmt`].
    /// - Infer and return the type of the last statement.
    ///
    pub(crate) fn infer_block(&mut self, mut block: Vec<Stmt>) -> Typ {
        // Last stmt
        let last = match block.pop() {
            Some(last) => last,
            None => return Typ::Unit, // Block is empty
        };
        // Analyzing each statement
        for stmt in block {
            self.infer_stmt(stmt);
        }
        // Inferring last
        self.infer_stmt(last)
    }
}
