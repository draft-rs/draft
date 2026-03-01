/// Imports
use crate::{
    cx::module::ModuleCx,
    errors::ExError,
    typ::{
        res::Res,
        typ::{Enum, EnumVariant, PreludeType, Typ},
    },
};
use draft_ast::untyped::{Case, Pat, UnwrapField};
use draft_common::{bail, span::Span};
use id_arena::Id;

/// Context for exhaustiveness checking in pattern matching.
///
/// `ExMatchCx` is used by the compiler to analyze all possible cases
/// when performing a `match` and to ensure that all possible variants
/// of the `value` type are covered by the provided cases.
///
/// Lifetime parameters:
/// - `'module_cx` — a reference to the module context where the check is performed.
/// - `'pkg` — a reference to the package meta context (for access to types, modules, and dependencies).
/// - `'cx` — root context lifetime.
///
pub struct ExMatchCx<'module_cx, 'pkg, 'cx> {
    /// Reference to the module context where the match analysis occurs.
    /// Allows access to functions, types, and other entities in the module.
    cx: &'module_cx mut ModuleCx<'pkg, 'cx>,

    /// The type of the value being matched.
    value: Typ,

    /// The list of match cases (`Case`) provided in the `match` expression.
    cases: Vec<Case>,
}

/// Implementation
impl<'module_cx, 'pkg, 'cx> ExMatchCx<'module_cx, 'pkg, 'cx> {
    /// Checks that all possible values
    /// are covered.
    pub fn check(cx: &'module_cx mut ModuleCx<'pkg, 'cx>, value: Typ, cases: Vec<Case>) -> bool {
        // Match cx
        let mut ex = Self { cx, value, cases };
        // Matching value
        match &ex.value {
            // All prelude type possible values
            // could not be covered, except boolean.
            Typ::Prelude(typ) => match typ {
                PreludeType::Bool => ex.check_bool_values_covered(),
                _ => ex.has_default_pat(&ex.cases),
            },
            // All custom type values
            // could not be covered,
            // because it's a ref type.
            //
            // So, checking for default patterns
            // `BindTo` and `Wildcard`
            Typ::Struct(_, _) => ex.has_default_pat(&ex.cases),
            // All enum variant values
            // could be covered, so
            // checking it
            //
            // So, checking for default patterns
            // `BindTo` and `Wildcard`
            Typ::Enum(en, _) => {
                ex.has_default_pat(&ex.cases) || ex.check_enum_variants_covered(*en)
            }
            // All function values
            // cold not be covered,
            // becuase it's a ref type.
            //
            // So, checking for default patterns
            // `BindTo` and `Wildcard`
            Typ::FnDef(_, _) | Typ::FnRef(_, _) => ex.has_default_pat(&ex.cases),
            // Could not cover unit
            // values, becuase...
            // it's nothing =)
            //
            // So, checking for default patterns
            // `BindTo` and `Wildcard`
            Typ::Unit => ex.has_default_pat(&ex.cases),
            // All type variable values
            // could not be covered,
            // because it's a unknown type
            // with unknown constraints
            //
            // So, checking for default patterns
            // `BindTo` and `Wildcard`
            Typ::Var(_) => ex.has_default_pat(&ex.cases),
            // All generic values
            // could not be covered,
            // because it's a unknown type
            // with unknown constraints
            //
            // So, checking for default patterns
            // `BindTo` and `Wildcard`
            Typ::Generic(_) => ex.has_default_pat(&ex.cases),
        }
    }

    /// Checks that `BindTo` or `Wildcard` pattern exists in cases vec
    fn has_default_pat(&self, cases: &Vec<Case>) -> bool {
        // Checking for patterns
        for case in cases {
            match case.pat {
                Pat::BindTo(_, _) => return true,
                Pat::Wildcard => return true,
                _ => continue,
            }
        }
        // Else
        false
    }

    /// Checks that true or false is matched
    /// (true_matched, false_matched)
    fn check_bool_pat(pattern: &Pat) -> (bool, bool) {
        // Matching pattern
        match &pattern {
            // Bool pattern
            Pat::Bool(_, val) => match val.as_str() {
                "true" => (true, false),
                "false" => (false, true),
                _ => unreachable!(),
            },
            // Or pattern
            Pat::Or(pat1, pat2) => {
                let first = Self::check_bool_pat(pat1);
                let second = Self::check_bool_pat(pat2);
                (first.0 || second.0, first.1 || second.1)
            }
            // Other
            _ => (false, false),
        }
    }

    /// Checks that all possible
    /// bool values (true, false) are covered
    fn check_bool_values_covered(&mut self) -> bool {
        // True matched
        let mut true_matched = false;
        let mut false_matched = false;
        // Matching all cases
        for case in &self.cases {
            match Self::check_bool_pat(&case.pat) {
                (true, true) => return true,
                (true, false) => {
                    true_matched = true;
                }
                (false, true) => {
                    false_matched = true;
                }
                _ => {}
            };
        }
        // If not not matched
        (true_matched && false_matched) || self.has_default_pat(&self.cases)
    }

    /// Ensures all enum patterns are consistent
    fn ensure_enum_pats_consistent(&mut self, span: Span, pat1: &Pat, pat2: &Pat) {
        /// Collects all patterns
        fn collect_pats(pattern: &Pat) -> Vec<Pat> {
            let mut patterns = Vec::new();
            match pattern {
                Pat::Or(pat1, pat2) => {
                    patterns.append(&mut collect_pats(pat1));
                    patterns.append(&mut collect_pats(pat2));
                }
                pattern => patterns.push(pattern.clone()),
            }
            patterns
        }

        // Collecting all patterns
        let mut collected_pats = Vec::new();
        collected_pats.append(&mut collect_pats(pat1));
        collected_pats.append(&mut collect_pats(pat2));

        // Collecting enum patterns
        let enum_pats: Vec<Pat> = collected_pats
            .into_iter()
            .filter(|pattern| matches!(pattern, Pat::Unwrap { .. } | Pat::Variant(_, _)))
            .collect();

        // Collecting unwrap patterns
        let unwrap_pats: Vec<Vec<UnwrapField>> = enum_pats
            .iter()
            .filter_map(|pattern| {
                if let Pat::Unwrap(_, _, fields) = pattern {
                    Some(fields.clone())
                } else {
                    None
                }
            })
            .collect();

        // If exists at least one unwrap pattern, checking
        // unwrap fields consistent
        if !unwrap_pats.is_empty() {
            // If `variant_patterns` and `unwrap_patterns`
            // are missmatched, raising error
            if enum_pats.len() != unwrap_pats.len() {
                bail!(ExError::EnumPatternsMissmatch {
                    src: self.cx.module.source.clone(),
                    span: span.1.into()
                })
            }

            // Checking that all unwrap patterns fields are same
            let first = unwrap_pats.first().unwrap();
            for pat in &unwrap_pats {
                if pat != first {
                    bail!(ExError::EnumUnwrapFieldsMissmatch {
                        src: self.cx.module.source.clone(),
                        span: span.1.into()
                    })
                }
            }
        }
    }

    /// Collects matched variants
    fn collect_enum_variants(&mut self, span: &Span, pattern: &Pat) -> Vec<EnumVariant> {
        // Matched variants
        let mut variants = Vec::new();
        // Matching pattern
        match pattern {
            Pat::Unwrap(_, en, _) => match self.cx.infer_res(en.clone()) {
                Res::Variant(_, pattern_variant) => {
                    variants.push(pattern_variant);
                }
                _ => unreachable!(),
            },
            Pat::Variant(_, var) => match self.cx.infer_res(var.clone()) {
                Res::Variant(_, pattern_variant) => {
                    variants.push(pattern_variant);
                }
                _ => unreachable!(),
            },
            Pat::Or(pat1, pat2) => {
                // Collecting variants
                variants.append(&mut self.collect_enum_variants(span, pat1));
                variants.append(&mut self.collect_enum_variants(span, pat2));

                // Ensuring that enum patterns are consistent
                self.ensure_enum_pats_consistent(span.clone(), pat1, pat2)
            }
            _ => return variants,
        }
        variants
    }

    /// Checks that all possible
    /// enum variants are covered
    fn check_enum_variants_covered(&mut self, en: Id<Enum>) -> bool {
        // Matching all cases
        let mut matched_variants = Vec::new();
        for case in std::mem::take(&mut self.cases) {
            matched_variants.append(&mut self.collect_enum_variants(&case.span, &case.pat));
        }

        // Deleting duplicates
        matched_variants.dedup();

        // Checking all patterns covered
        matched_variants.len() == self.cx.icx.tcx.enum_(en).variants.len()
    }
}
