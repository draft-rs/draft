/// Imports
use crate::{
    cx::package::PackageCx,
    resolve::resolve::ModuleResolver,
    typ::{
        cx::{InferCx, TyCx},
        typ::Module,
    },
};
use draft_ast::untyped;

/// Module ctx
pub struct ModuleCx<'pkg, 'cx> {
    /// Current analyzing module info
    pub(crate) module: &'pkg untyped::Module,
    pub(crate) module_name: &'pkg String,
    /// Resolver
    pub(crate) resolver: ModuleResolver,
    /// Inference context
    pub(crate) icx: InferCx<'cx>,
    /// Root package context
    pub(crate) package: &'cx PackageCx<'cx>,
}

/// Implementation
impl<'pkg, 'cx> ModuleCx<'pkg, 'cx> {
    /// Creates new module analyzer
    pub fn new(
        module: &'pkg untyped::Module,
        module_name: &'pkg String,
        tcx: &'cx mut TyCx,
        package: &'cx PackageCx<'pkg>,
    ) -> Self {
        Self {
            module,
            module_name,
            resolver: ModuleResolver::default(),
            icx: InferCx::new(tcx),
            package,
        }
    }

    /// Performs analyze of module
    pub fn analyze(&mut self) -> Module {
        self.pipeline()
    }
}
