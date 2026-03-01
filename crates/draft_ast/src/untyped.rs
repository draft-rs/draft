/// Imports
use crate::common::{AssignOp, BinOp, Lit, Publicity, UnOp};
use draft_common::span::Span;
use miette::NamedSource;
use std::sync::Arc;

/// Represents type hint
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeHint {
    /// Local type
    Local {
        span: Span,
        name: String,
        generics: Vec<TypeHint>,
    },
    /// Module type
    Module {
        span: Span,
        module: String,
        name: String,
        generics: Vec<TypeHint>,
    },
    /// Function type
    Function {
        span: Span,
        params: Vec<TypeHint>,
        ret: Option<Box<TypeHint>>,
    },
    /// Unit type
    Unit(Span),
}

/// Function param
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub span: Span,
    pub name: String,
    pub hint: TypeHint,
}

/// Represents unwrap field
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnwrapField {
    // `_`
    Wildcard(Span),
    // Field
    Field(Span, String),
}

/// Represents unwrap pattern
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    /// Represents enum fields unwrap pattern
    Unwrap(Span, Expr, Vec<UnwrapField>),

    /// Represents just enum variant pattern
    Variant(Span, Expr),

    /// Represents integer pattern, e.g `123`
    Int(Span, String),

    /// Represents float pattern, e.g `1.34`
    Float(Span, String),

    /// Represents bool pattern, e.g `true` / `false
    Bool(Span, String),

    /// Represents string pattern, e.g "Hello, world!"
    String(Span, String),

    /// Represents bind pattern
    BindTo(Span, String),

    /// Represents wildcard pattern
    Wildcard,

    /// Represents or pattern
    Or(Box<Pat>, Box<Pat>),
}

/// Represents case
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Case {
    pub span: Span,
    pub pat: Pat,
    pub body: Expr,
}

/// Represents expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    /// Literal expression
    Lit(Span, Lit),

    /// Represents todo expression (e.g `todo as "simple todo"`)
    Todo(Span, Option<String>),

    /// Represents panic expression (e.g `panic as "simple panic"`)
    Panic(Span, Option<String>),

    /// Represents unary expression
    Unary(Span, Box<Expr>, UnOp),

    /// Represents binary expression
    Bin(Span, Box<Expr>, Box<Expr>, BinOp),

    /// Assignment expression
    Assign(Span, Box<Expr>, Box<Expr>, AssignOp),

    /// Represents if expression (cond, then, else)
    If(Span, Box<Expr>, Box<Expr>, Option<Box<Expr>>),

    /// Represents variable access
    Var(Span, String),

    /// Represents field access
    Suffix(Span, Box<Expr>, String),

    /// Represents call expression
    Call(Span, Box<Expr>, Vec<Expr>),

    /// Represents anonymous function expression
    Function(Span, Vec<String>, Box<Expr>),

    /// Represents match expression
    Match(Span, Box<Expr>, Vec<Case>),

    /// Represents paren expression
    Paren(Span, Box<Expr>),

    /// Block expression
    Block(Span, Vec<Stmt>),

    /// None expression
    None(Span),
}

/// Implementation
impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit(span, ..) => span.clone(),
            Expr::Panic(span, ..) => span.clone(),
            Expr::Todo(span, ..) => span.clone(),
            Expr::Bin(span, ..) => span.clone(),
            Expr::Assign(span, ..) => span.clone(),
            Expr::Unary(span, ..) => span.clone(),
            Expr::If(span, ..) => span.clone(),
            Expr::Var(span, ..) => span.clone(),
            Expr::Suffix(span, ..) => span.clone(),
            Expr::Call(span, ..) => span.clone(),
            Expr::Function(span, ..) => span.clone(),
            Expr::Match(span, ..) => span.clone(),
            Expr::Paren(span, ..) => span.clone(),
            Expr::Block(span, ..) => span.clone(),
            Expr::None(span) => span.clone(),
        }
    }
}

/// Statement kind
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    /// Let definition
    Let(Span, String, Option<TypeHint>, Expr),

    /// Expr without trailing semi-colon
    Expr(Expr),

    /// Expr with trailing semi-colon
    Semi(Expr),
}

/// Implementation
impl Stmt {
    /// Returns true if statement requires semicolon after it
    pub fn requires_semi(&self) -> bool {
        match self {
            Stmt::Let(_, _, _, _) | Stmt::Semi(_) => true,
            Stmt::Expr(_) => false,
        }
    }
}

/// Represents enum varisnt
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub span: Span,
    pub name: String,
    pub fields: Vec<TypeHint>,
}

/// Import path (e.g `this/is/some/module`)
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ImportPath {
    pub span: Span,
    pub module: String,
}

/// Represents import kind
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportKind {
    /// Represents import of module as given name
    As(String),
    /// Represents import of module contents separated by comma
    For(Vec<String>),
    /// Just import of module
    Just,
}

/// Represents import declaration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub span: Span,
    pub path: ImportPath,
    pub kind: ImportKind,
}

/// Represents struct field
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub span: Span,
    pub name: String,
    pub hint: TypeHint,
}

/// Struct item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub span: Span,
    pub name: String,
    pub publicity: Publicity,
    pub generics: Vec<String>,
    pub fields: Vec<Field>,
}

/// Enum item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub span: Span,
    pub name: String,
    pub publicity: Publicity,
    pub generics: Vec<String>,
    pub variants: Vec<Variant>,
}

/// Function item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fn {
    pub span: Span,
    pub publicity: Publicity,
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<Param>,
    pub ret: Option<TypeHint>,
    pub block: Expr,
}

/// Extern function item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternFn {
    pub span: Span,
    pub name: String,
    pub publicity: Publicity,
    pub generics: Vec<String>,
    pub params: Vec<Param>,
    pub ret: Option<TypeHint>,
    pub body: String,
}

/// Constant item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Const {
    pub span: Span,
    pub publicity: Publicity,
    pub name: String,
    pub value: Expr,
    pub hint: TypeHint,
}

/// Item declaration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    Struct(Struct),
    Enum(Enum),
    Fn(Fn),
    ExternFn(ExternFn),
    Const(Const),
}

/// Module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub source: Arc<NamedSource<String>>,
    pub imports: Vec<Import>,
    pub items: Vec<Item>,
}
