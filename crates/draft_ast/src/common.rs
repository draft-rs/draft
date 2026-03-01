/// Represents item publicity
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Publicity {
    Pub,
    Priv,
}

/// Binary operator
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,    // `+`
    Sub,    // `-`
    Mul,    // `*`
    Div,    // `/`
    Mod,    // `%`
    Eq,     // `==`
    Ne,     // `!=`
    Gt,     // `>`
    Ge,     // `>=`
    Lt,     // `<`
    Le,     // `<=`
    And,    // `&&`
    Or,     // `||`
    Xor,    // `^`
    BitAnd, // `&`
    BitOr,  // `|`
    Concat, // `<>`
}

/// Assignment operation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignOp {
    AddEq, // +=
    SubEq, // -=
    MulEq, // *=
    DivEq, // /=
    ModEq, // %=
    AndEq, // &=
    OrEq,  // |=
    XorEq, // ^=
    Eq,    // =
}

/// Unary operator
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,  // -
    Bang, // !
}

/// Represents literal
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    /// Integer number literal
    Int(String),

    /// Floating-point number literal
    Float(String),

    /// String literal
    String(String),

    /// Boolean literal
    Bool(bool),
}