#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    And,
    BineryAnd,
    Or,
    BineryOr,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterEquals,
    LessThan,
    LessEquals,
    Add,
    Subtract,
    Multiply,
    Divide,
    ModDiv,
    QuotDiv,
    In,
    Dot,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,

    None,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Identifier(String),
    NumberLiteral(f64),
    StringLiteral(String),

    Assign(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Closure(Vec<String>, Vec<Stmt>),
    Block(Vec<Stmt>),
    If(Vec<(Option<Expr>, Box<Expr>)>),

    UnaryOp(UnaryOp, Box<Expr>),
    Op(Op, Box<Expr>, Box<Expr>),

    None,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Program(Vec<Stmt>),
    Return(Expr),
    ExprStmt(Expr),
    FunctionDefinition {
        name: String,
        return_type: Option<String>,
        args: Vec<(String, String)>,
        body: Vec<Stmt>,
    },
    VarDeclaration {
        name: String,
        value: Expr,
        constant: bool,
    },
    While(Expr, Vec<Stmt>),
    For(String, Expr, Vec<Stmt>),

    None,
}
