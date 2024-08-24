use crate::errors::{Error, ErrorKind, Result};

use super::token::{Token, TokenKind};

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

impl TryFrom<Token> for Op {
    type Error = Error;

    fn try_from(value: Token) -> Result<Self> {
        match value.kind() {
            &TokenKind::AmpAmp => Ok(Op::And),
            &TokenKind::PipePipe => Ok(Op::Or),
            &TokenKind::Amp => Ok(Op::BineryAnd),
            &TokenKind::Pipe => Ok(Op::BineryOr),
            &TokenKind::Equals => Ok(Op::Equals),
            &TokenKind::BangEqual => Ok(Op::NotEquals),
            &TokenKind::Greater => Ok(Op::GreaterThan),
            &TokenKind::GreaterEqual => Ok(Op::GreaterEquals),
            &TokenKind::Less => Ok(Op::LessThan),
            &TokenKind::LessEqual => Ok(Op::LessEquals),
            &TokenKind::Plus => Ok(Op::Add),
            &TokenKind::Minus => Ok(Op::Subtract),
            &TokenKind::Star => Ok(Op::Multiply),
            &TokenKind::Slash => Ok(Op::Divide),
            &TokenKind::Percent => Ok(Op::ModDiv),
            &TokenKind::Colon => Ok(Op::QuotDiv),
            _ => Err(Error::from_kind(ErrorKind::UnsuspectedToken(value))),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Not,
    Negate,
    Positive,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Identifier(String),
    NumberLiteral(f64),
    StringLiteral(String),

    Assign(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Block(Vec<Stmt>),
    If(Vec<(Option<Expr>, Box<Expr>)>),
    While(Box<Expr>, Box<Expr>),

    Return(Box<Expr>),
    Yield(Box<Expr>),
    Break(Option<Box<Expr>>),
    Continue,

    UnaryOp(UnaryOp, Box<Expr>),
    Op(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Program(Vec<Stmt>),
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
    For(String, Expr, Vec<Stmt>),

    None,
}
