use crate::{
    frontend::token::{Token, TokenKind},
    runtime::environment::EnvOrigin,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind<'t> {
    /// LEXER
    InvalidChar((usize, usize), char),
    MissingCharacter((usize, usize), char),
    EscapeSequence((usize, usize)),
    Unexpected,

    /// PARSER
    ExpectedToken(TokenKind<'t>),
    UnexpectedToken {
        received: Token<'t>,
        expected: TokenKind<'t>,
    },
    UnsuspectedToken(Token<'t>),

    /// RUNTIME
    UnknownVariable(String),
    InvalidContext(EnvOrigin),
}

pub type Result<'e, T> = core::result::Result<T, Error<'e>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Error<'k> {
    kind: ErrorKind<'k>,
}

impl<'k> Error<'k> {
    pub fn from_kind(kind: ErrorKind<'k>) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &ErrorKind<'k> {
        &self.kind
    }
}
