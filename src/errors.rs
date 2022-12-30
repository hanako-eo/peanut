use crate::frontend::token::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    /// LEXER
    InvalidChar((usize, usize), char),
    MissingCharacter((usize, usize), char),
    Unexpected,

    /// PARSER
    ExpectedToken(TokenKind),
    UnexpectedToken {
        received: Token,
        expected: TokenKind,
    },
    UnsuspectedToken(Token),

    /// RUNTIME
    UnknownVariable(String),
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn from_kind(kind: ErrorKind) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}
