#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    /// LEXER
    InvalidChar((usize, usize), char),
    MissingCharacter((usize, usize), char),
    Unexpected,
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
