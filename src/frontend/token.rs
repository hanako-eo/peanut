#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Sigils
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]
    OpenParen,    // (
    CloseParen,   // )
    Semicolon,    // ;
    Colon,        // :
    Comma,        // ,
    Dot,          // .
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Not,          // !
    NotEqual,     // !=
    Equal,        // =
    Equals,       // ==
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
    Amp,          // &
    And,          // &&
    Pipe,         // |
    Or,           // ||
    BigArrow,     // =>

    // Literals
    ID(String),
    String(String),
    Number(String),

    // Keywords
    Const,
    Let,
    If,
    Else,
    While,
    For,
    In,
    Func,
    Return,
    Yield,
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    pos: usize,
    len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, pos: usize, len: usize) -> Self {
        Self { kind, pos, len }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn len(&self) -> usize {
        self.len
    }
}
