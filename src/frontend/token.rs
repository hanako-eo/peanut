#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind<'s> {
    // Symbols
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
    PlusPlus,     // ++
    PlusEqual,    // +=
    PPlusEqual,   // ++=
    Minus,        // -
    MinusEqual,   // -=
    Star,         // *
    StarEqual,    // *=
    Slash,        // /
    SlashEqual,   // /=
    Percent,      // %
    PercentEqual, // %=
    Bang,         // !
    BangEqual,    // !=
    Equal,        // =
    Equals,       // ==
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
    Amp,          // &
    AmpAmp,       // &&
    Pipe,         // |
    PipePipe,     // ||
    BigArrow,     // =>

    // Literals
    Ident(&'s str),
    RawString(&'s str),
    String(String),
    Number(&'s str),

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
pub struct Token<'k> {
    kind: TokenKind<'k>,
    pos: usize,
    len: usize,
}

impl<'k> Token<'k> {
    pub fn new(kind: TokenKind<'k>, pos: usize, len: usize) -> Self {
        Self { kind, pos, len }
    }

    pub fn kind(&self) -> &TokenKind<'k> {
        &self.kind
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn len(&self) -> usize {
        self.len
    }
}
