use core::mem;
use core::str::CharIndices;

use crate::errors::{Error, ErrorKind, Result};

use super::token::{Token, TokenKind};

type CharIndice = Option<(usize, char)>;

pub struct Lexer<'i> {
    source: &'i str,

    cursor: usize,
    line: usize,
    col: usize,

    chars: CharIndices<'i>,
    previous: CharIndice,
    lookahead: CharIndice,
    lookahead2: CharIndice,
}

impl<'i> Lexer<'i> {
    pub fn new(source: &'i str) -> Self {
        let mut chars = source.char_indices();
        let lookahead = chars.next();
        let lookahead2 = chars.next();

        Self {
            source,

            cursor: 0,
            line: 1,
            col: 1,

            chars,
            previous: None,
            lookahead,
            lookahead2,
        }
    }

    fn bump(&mut self) -> CharIndice {
        let curr = mem::replace::<CharIndice>(
            &mut self.lookahead,
            mem::replace(&mut self.lookahead2, self.chars.next()),
        );

        if let Some((_, ch)) = curr {
            self.previous = self.source.char_indices().nth(self.cursor);
            self.cursor += 1;
            self.col += 1;
            if ch == '\n' {
                self.col = 1;
                self.line += 1;
            }
        }

        curr
    }

    /// SKIP
    fn skip_until<F>(&mut self, mut predicate: F) -> Option<usize>
    where
        F: FnMut(char, CharIndice, CharIndice) -> bool,
    {
        while let Some((i, ch)) = self.lookahead {
            if predicate(ch, self.previous, self.lookahead2) {
                return Some(i);
            } else {
                self.bump();
            }
        }

        Some(self.source.len())
    }

    fn skip_n(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    fn skip_whitespace(&mut self) {
        self.skip_until(|ch, _, _| !ch.is_whitespace() && ch != '\n');
    }

    fn skip_to_endline(&mut self) {
        self.skip_until(|ch, _, _| ch == '\n');
    }

    fn skip_to_endcomment(&mut self) {
        self.skip_until(|ch, _, lookahead| match lookahead {
            Some((_, '/')) => ch == '*',
            _ => false,
        });
        self.skip_n(2);
    }

    /// COLLECT
    fn collect_until<F>(&mut self, start: usize, predicate: F) -> Option<&'i str>
    where
        F: FnMut(char, CharIndice, CharIndice) -> bool,
    {
        self.skip_until(predicate)
            .and_then(|i| self.source.get(start..i))
    }

    fn collect_number(&mut self, start: usize) -> Result<'i, Token<'i>> {
        let end = self.skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()));

        // Check if it's a decimal or a field access after the . char
        if let (Some((_, '.')), Some((_, next_ch))) = (self.lookahead, self.lookahead2) {
            if next_ch.is_ascii_digit() {
                self.bump();
                return self
                    .skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()))
                    .and_then(|i| self.source.get(start..i))
                    .map(|str| Token::new(TokenKind::Number(str), start, str.len()))
                    .ok_or(Error::from_kind(ErrorKind::Unexpected));
            }
        }

        end.and_then(|i| self.source.get(start..i))
            .map(|str| Token::new(TokenKind::Number(str), start, str.len()))
            .ok_or(Error::from_kind(ErrorKind::Unexpected))
    }

    // FIXME: collect and parse \n, \t and more
    fn collect_string(&mut self, raw: bool, quote: char, start: usize) -> Result<'i, Token<'i>> {
        let (line, col) = (self.line, self.col - 1);
        let Some((mut end, mut current_char)) = self.bump() else {
            unreachable!()
        };

        if raw {
            // we add 2 to remove r" to the result string
            self.collect_until(start + 2, |ch, _, _| (ch == quote || ch == '\n'))
                .and_then(|str| match self.bump() {
                    Some((_, ch)) if ch == quote => {
                        // we add 3 so as not to forget the r character and the two "
                        Some(Token::new(TokenKind::RawString(str), start, str.len() + 3))
                    }
                    _ => None,
                })
                .ok_or(Error::from_kind(ErrorKind::MissingCharacter(
                    (line, col),
                    quote,
                )))
        } else {
            let mut result_string = String::new();
            while current_char != quote {
                if current_char == '\\' {
                    let (_, to_escape) =
                        self.bump()
                            .ok_or(Error::from_kind(ErrorKind::EscapeSequence((
                                self.line,
                                self.col - 1,
                            ))))?;

                    match to_escape {
                        '0' => result_string.push('\0'),
                        'n' => result_string.push('\n'),
                        'r' => result_string.push('\r'),
                        't' => result_string.push('\t'),
                        '\n' => {}
                        c => result_string.push(c),
                    }
                } else {
                    result_string.push(current_char);
                }

                (end, current_char) =
                    self.bump()
                        .ok_or(Error::from_kind(ErrorKind::MissingCharacter(
                            (line, col),
                            quote,
                        )))?;
            }

            Ok(Token::new(
                TokenKind::String(result_string),
                start,
                end - start + 1,
            ))
        }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Result<'i, Token<'i>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        enum Started<'t> {
            Slash,
            Equal,
            Plus,
            RepeatElse(TokenKind<'t>, TokenKind<'t>),
            IfEqualElse(TokenKind<'t>, TokenKind<'t>),
        }

        let (position, char) = self.bump()?;
        let started = match char {
            'r' if matches!(self.lookahead, Some((_, '"')) | Some((_, '\''))) => {
                let Some((_, char)) = self.bump() else {
                    unreachable!()
                };
                return Some(self.collect_string(true, char, position));
            }
            '"' | '\'' => return Some(self.collect_string(false, char, position)),

            '{' => return Some(Ok(Token::new(TokenKind::OpenBrace, position, 1))),
            '}' => return Some(Ok(Token::new(TokenKind::CloseBrace, position, 1))),
            '(' => return Some(Ok(Token::new(TokenKind::OpenParen, position, 1))),
            ')' => return Some(Ok(Token::new(TokenKind::CloseParen, position, 1))),
            '[' => return Some(Ok(Token::new(TokenKind::OpenBracket, position, 1))),
            ']' => return Some(Ok(Token::new(TokenKind::CloseBracket, position, 1))),
            ':' => return Some(Ok(Token::new(TokenKind::Colon, position, 1))),
            ';' => return Some(Ok(Token::new(TokenKind::Semicolon, position, 1))),
            ',' => return Some(Ok(Token::new(TokenKind::Comma, position, 1))),
            '.' => return Some(Ok(Token::new(TokenKind::Dot, position, 1))),
            '+' => Started::Plus,
            '/' => Started::Slash,
            '=' => Started::Equal,
            '-' => Started::IfEqualElse(TokenKind::MinusEqual, TokenKind::Minus),
            '*' => Started::IfEqualElse(TokenKind::StarEqual, TokenKind::Star),
            '%' => Started::IfEqualElse(TokenKind::PercentEqual, TokenKind::Percent),
            '!' => Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
            '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
            '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
            '&' => Started::RepeatElse(TokenKind::AmpAmp, TokenKind::Amp),
            '|' => Started::RepeatElse(TokenKind::PipePipe, TokenKind::Pipe),

            ch if ch == '_' || ch == '$' || ch.is_alphabetic() => {
                return Some(
                    self.collect_until(position, |ch, _, _| {
                        !(ch == '_' || ch == '$' || ch.is_alphanumeric())
                    })
                    .map(|id| {
                        Token::new(
                            match id {
                                "func" => TokenKind::Func,
                                "let" => TokenKind::Let,
                                "const" => TokenKind::Const,
                                "if" => TokenKind::If,
                                "else" => TokenKind::Else,
                                "while" => TokenKind::While,
                                "for" => TokenKind::For,
                                "in" => TokenKind::In,
                                "return" => TokenKind::Return,
                                "yield" => TokenKind::Yield,
                                "break" => TokenKind::Break,
                                "continue" => TokenKind::Continue,

                                id => TokenKind::Ident(id),
                            },
                            position,
                            id.len(),
                        )
                    })
                    .ok_or(Error::from_kind(ErrorKind::Unexpected)),
                )
            }

            ch if ch.is_ascii_digit() => return Some(self.collect_number(position)),
            ch => {
                return Some(Err(Error::from_kind(ErrorKind::InvalidChar(
                    (self.line, self.col - 1),
                    ch,
                ))))
            }
        };

        match started {
            Started::Plus => match self.lookahead {
                Some((_, '=')) => {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::PlusEqual, position, 2)))
                }
                Some((_, '+')) => {
                    self.bump();
                    match self.lookahead {
                        Some((_, '=')) => {
                            self.bump();
                            Some(Ok(Token::new(TokenKind::PPlusEqual, position, 3)))
                        }
                        _ => Some(Ok(Token::new(TokenKind::PlusPlus, position, 2))),
                    }
                }
                _ => Some(Ok(Token::new(TokenKind::Plus, position, 1))),
            },
            Started::Slash => match self.lookahead {
                Some((_, '/')) => {
                    self.skip_to_endline();
                    self.next()
                }
                Some((_, '*')) => {
                    self.skip_to_endcomment();
                    self.next()
                }
                _ => Some(Ok(Token::new(TokenKind::Slash, position, 1))),
            },
            Started::Equal => match self.lookahead {
                Some((_, '=')) => {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::Equals, position, 2)))
                }
                Some((_, '>')) => {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::BigArrow, position, 2)))
                }
                _ => Some(Ok(Token::new(TokenKind::Equal, position, 1))),
            },
            Started::IfEqualElse(yes, no) => match self.lookahead {
                Some((_, '=')) => {
                    self.bump();
                    Some(Ok(Token::new(yes, position, 2)))
                }
                _ => Some(Ok(Token::new(no, position, 1))),
            },
            Started::RepeatElse(yes, no) => match self.lookahead {
                Some((_, lookahead)) if lookahead == char => {
                    self.bump();
                    Some(Ok(Token::new(yes, position, 2)))
                }
                _ => Some(Ok(Token::new(no, position, 1))),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::{Error, ErrorKind, Result};
    use crate::frontend::token::{Token, TokenKind};

    use super::Lexer;

    #[test]
    fn keywords() {
        let tokens = Lexer::new("func let const if else while for in return")
            .collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Ok(Vec::from([
                Token::new(TokenKind::Func, 0, 4),
                Token::new(TokenKind::Let, 5, 3),
                Token::new(TokenKind::Const, 9, 5),
                Token::new(TokenKind::If, 15, 2),
                Token::new(TokenKind::Else, 18, 4),
                Token::new(TokenKind::While, 23, 5),
                Token::new(TokenKind::For, 29, 3),
                Token::new(TokenKind::In, 33, 2),
                Token::new(TokenKind::Return, 36, 6)
            ]))
        );
    }

    #[test]
    fn identifier() {
        let tokens = Lexer::new("a ab a1 _ _a _1 $ $a 識別子 しきべつし αναγνωριστικό")
            .collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Ok(Vec::from([
                Token::new(TokenKind::Ident("a".into()), 0, 1),
                Token::new(TokenKind::Ident("ab".into()), 2, 2),
                Token::new(TokenKind::Ident("a1".into()), 5, 2),
                Token::new(TokenKind::Ident("_".into()), 8, 1),
                Token::new(TokenKind::Ident("_a".into()), 10, 2),
                Token::new(TokenKind::Ident("_1".into()), 13, 2),
                Token::new(TokenKind::Ident("$".into()), 16, 1),
                Token::new(TokenKind::Ident("$a".into()), 18, 2),
                Token::new(TokenKind::Ident("識別子".into()), 21, 9),
                Token::new(TokenKind::Ident("しきべつし".into()), 31, 15),
                Token::new(TokenKind::Ident("αναγνωριστικό".into()), 47, 26)
            ]))
        );
    }

    #[test]
    fn number() {
        let tokens = Lexer::new("1 1_02 2.5").collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Ok(Vec::from([
                Token::new(TokenKind::Number("1"), 0, 1),
                Token::new(TokenKind::Number("1_02"), 2, 4),
                Token::new(TokenKind::Number("2.5"), 7, 3)
            ]))
        );
    }

    #[test]
    fn comment() {
        let tokens = Lexer::new("test /* hello :) */ test // hello 2\ntest")
            .collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Ok(Vec::from([
                Token::new(TokenKind::Ident("test".into()), 0, 4),
                Token::new(TokenKind::Ident("test".into()), 20, 4),
                Token::new(TokenKind::Ident("test".into()), 36, 4)
            ]))
        );
    }

    #[test]
    fn string() {
        assert_eq!('"', '\"');
        let tokens = Lexer::new("\"hello\" 'hi' \"yo \\\"\" \"he\\nllo\" r\"he\\nllo\"")
            .collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Ok(Vec::from([
                Token::new(TokenKind::String("hello".into()), 0, 7),
                Token::new(TokenKind::String("hi".into()), 8, 4),
                Token::new(TokenKind::String("yo \"".into()), 13, 7),
                Token::new(TokenKind::String("he\nllo".into()), 21, 9),
                Token::new(TokenKind::RawString("he\\nllo"), 31, 10),
            ]))
        );
    }

    #[test]
    fn errors() {
        let tokens = Lexer::new("#").collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Err(Error::from_kind(ErrorKind::InvalidChar((1, 1), '#')))
        );

        let tokens = Lexer::new("1 #").collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Err(Error::from_kind(ErrorKind::InvalidChar((1, 3), '#')))
        );

        let tokens = Lexer::new("'aaa").collect::<Result<Vec<Token<'_>>>>();

        assert_eq!(
            tokens,
            Err(Error::from_kind(ErrorKind::MissingCharacter((1, 1), '\'')))
        );
    }
}
