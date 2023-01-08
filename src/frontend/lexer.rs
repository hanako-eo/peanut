use core::mem;
use core::str::CharIndices;
use std::collections::VecDeque;

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

    pub fn tokenize(source: &'i str) -> Result<VecDeque<Token>> {
        let lexer = Self::new(source);
        let mut tokens = VecDeque::new();
        for token in lexer {
            tokens.push_back(token?);
        }
        Ok(tokens)
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

    fn collect_number(&mut self, start: usize) -> Result<Token> {
        let end = self.skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()));

        // Check if it's a decimal or a field access after the . char
        if let (Some((_, '.')), Some((_, next_ch))) = (self.lookahead, self.lookahead2) {
            if next_ch.is_ascii_digit() {
                self.bump();
                return self
                    .skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()))
                    .and_then(|i| self.source.get(start..i))
                    .and_then(|str| Some((str.replace("_", ""), str.len())))
                    .map(|(number, len)| Token::new(TokenKind::Number(number), start, len))
                    .ok_or(Error::from_kind(ErrorKind::Unexpected));
            }
        }

        end.and_then(|i| self.source.get(start..i))
            .and_then(|str| Some((str.replace("_", ""), str.len())))
            .map(|(number, len)| Token::new(TokenKind::Number(number), start, len))
            .ok_or(Error::from_kind(ErrorKind::Unexpected))
    }

    fn collect_string(&mut self, quote: char, start: usize) -> Result<Token> {
        let (line, col) = (self.line, self.col - 1);

        self.collect_until(start, |ch, previous, _| {
            (ch == quote || ch == '\n') && matches!(previous, Some((_, ch)) if ch != '\\')
        })
        .and_then(|str| match self.bump() {
            Some((_, ch)) if ch == quote => Some(Token::new(
                TokenKind::String(
                    str[1..].replace(format!("\\{}", quote).as_str(), quote.to_string().as_str()),
                ),
                start,
                str.len() + 1,
            )),
            _ => None,
        })
        .ok_or(Error::from_kind(ErrorKind::MissingCharacter(
            (line, col),
            quote,
        )))
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        match self.bump() {
            Some((i, '{')) => Some(Ok(Token::new(TokenKind::OpenBrace, i, 1))),
            Some((i, '}')) => Some(Ok(Token::new(TokenKind::CloseBrace, i, 1))),
            Some((i, '(')) => Some(Ok(Token::new(TokenKind::OpenParen, i, 1))),
            Some((i, ')')) => Some(Ok(Token::new(TokenKind::CloseParen, i, 1))),
            Some((i, '[')) => Some(Ok(Token::new(TokenKind::OpenBracket, i, 1))),
            Some((i, ']')) => Some(Ok(Token::new(TokenKind::CloseBracket, i, 1))),
            Some((i, ':')) => Some(Ok(Token::new(TokenKind::Colon, i, 1))),
            Some((i, ';')) => Some(Ok(Token::new(TokenKind::Semicolon, i, 1))),
            Some((i, ',')) => Some(Ok(Token::new(TokenKind::Comma, i, 1))),
            Some((i, '.')) => Some(Ok(Token::new(TokenKind::Dot, i, 1))),
            Some((i, '+')) => Some(Ok(Token::new(TokenKind::Plus, i, 1))),
            Some((i, '-')) => Some(Ok(Token::new(TokenKind::Minus, i, 1))),
            Some((i, '*')) => Some(Ok(Token::new(TokenKind::Star, i, 1))),
            Some((i, '/')) => match self.lookahead {
                Some((_, '/')) => {
                    self.skip_to_endline();
                    self.next()
                }
                Some((_, '*')) => {
                    self.skip_to_endcomment();
                    self.next()
                }
                _ => Some(Ok(Token::new(TokenKind::Slash, i, 1))),
            },
            Some((i, '%')) => Some(Ok(Token::new(TokenKind::Percent, i, 1))),

            Some((i, '!')) => {
                if let Some((_, '=')) = self.lookahead {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::NotEqual, i, 2)))
                } else {
                    Some(Ok(Token::new(TokenKind::Not, i, 1)))
                }
            }

            Some((i, '=')) => match self.lookahead {
                Some((_, '=')) => {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::Equals, i, 2)))
                }
                Some((_, '>')) => {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::BigArrow, i, 2)))
                }
                _ => Some(Ok(Token::new(TokenKind::Equal, i, 1))),
            },

            Some((i, '>')) => {
                if let Some((_, '=')) = self.lookahead {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::GreaterEqual, i, 2)))
                } else {
                    Some(Ok(Token::new(TokenKind::Greater, i, 1)))
                }
            }

            Some((i, '<')) => {
                if let Some((_, '=')) = self.lookahead {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::LessEqual, i, 2)))
                } else {
                    Some(Ok(Token::new(TokenKind::Less, i, 1)))
                }
            }

            Some((i, '&')) => {
                if let Some((_, '&')) = self.lookahead {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::And, i, 2)))
                } else {
                    Some(Ok(Token::new(TokenKind::Amp, i, 1)))
                }
            }

            Some((i, '|')) => {
                if let Some((_, '|')) = self.lookahead {
                    self.bump();
                    Some(Ok(Token::new(TokenKind::Or, i, 2)))
                } else {
                    Some(Ok(Token::new(TokenKind::Pipe, i, 1)))
                }
            }

            Some((i, '"')) => Some(self.collect_string('"', i)),
            Some((i, '\'')) => Some(self.collect_string('\'', i)),

            Some((i, ch)) if ch == '_' || ch == '$' || ch.is_alphabetic() => Some(
                self.collect_until(i, |ch, _, _| {
                    !(ch == '_' || ch == '$' || ch.is_alphanumeric())
                })
                .and_then(|id| {
                    Some((
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

                            id => TokenKind::ID(id.into()),
                        },
                        id.len(),
                    ))
                })
                .and_then(|(kind, len)| Some(Token::new(kind, i, len)))
                .ok_or(Error::from_kind(ErrorKind::Unexpected)),
            ),

            Some((i, ch)) if ch.is_ascii_digit() => Some(self.collect_number(i)),
            Some((_, ch)) => Some(Err(Error::from_kind(ErrorKind::InvalidChar(
                (self.line, self.col - 1),
                ch,
            )))),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::errors::{Error, ErrorKind};
    use crate::frontend::token::{Token, TokenKind};

    use super::Lexer;

    #[test]
    fn keywords() {
        let tokens = Lexer::tokenize("func let const if else while for in return");

        assert_eq!(
            tokens,
            Ok(VecDeque::from([
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
        let tokens = Lexer::tokenize("a ab a1 _ _a _1 $ $a 識別子 しきべつし αναγνωριστικό");

        assert_eq!(
            tokens,
            Ok(VecDeque::from([
                Token::new(TokenKind::ID("a".into()), 0, 1),
                Token::new(TokenKind::ID("ab".into()), 2, 2),
                Token::new(TokenKind::ID("a1".into()), 5, 2),
                Token::new(TokenKind::ID("_".into()), 8, 1),
                Token::new(TokenKind::ID("_a".into()), 10, 2),
                Token::new(TokenKind::ID("_1".into()), 13, 2),
                Token::new(TokenKind::ID("$".into()), 16, 1),
                Token::new(TokenKind::ID("$a".into()), 18, 2),
                Token::new(TokenKind::ID("識別子".into()), 21, 9),
                Token::new(TokenKind::ID("しきべつし".into()), 31, 15),
                Token::new(TokenKind::ID("αναγνωριστικό".into()), 47, 26)
            ]))
        );
    }

    #[test]
    fn number() {
        let tokens = Lexer::tokenize("1 1_02 2.5");

        assert_eq!(
            tokens,
            Ok(VecDeque::from([
                Token::new(TokenKind::Number("1".into()), 0, 1),
                Token::new(TokenKind::Number("102".into()), 2, 4),
                Token::new(TokenKind::Number("2.5".into()), 7, 3)
            ]))
        );
    }

    #[test]
    fn comment() {
        let tokens = Lexer::tokenize("test /* hello :) */ test // hello 2\ntest");

        assert_eq!(
            tokens,
            Ok(VecDeque::from([
                Token::new(TokenKind::ID("test".into()), 0, 4),
                Token::new(TokenKind::ID("test".into()), 20, 4),
                Token::new(TokenKind::ID("test".into()), 36, 4)
            ]))
        );
    }

    #[test]
    fn string() {
        assert_eq!('"', '\"');
        let tokens = Lexer::tokenize("\"hello\" 'hi' \"yo \\\"\"");

        assert_eq!(
            tokens,
            Ok(VecDeque::from([
                Token::new(TokenKind::String("hello".into()), 0, 7),
                Token::new(TokenKind::String("hi".into()), 8, 4),
                Token::new(TokenKind::String("yo \"".into()), 13, 7),
            ]))
        );
    }

    #[test]
    fn errors() {
        let tokens = Lexer::tokenize("#");

        assert_eq!(
            tokens,
            Err(Error::from_kind(ErrorKind::InvalidChar((1, 1), '#')))
        );

        let tokens = Lexer::tokenize("1 #");

        assert_eq!(
            tokens,
            Err(Error::from_kind(ErrorKind::InvalidChar((1, 3), '#')))
        );

        let tokens = Lexer::tokenize("'aaa");

        assert_eq!(
            tokens,
            Err(Error::from_kind(ErrorKind::MissingCharacter((1, 1), '\'')))
        );
    }
}
