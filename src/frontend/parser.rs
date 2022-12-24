use crate::{
    errors::{Error, ErrorKind, Result},
    parse_op_expr,
};

use core::mem;
use std::collections::VecDeque;

use super::{
    ast::{Expr, Op, Stmt, UnaryOp},
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn parse(source: &str) -> Result<Stmt> {
        let tokens = Lexer::tokenize(source)?;
        let mut parser = Self { tokens };

        parser.parse_program()
    }

    fn check(&self, token_kind: TokenKind) -> Result<()> {
        match self.current_token() {
            Some(token) => {
                if mem::discriminant(token.kind()) == mem::discriminant(&token_kind) {
                    Ok(())
                } else {
                    self.generate_unexpected(token, token_kind)
                }
            }
            None => self.generate_expected(token_kind),
        }
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(0)
    }

    fn eat(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn expect(&mut self, token_kind: TokenKind) -> Result<Token> {
        self.check(token_kind).map(|_| self.eat().unwrap())
    }

    /// COLLECT
    fn parse_program(&mut self) -> Result<Stmt> {
        let mut statements = Vec::new();
        while let Some(_) = self.current_token() {
            statements.push(self.parse_statement()?);
            self.expect(TokenKind::Semicolon)?;
        }
        Ok(Stmt::Program(statements))
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        let Some(current) = self.current_token() else {
            return Ok(Stmt::None)
        };

        // TODO: other statement like function
        match current.kind() {
            TokenKind::Let | TokenKind::Const => self.parse_var_declaration(),
            _ => Ok(Stmt::ExprStmt(self.parse_expr()?)),
        }
    }

    fn parse_var_declaration(&mut self) -> Result<Stmt> {
        // variable declaration -> let|const foo = 0
        let is_constant = self.eat().unwrap().kind() == &TokenKind::Const;
        let token = self.expect(TokenKind::ID(String::new()))?;
        let TokenKind::ID(name) = token.kind() else { panic!() };

        self.expect(TokenKind::Equal)?;

        let expr = self.parse_expr()?;

        Ok(Stmt::VarDeclaration {
            name: name.clone(),
            value: expr,
            constant: is_constant,
        })
    }

    fn parse_if_expr(&mut self) -> Result<Expr> {
        let mut conditions = Vec::new();
        let mut first = true;

        loop {
            match (first, self.check(TokenKind::Else)) {
                (true, _) => (),
                (false, Err(_)) => break,
                (false, Ok(())) => {
                    self.eat();
                }
            };

            let condition = match (first, self.check(TokenKind::If)) {
                (true, _) => Some(self.parse_expr()?),
                (false, Err(_)) => None,
                (false, Ok(())) => {
                    self.eat();
                    Some(self.parse_expr()?)
                }
            };

            let body = Box::new(self.parse_expr()?);
            match condition {
                Some(condition) => conditions.push((Some(condition), body)),
                None => {
                    conditions.push((None, body));
                    break;
                }
            }
            first = false;
        }

        Ok(Expr::If(conditions))
    }

    /// Expression parsing orders of precedence
    /// ❌ Assignment
    /// ❌ Member
    /// ❌ Function call
    /// ✅ Logical operator
    /// ✅ Comparison
    /// ✅ Additive
    /// ✅ Multiplication
    /// ✅ Unary
    /// ✅ Primary
    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_logical_expr()
    }

    parse_op_expr!(
        fn parse_logical_expr from parse_comparison_expr {
            TokenKind::And => Op::And,
            TokenKind::Or => Op::Or,
            TokenKind::Amp => Op::BineryAnd,
            TokenKind::Pipe => Op::BineryOr,
        }

        fn parse_comparison_expr from parse_additive_expr {
            TokenKind::Equals => Op::Equals,
            TokenKind::NotEqual => Op::NotEquals,
            TokenKind::Greater => Op::GreaterThan,
            TokenKind::GreaterEqual => Op::GreaterEquals,
            TokenKind::Less => Op::LessThan,
            TokenKind::LessEqual => Op::LessEquals,
        }

        fn parse_additive_expr from parse_multiplicative_expr {
            TokenKind::Plus => Op::Add,
            TokenKind::Minus => Op::Subtract,
        }

        fn parse_multiplicative_expr from parse_unary_expr {
            TokenKind::Star => Op::Multiply,
            TokenKind::Slash => Op::Divide,
            TokenKind::Percent => Op::ModDiv,
            TokenKind::Colon => Op::QuotDiv,
        }
    );

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        let unary = match self.current_token().unwrap().kind() {
            &TokenKind::Plus => UnaryOp::Positive,
            &TokenKind::Minus => UnaryOp::Negate,
            &TokenKind::Not => UnaryOp::Not,
            _ => UnaryOp::None,
        };

        match unary {
            UnaryOp::None => self.parse_primary_expr(),
            _ => {
                self.eat();
                Ok(Expr::UnaryOp(unary, Box::new(self.parse_unary_expr()?)))
            }
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        Ok(match self.eat().unwrap().kind() {
            TokenKind::ID(symbol) => Expr::Identifier(symbol.clone()),
            TokenKind::Number(num) => Expr::NumberLiteral(num.parse().unwrap()),
            TokenKind::String(str) => Expr::StringLiteral(str.clone()),
            TokenKind::OpenParen => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::CloseParen)?;
                expr
            }
            TokenKind::If => self.parse_if_expr()?,
            _ => Expr::None,
        })
    }

    /// ERRORS
    fn generate_unexpected<T>(&self, token: &Token, expected: TokenKind) -> Result<T> {
        Err(Error::from_kind(ErrorKind::UnexpectedToken {
            received: token.clone(),
            expected,
        }))
    }

    fn generate_expected<T>(&self, expected: TokenKind) -> Result<T> {
        Err(Error::from_kind(ErrorKind::ExpectedToken(expected)))
    }
}
