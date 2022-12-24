use crate::errors::{Error, ErrorKind, Result};

use core::mem;
use std::collections::VecDeque;

use super::{
    ast::{Expr, Op, Stmt, UnaryOp},
    lexer::Lexer,
    token::{Token, TokenKind},
};

macro_rules! parse_op_expr {
    ($(fn $name:ident from $calle:ident {$($token:path => $op:expr,)* })*) => {
        $(fn $name(&mut self) -> Result<Expr> {
            let mut left = self.$calle()?;

            while $(self.check($token).is_ok() ||)* true {
                let op = match self.eat().unwrap().kind() {
                    $(&$token => $op,)*
                    _ => panic!(),
                };
                let right = self.$calle()?;
                left = Expr::Op(op, Box::new(left), Box::new(right))
            }

            Ok(left)
        })*
    };
}

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
            self.eat();
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

    // fn parse_comparison_expr(&mut self) -> Result<Expr> {}

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
            _ => Ok(Expr::UnaryOp(unary, Box::new(self.parse_unary_expr()?))),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        let token = self.current_token().unwrap();
        Ok(match token.kind() {
            TokenKind::ID(symbol) => Expr::Identifier(symbol.clone()),
            TokenKind::Number(num) => Expr::NumberLiteral(num.parse().unwrap()),
            TokenKind::String(str) => Expr::StringLiteral(str.clone()),
            TokenKind::OpenParen => {
                self.eat();
                let expr = self.parse_expr()?;
                self.expect(TokenKind::CloseParen)?;
                expr
            }
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
