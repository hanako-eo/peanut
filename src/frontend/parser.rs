use crate::{
    errors::{Error, ErrorKind, Result},
    parse_op_expr,
};

use core::mem;
use std::collections::VecDeque;

use super::{
    ast::{Expr, Stmt, UnaryOp},
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

    fn check_index(&self, index: usize, token_kind: TokenKind) -> bool {
        match self.at(index) {
            Some(token) if mem::discriminant(token.kind()) == mem::discriminant(&token_kind) => {
                true
            }
            _ => false,
        }
    }

    fn check(&self, token_kind: TokenKind) -> bool {
        self.check_index(0, token_kind)
    }

    fn at(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }

    fn eat(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn expect(&mut self, token_kind: TokenKind) -> Result<Token> {
        match self.at(0) {
            Some(token) if mem::discriminant(token.kind()) == mem::discriminant(&token_kind) => {
                Ok(self.eat().unwrap())
            }
            Some(token) => self.generate_unexpected(token, token_kind),
            None => self.generate_expected(token_kind),
        }
    }

    fn expect_ident(&mut self) -> Result<Token> {
        match self.at(0) {
            Some(token) if matches!(token.kind(), TokenKind::ID(_)) => Ok(self.eat().unwrap()),
            Some(token) => self.generate_unexpected(token, TokenKind::ID(String::new())),
            None => self.generate_expected(TokenKind::ID(String::new())),
        }
    }

    fn get_ident_value(&mut self) -> Result<String> {
        let ident = self.expect_ident()?;
        match ident.kind() {
            TokenKind::ID(val) => Ok(val.clone()),
            _ => self.generate_unsuspected(ident),
        }
    }

    /// COLLECT
    fn parse_program(&mut self) -> Result<Stmt> {
        let mut statements = Vec::new();
        while let Some(_) = self.at(0) {
            statements.push(self.parse_statement()?);
            if self.check(TokenKind::Semicolon) {
                self.eat();
            }
        }
        Ok(Stmt::Program(statements))
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        let Some(current) = self.at(0) else {
            return Ok(Stmt::None)
        };

        // TODO: other statement like function
        match current.kind() {
            TokenKind::Let | TokenKind::Const => self.parse_var_declaration(),
            TokenKind::Func => self.parse_function(),
            TokenKind::Return => {
                self.eat();
                Ok(Stmt::ExprStmt(Expr::Return(Box::new(self.parse_expr()?))))
            }
            TokenKind::Yield => {
                self.eat();
                Ok(Stmt::ExprStmt(Expr::Yield(Box::new(self.parse_expr()?))))
            }
            _ => Ok(Stmt::ExprStmt(self.parse_expr()?)),
        }
    }

    fn parse_var_declaration(&mut self) -> Result<Stmt> {
        // variable declaration -> let|const foo = 0
        let is_constant = self.eat().unwrap().kind() == &TokenKind::Const;
        let name = self.get_ident_value()?;

        self.expect(TokenKind::Equal)?;

        let expr = self.parse_expr()?;

        Ok(Stmt::VarDeclaration {
            name,
            value: expr,
            constant: is_constant,
        })
    }

    fn parse_function(&mut self) -> Result<Stmt> {
        // function declaration -> func foo() return_type { ... }
        //                       | func foo(arg1 type) { ... }
        //                       | func foo() => expr

        self.eat();
        let name = self.get_ident_value()?;
        let args = self.parse_args_definition()?;

        let return_type = self.get_ident_value().ok();
        let is_sort_func = self.expect(TokenKind::BigArrow).is_ok();

        let mut body = Vec::new();
        if is_sort_func {
            body.push(Stmt::ExprStmt(Expr::Return(Box::new(self.parse_expr()?))));
        } else {
            self.expect(TokenKind::OpenBrace)?;
            loop {
                if self.check(TokenKind::CloseBrace) {
                    break;
                }

                body.push(self.parse_statement()?);

                if self.check(TokenKind::Semicolon) {
                    self.eat();
                }
            }
            self.expect(TokenKind::CloseBrace)?;
        }

        Ok(Stmt::FunctionDefinition {
            name,
            args,
            return_type,
            body,
        })
    }

    fn parse_tuple_of<T, F>(&mut self, callback: F) -> Result<Vec<T>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let mut args = Vec::new();
        let mut first = true;
        self.expect(TokenKind::OpenParen)?;
        loop {
            if self.check(TokenKind::CloseParen) {
                break;
            }
            if !mem::replace(&mut first, false) {
                self.expect(TokenKind::Comma)?;
            }

            args.push(callback(self)?);

            if !self.check(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::CloseParen)?;
        Ok(args)
    }

    fn parse_args_definition(&mut self) -> Result<Vec<(String, String)>> {
        self.parse_tuple_of(|parser| {
            let arg = parser.get_ident_value()?;
            let r#type = parser.get_ident_value()?;
            Ok((arg, r#type))
        })
    }

    fn parse_if_expr(&mut self) -> Result<Expr> {
        let mut conditions = Vec::new();
        let mut first = true;

        loop {
            match (first, self.check(TokenKind::Else)) {
                (true, _) => (),
                (false, false) => break,
                (false, true) => {
                    self.eat();
                }
            };

            let condition = match (first, self.check(TokenKind::If)) {
                (true, _) => Some(self.parse_expr()?),
                (false, false) => None,
                (false, true) => {
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

    fn parse_while_expr(&mut self) -> Result<Expr> {
        let condition = self.parse_expr()?;
        let body = self.parse_expr()?;
        Ok(Expr::While(Box::new(condition), Box::new(body)))
    }

    /// Expression parsing orders of precedence
    /// ✅ Assignment
    /// ❌ Member
    /// ✅ Function call
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
            TokenKind::And,
            TokenKind::Or,
            TokenKind::Amp,
            TokenKind::Pipe,
        }

        fn parse_comparison_expr from parse_additive_expr {
            TokenKind::Equals,
            TokenKind::NotEqual,
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        }

        fn parse_additive_expr from parse_multiplicative_expr {
            TokenKind::Plus,
            TokenKind::Minus,
        }

        fn parse_multiplicative_expr from parse_unary_expr {
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Percent,
            TokenKind::Colon,
        }
    );

    fn parse_assignment(&mut self) -> Result<Expr> {
        let left = self.parse_call_member_expr()?;

        Ok(if self.check(TokenKind::Equal) {
            self.eat();
            let value = self.parse_call_member_expr()?;
            Expr::Assign(Box::new(left), Box::new(value))
        } else if self.check_index(1, TokenKind::Equal) {
            let left = Box::new(left);
            let op = self.eat().unwrap().try_into()?;
            self.eat();
            let value = self.parse_call_member_expr()?;

            Expr::Assign(
                left.clone(),
                Box::new(Expr::Op(op, left.clone(), Box::new(value))),
            )
        } else {
            left
        })
    }

    fn parse_call_member_expr(&mut self) -> Result<Expr> {
        let member = self.parse_primary_expr()?;

        if self.check(TokenKind::OpenParen) {
            self.parse_call_expr(member)
        } else {
            Ok(member)
        }
    }

    fn parse_call_expr(&mut self, caller: Expr) -> Result<Expr> {
        let expr = Expr::Call(
            Box::new(caller),
            self.parse_tuple_of(|parser| parser.parse_expr())?,
        );

        if self.check(TokenKind::OpenParen) {
            self.parse_call_expr(expr)
        } else {
            Ok(expr)
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        let unary = match self.at(0).unwrap().kind() {
            &TokenKind::Plus => Some(UnaryOp::Positive),
            &TokenKind::Minus => Some(UnaryOp::Negate),
            &TokenKind::Not => Some(UnaryOp::Not),
            _ => None,
        };

        match unary {
            None => self.parse_assignment(),
            Some(unary) => {
                self.eat();
                Ok(Expr::UnaryOp(unary, Box::new(self.parse_unary_expr()?)))
            }
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        let token = self.eat().unwrap();
        Ok(match token.kind() {
            TokenKind::ID(symbol) => Expr::Identifier(symbol.clone()),
            TokenKind::Number(num) => Expr::NumberLiteral(num.parse().unwrap()),
            TokenKind::String(str) => Expr::StringLiteral(str.clone()),
            TokenKind::OpenParen => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::CloseParen)?;
                expr
            }
            TokenKind::If => self.parse_if_expr()?,
            TokenKind::While => self.parse_while_expr()?,
            TokenKind::Break => Expr::Break(None),
            TokenKind::Continue => Expr::Continue,
            TokenKind::OpenBrace => {
                let mut body = Vec::new();
                loop {
                    if self.check(TokenKind::CloseBrace) {
                        break;
                    }

                    body.push(self.parse_statement()?);

                    if self.check(TokenKind::Semicolon) {
                        self.eat();
                    }
                }
                self.expect(TokenKind::CloseBrace)?;
                Expr::Block(body)
            }
            _ => return self.generate_unsuspected(token),
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

    fn generate_unsuspected<T>(&self, unsuspected: Token) -> Result<T> {
        Err(Error::from_kind(ErrorKind::UnsuspectedToken(unsuspected)))
    }
}

mod tests {
    use crate::{
        errors::{Error, ErrorKind},
        frontend::ast::{Expr, Op, Stmt, UnaryOp},
    };

    use super::Parser;

    #[test]
    fn var_declaration() {
        let ast = Parser::parse("let a = 0\nconst a = 0");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![
                Stmt::VarDeclaration {
                    name: "a".into(),
                    value: Expr::NumberLiteral(0.),
                    constant: false
                },
                Stmt::VarDeclaration {
                    name: "a".into(),
                    value: Expr::NumberLiteral(0.),
                    constant: true
                }
            ]))
        );
    }

    #[test]
    fn arithmetic_expression() {
        let ast = Parser::parse("(-a + 2 * 4) / 2 + (b : 2) - (c % 2)");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Op(
                Op::Subtract,
                Box::new(Expr::Op(
                    Op::Add,
                    Box::new(Expr::Op(
                        Op::Divide,
                        Box::new(Expr::Op(
                            Op::Add,
                            Box::new(Expr::UnaryOp(
                                UnaryOp::Negate,
                                Box::new(Expr::Identifier("a".into()))
                            )),
                            Box::new(Expr::Op(
                                Op::Multiply,
                                Box::new(Expr::NumberLiteral(2.0)),
                                Box::new(Expr::NumberLiteral(4.0))
                            ))
                        )),
                        Box::new(Expr::NumberLiteral(2.0))
                    )),
                    Box::new(Expr::Op(
                        Op::QuotDiv,
                        Box::new(Expr::Identifier("b".into())),
                        Box::new(Expr::NumberLiteral(2.0))
                    ))
                )),
                Box::new(Expr::Op(
                    Op::ModDiv,
                    Box::new(Expr::Identifier("c".into())),
                    Box::new(Expr::NumberLiteral(2.0))
                ))
            ))]))
        );
    }

    #[test]
    fn function_definition() {
        let ast = Parser::parse("func test() {}");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::FunctionDefinition {
                name: "test".into(),
                return_type: None,
                args: Vec::new(),
                body: Vec::new()
            }]))
        );

        let ast = Parser::parse("func test() {0}");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::FunctionDefinition {
                name: "test".into(),
                return_type: None,
                args: Vec::new(),
                body: vec![Stmt::ExprStmt(Expr::NumberLiteral(0.))]
            }]))
        );

        let ast = Parser::parse("func test(a string) {}");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::FunctionDefinition {
                name: "test".into(),
                return_type: None,
                args: vec![("a".into(), "string".into())],
                body: Vec::new(),
            }]))
        );
    }

    #[test]
    fn while_loop() {
        let ast = Parser::parse("while 2 1");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::While(
                Box::new(Expr::NumberLiteral(2.)),
                Box::new(Expr::NumberLiteral(1.))
            ))]))
        );
    }

    #[test]
    fn condition() {
        let ast = Parser::parse("if 2 1");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::If(vec![(
                Some(Expr::NumberLiteral(2.)),
                Box::new(Expr::NumberLiteral(1.))
            )]))]))
        );

        let ast = Parser::parse("if 2 1 else if 4 3");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::If(vec![
                (
                    Some(Expr::NumberLiteral(2.)),
                    Box::new(Expr::NumberLiteral(1.))
                ),
                (
                    Some(Expr::NumberLiteral(4.)),
                    Box::new(Expr::NumberLiteral(3.))
                )
            ]))]))
        );

        let ast = Parser::parse("if 2 1 else 3");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::If(vec![
                (
                    Some(Expr::NumberLiteral(2.)),
                    Box::new(Expr::NumberLiteral(1.))
                ),
                (None, Box::new(Expr::NumberLiteral(3.)))
            ]))]))
        );
    }

    #[test]
    fn assignment() {
        let ast = Parser::parse("a = 1");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Assign(
                Box::new(Expr::Identifier("a".into())),
                Box::new(Expr::NumberLiteral(1.)),
            ))]))
        );

        let ast = Parser::parse("a += 1");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Assign(
                Box::new(Expr::Identifier("a".into())),
                Box::new(Expr::Op(
                    Op::Add,
                    Box::new(Expr::Identifier("a".into())),
                    Box::new(Expr::NumberLiteral(1.))
                )),
            ))]))
        );
    }

    #[test]
    fn call() {
        let ast = Parser::parse("a()");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Call(
                Box::new(Expr::Identifier("a".into())),
                Vec::new(),
            ))]))
        );

        let ast = Parser::parse("a(1)");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Call(
                Box::new(Expr::Identifier("a".into())),
                vec![Expr::NumberLiteral(1.)],
            ))]))
        );

        let ast = Parser::parse("a()(1)");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Call(
                Box::new(Expr::Call(Box::new(Expr::Identifier("a".into())), vec![],)),
                vec![Expr::NumberLiteral(1.)],
            ))]))
        );
    }

    #[test]
    fn arithmetic_call() {
        let ast = Parser::parse("a(n-1) + a(n-2)");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::Op(
                Op::Add,
                Box::new(Expr::Call(
                    Box::new(Expr::Identifier("a".into())),
                    vec![Expr::Op(
                        Op::Subtract,
                        Box::new(Expr::Identifier("n".into())),
                        Box::new(Expr::NumberLiteral(1.0))
                    )]
                )),
                Box::new(Expr::Call(
                    Box::new(Expr::Identifier("a".into())),
                    vec![Expr::Op(
                        Op::Subtract,
                        Box::new(Expr::Identifier("n".into())),
                        Box::new(Expr::NumberLiteral(2.0))
                    )]
                ))
            ))]))
        );
    }

    #[test]
    fn unary_call() {
        let ast = Parser::parse("-a(1)");

        assert_eq!(
            ast,
            Ok(Stmt::Program(vec![Stmt::ExprStmt(Expr::UnaryOp(
                UnaryOp::Negate,
                Box::new(Expr::Call(
                    Box::new(Expr::Identifier("a".into())),
                    vec![Expr::NumberLiteral(1.0)]
                )),
            ))]))
        );
    }
}
