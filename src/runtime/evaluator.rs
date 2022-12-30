use std::{cell::RefCell, rc::Rc};

use crate::{
    errors::Result,
    frontend::{
        ast::{Expr, Stmt},
        parser::Parser,
    },
};

use super::{
    environment::{self, Environment},
    value::{RuntimeValue, Value},
};

pub struct Evaluator;

impl Evaluator {
    pub fn evaluate(source: &str) -> Result<RuntimeValue> {
        let program = Parser::parse(source)?;
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Self;

        evaluator.evaluate_stmt(program, env)
    }
    fn evaluate_stmt(&mut self, stmt: Stmt, env: Rc<RefCell<Environment>>) -> Result<RuntimeValue> {
        match stmt {
            Stmt::Program(list) => {
                let mut value = Value::default_rt();
                for stmt in list {
                    value = self.evaluate_stmt(stmt, env.clone())?
                }
                Ok(value)
            }
            Stmt::VarDeclaration {
                name,
                value,
                constant,
            } => environment::declare_var(
                env.clone(),
                name,
                self.evaluate_expr(value, env.clone())?,
                constant,
            ),
            Stmt::ExprStmt(expr) => self.evaluate_expr(expr, env.clone()),
            _ => Ok(Value::default_rt()),
        }
    }

    fn evaluate_expr(&mut self, expr: Expr, env: Rc<RefCell<Environment>>) -> Result<RuntimeValue> {
        match expr {
            Expr::Identifier(varname) => environment::lookup_var(env, varname),
            Expr::NumberLiteral(n) => Ok(Rc::new(RefCell::new(Value::Number(n)))),
            _ => Ok(Value::default_rt()),
        }
    }
}
