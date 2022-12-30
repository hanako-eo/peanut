use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    errors::Result,
    frontend::{
        ast::{Expr, Op, Stmt},
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
            Expr::StringLiteral(str) => Ok(Rc::new(RefCell::new(Value::String(str)))),
            Expr::Assign(id, expr) => {
                let varname = match *id {
                    Expr::Identifier(symbol) => symbol,
                    _ => panic!(),
                };

                environment::assign_var(env.clone(), varname, self.evaluate_expr(*expr, env)?)
            }
            Expr::Op(op, lhs, rhs) => {
                // TODO: evaluate conditionals operators
                let lhs = self.evaluate_expr(*lhs, env.clone())?;
                let rhs = self.evaluate_expr(*rhs, env.clone())?;

                let left = Ref::leak(lhs.borrow());
                let right = Ref::leak(rhs.borrow());

                Ok(Rc::new(RefCell::new(match op {
                    Op::Add => left.add(right),
                    Op::Subtract => left.sub(right),
                    Op::Multiply => left.mul(right),
                    Op::Divide => left.try_div(right)?,
                    Op::ModDiv => left.try_mod_div(right)?,
                    Op::QuotDiv => left.try_quot_div(right)?,
                    _ => unimplemented!(),
                })))
            }
            Expr::Block(list) => {
                let mut value = Value::default_rt();
                for stmt in list {
                    value = self.evaluate_stmt(stmt, env.clone())?
                }
                Ok(value)
            }
            _ => Ok(Value::default_rt()),
        }
    }
}
