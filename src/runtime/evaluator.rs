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
    runtime::environment::EnvState,
};

use super::{
    environment::{self, EnvOrigin, Environment},
    value::{RuntimeValue, Value},
};

pub struct Evaluator;

impl Evaluator {
    pub fn evaluate(source: &str) -> Result<RuntimeValue> {
        let program = Parser::parse(source)?;
        let env = Environment::new(EnvOrigin::Global);
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
            Stmt::FunctionDefinition {
                name,
                return_type: _, // TODO: types
                args,
                body,
            } => {
                environment::declare_function(env.clone(), name, args, body);
                Ok(Value::default_rt())
            }
            Stmt::Return(expr) => {
                let value = self.evaluate_expr(expr, env.clone())?;
                let mut ref_env = env.borrow_mut();
                ref_env.state = EnvState::Return(value);
                Ok(Value::default_rt())
            }
            Stmt::ExprStmt(expr) => self.evaluate_expr(expr, env.clone()),
            _ => Ok(Value::default_rt()),
        }
    }

    fn evaluate_expr(&mut self, expr: Expr, env: Rc<RefCell<Environment>>) -> Result<RuntimeValue> {
        match expr {
            Expr::Identifier(varname) => environment::lookup_var(env, varname),
            Expr::NumberLiteral(n) => Ok(Rc::new(RefCell::new(Value::Number(n)))),
            Expr::StringLiteral(str) => Ok(Rc::new(RefCell::new(Value::String(str)))),
            Expr::Assign(assigne, expr) => {
                let varname = match *assigne {
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
            Expr::Call(calle, params) => {
                let funcname = match *calle {
                    Expr::Identifier(symbol) => symbol,
                    _ => panic!(),
                };

                let values: Vec<_> = params
                    .iter()
                    .map(|expr| self.evaluate_expr(expr.clone(), env.clone()))
                    .collect();

                let (args, body) = environment::lookup_function(env.clone(), funcname)?;

                assert_eq!(values.len(), args.len());

                let func_env = Environment::with_parent(Rc::downgrade(&env), EnvOrigin::Function);

                for (i, expr) in values.iter().enumerate() {
                    let value = match expr.as_ref() {
                        Ok(v) => v.clone(),
                        Err(err) => return Err(err.clone()),
                    };

                    environment::declare_var(
                        func_env.clone(),
                        args.get(i).unwrap().0.clone(),
                        value,
                        false,
                    )?;
                }

                for stmt in body {
                    self.evaluate_stmt(stmt, func_env.clone())?;
                    let func_env = func_env.borrow();
                    if let EnvState::Return(_) = func_env.state {
                        break;
                    }
                }
                let func_env = func_env.borrow();
                Ok(match &func_env.state {
                    EnvState::Return(value) => value.clone(),
                    _ => Value::default_rt(),
                })
            }
            Expr::If(conditions) => {
                let mut value = Value::default_rt();
                for (condition, expr) in conditions {
                    let result =
                        condition.map(|condition| self.evaluate_expr(condition, env.clone()));

                    match result {
                        Some(result) => {
                            if result?.borrow().is_truthy() {
                                value = self.evaluate_expr(*expr, env)?;
                                break;
                            }
                        }
                        None => {
                            value = self.evaluate_expr(*expr, env)?;
                            break;
                        }
                    }
                }
                Ok(value)
            }
            Expr::Block(list) => {
                let mut value = Value::default_rt();
                for stmt in list {
                    value = self.evaluate_stmt(stmt, env.clone())?;
                    let env = env.borrow();
                    if let EnvState::Return(_) = env.state {
                        break;
                    }
                }
                Ok(value)
            }
            _ => Ok(Value::default_rt()),
        }
    }
}
