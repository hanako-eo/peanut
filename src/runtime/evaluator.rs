use std::{cell::RefCell, rc::Rc};

use crate::{
    errors::Result,
    frontend::{
        ast::{Expr, Op, Stmt, UnaryOp},
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

        environment::declare_var(
            env.clone(),
            "true".into(),
            Rc::new(RefCell::new(Value::True)),
            true,
        );
        environment::declare_var(
            env.clone(),
            "false".into(),
            Rc::new(RefCell::new(Value::False)),
            true,
        );
        environment::declare_var(
            env.clone(),
            "null".into(),
            Rc::new(RefCell::new(Value::Null)),
            true,
        );
        environment::declare_var(
            env.clone(),
            "print".into(),
            Rc::new(RefCell::new(Value::NativeCallback(Box::new(
                |params, _env| {
                    for param in params {
                        print!("{}", param.borrow());
                    }
                    Value::Null
                },
            )))),
            true,
        );
        environment::declare_var(
            env.clone(),
            "println".into(),
            Rc::new(RefCell::new(Value::NativeCallback(Box::new(
                |params, env| {
                    let value = environment::lookup_var(env.clone(), "print".into()).unwrap();
                    match value.borrow().to_owned() {
                        Value::NativeCallback(print) => print(params, env.clone()),
                        _ => unreachable!(),
                    };
                    print!("\n");
                    Value::Null
                },
            )))),
            true,
        );

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
            } => {
                environment::declare_var(
                    env.clone(),
                    name,
                    self.evaluate_expr(value, env.clone())?,
                    constant,
                );
                Ok(Value::default_rt())
            }
            Stmt::FunctionDefinition {
                name,
                return_type: _, // TODO: types
                args,
                body,
            } => {
                environment::declare_var(
                    env.clone(),
                    name,
                    Rc::new(RefCell::new(Value::Function(args, body))),
                    true,
                );
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
            Expr::Return(expr) => {
                let value = self.evaluate_expr(*expr, env.clone())?;

                let env = environment::resolve_with_origin(env, EnvOrigin::Function)?;
                let mut ref_env = env.borrow_mut();
                ref_env.state = EnvState::Return(value);
                Ok(Value::default_rt())
            }
            Expr::Yield(expr) => {
                let value = self.evaluate_expr(*expr, env.clone())?;
                let mut ref_env = env.borrow_mut();
                ref_env.state = EnvState::Yield(value);
                Ok(Value::default_rt())
            }
            Expr::Break(expr) => {
                let value = match expr {
                    Some(expr) => Some(self.evaluate_expr(*expr, env.clone())?),
                    None => None,
                };

                let env = environment::resolve_with_origin(env, EnvOrigin::Loop)?;
                let mut ref_env = env.borrow_mut();
                ref_env.state = EnvState::Break(value);
                Ok(Value::default_rt())
            }
            Expr::Continue => {
                let env = environment::resolve_with_origin(env, EnvOrigin::Loop)?;
                let mut ref_env = env.borrow_mut();
                ref_env.state = EnvState::Continue;
                Ok(Value::default_rt())
            }
            Expr::Op(op, lhs, rhs) => self.evaluate_op(op, *lhs, *rhs, env.clone()),
            Expr::UnaryOp(op, expr) => {
                let value = self.evaluate_expr(*expr, env.clone())?;
                let value = &*value.borrow();
                Ok(Rc::new(RefCell::new(match op {
                    UnaryOp::Not => value.not(),
                    UnaryOp::Positive => value.parse(),
                    UnaryOp::Negate => value.negate(),
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

                let value = environment::lookup_var(env.clone(), funcname)?;
                let (args, body) = match value.borrow().to_owned() {
                    Value::Function(args, body) => (args, body),
                    Value::NativeCallback(function) => {
                        let mut params = Vec::new();
                        for value in values {
                            params.push(value?)
                        }
                        return Ok(Rc::new(RefCell::new(function(params, env.clone()))));
                    }
                    _ => panic!(),
                };

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
                    );
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
            Expr::While(condition, body) => {
                let while_env = Environment::with_parent(Rc::downgrade(&env), EnvOrigin::Loop);
                let value = loop {
                    let value = self.evaluate_expr(*condition.clone(), env.clone())?;
                    let value = value.borrow();
                    if !value.is_truthy() {
                        break None;
                    }

                    self.evaluate_expr(*body.clone(), while_env.clone())?;
                    let while_env = while_env.borrow();
                    match &while_env.state {
                        EnvState::Break(value) => break value.clone(),
                        _ => continue,
                    }
                };
                Ok(value.unwrap_or(Value::default_rt()))
            }
            Expr::Block(list) => {
                let mut value = Value::default_rt();
                let block_env = Environment::with_parent(Rc::downgrade(&env), EnvOrigin::Block);

                for stmt in list {
                    value = self.evaluate_stmt(stmt, block_env.clone())?;
                    let block_env = block_env.borrow();
                    if let EnvState::Return(_) | EnvState::Yield(_) = block_env.state {
                        break;
                    }
                }

                let block_env = block_env.borrow();
                Ok(match &block_env.state {
                    EnvState::Yield(value) => value.clone(),
                    _ => value,
                })
            }
        }
    }

    fn evaluate_op(
        &mut self,
        op: Op,
        lhs: Expr,
        rhs: Expr,
        env: Rc<RefCell<Environment>>,
    ) -> Result<RuntimeValue> {
        let lhs = self.evaluate_expr(lhs, env.clone())?;
        if let Op::And | Op::Or = op {
            let left = &*lhs.borrow();

            if (left.is_truthy() && Op::And == op) || (!left.is_truthy() && Op::Or == op) {
                self.evaluate_expr(rhs, env.clone())
            } else {
                Ok(lhs.clone())
            }
        } else {
            let rhs = self.evaluate_expr(rhs, env.clone())?;

            let left = &*lhs.borrow();
            let right = &*rhs.borrow();

            Ok(Rc::new(RefCell::new(match op {
                Op::Add => left.add(right),
                Op::Subtract => left.sub(right),
                Op::Multiply => left.mul(right),
                Op::Divide => left.try_div(right)?,
                Op::ModDiv => left.try_mod_div(right)?,
                Op::QuotDiv => left.try_quot_div(right)?,
                Op::Equals => left.eq(right),
                Op::NotEquals => left.ne(right),
                Op::GreaterThan => left.gt(right),
                Op::GreaterEquals => left.ge(right),
                Op::LessThan => left.lt(right),
                Op::LessEquals => left.le(right),
                Op::BineryAnd => left.binery_and(right),
                Op::BineryOr => left.binery_or(right),
                _ => unreachable!(),
            })))
        }
    }
}
