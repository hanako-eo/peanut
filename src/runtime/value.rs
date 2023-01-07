use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{errors::Result, frontend::ast::Stmt};

pub type RuntimeValue = Rc<RefCell<Value>>;
pub type EvaluateValue = Weak<RefCell<Value>>;

#[derive(Default, Debug, PartialEq, Clone)]
pub enum Value {
    True,
    False,

    Number(f64),
    String(String),

    Function(/*args*/ Vec<(String, String)>, /*body*/ Vec<Stmt>),
    NativeCallback(Box<fn(Vec<Value>) -> Value>),

    #[default]
    Null,
}

impl Value {
    pub fn default_rt() -> RuntimeValue {
        Rc::new(RefCell::new(Value::default()))
    }
    pub fn bool(condition: bool) -> Self {
        match condition {
            true => Value::True,
            false => Value::False,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(n) if n != &0. => true,
            Value::String(s) if !s.is_empty() => true,
            Value::True => true,
            _ => false,
        }
    }

    pub fn eq(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::bool(left == right),
            (Value::String(left), Value::String(right)) => Value::bool(left.len() == right.len()),
            (Value::Null, Value::Null) => Value::True,
            _ => Value::False,
        }
    }
    pub fn ne(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::bool(left != right),
            (Value::String(left), Value::String(right)) => Value::bool(left.len() != right.len()),
            (Value::Null, Value::Null) => Value::False,
            _ => Value::True,
        }
    }
    pub fn gt(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::bool(left > right),
            (Value::String(left), Value::String(right)) => Value::bool(left.len() > right.len()),
            _ => Value::False,
        }
    }
    pub fn ge(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::bool(left >= right),
            (Value::String(left), Value::String(right)) => Value::bool(left.len() >= right.len()),
            _ => Value::False,
        }
    }
    pub fn lt(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::bool(left < right),
            (Value::String(left), Value::String(right)) => Value::bool(left.len() < right.len()),
            _ => Value::False,
        }
    }
    pub fn le(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::bool(left <= right),
            (Value::String(left), Value::String(right)) => Value::bool(left.len() <= right.len()),
            _ => Value::False,
        }
    }

    pub fn add(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::Number(left + right),
            (Value::String(left), Value::String(right)) => Value::String(format!("{left}{right}")),
            _ => unimplemented!(),
        }
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (&Value::Number(left), &Value::Number(right)) => Value::Number(left - right),
            _ => unimplemented!(),
        }
    }

    pub fn mul(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Number(left), Value::Number(right)) => Value::Number(left * right),
            (Value::String(left), Value::Number(right))
            | (Value::Number(right), Value::String(left)) => {
                Value::String(left.repeat(*right as usize))
            }
            _ => unimplemented!(),
        }
    }

    pub fn try_div(&self, rhs: &Self) -> Result<Self> {
        match (self, rhs) {
            (&Value::Number(left), &Value::Number(right)) => Ok(Value::Number(left / right)),
            _ => unimplemented!(),
        }
    }
    pub fn try_mod_div(&self, rhs: &Self) -> Result<Self> {
        match (self, rhs) {
            (&Value::Number(left), &Value::Number(right)) => Ok(Value::Number(left % right)),
            _ => unimplemented!(),
        }
    }
    pub fn try_quot_div(&self, rhs: &Self) -> Result<Self> {
        match (self, rhs) {
            (&Value::Number(left), &Value::Number(right)) => {
                Ok(Value::Number(((left as i64) / (right as i64)) as f64))
            }
            _ => unimplemented!(),
        }
    }
}
