use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::errors::Result;

pub type RuntimeValue = Rc<RefCell<Value>>;
pub type EvaluateValue = Weak<RefCell<Value>>;

#[derive(Default, Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    #[default]
    Null,
}

impl Value {
    pub fn default_rt() -> RuntimeValue {
        Rc::new(RefCell::new(Value::default()))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(n) if n != &0. => true,
            Value::String(s) if !s.is_empty() => true,
            _ => false,
        }
    }

    pub fn add(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (&Value::Number(left), &Value::Number(right)) => Value::Number(left + right),
            // (&Value::String(left), &Value::String(right)) => Value::String(left + &right),
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
            (&Value::Number(left), &Value::Number(right)) => Value::Number(left * right),
            // (&Value::String(left), &Value::Number(right))
            // | (&Value::Number(right), &Value::String(left)) => {
            //     Value::String(left.repeat(right as usize))
            // }
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
