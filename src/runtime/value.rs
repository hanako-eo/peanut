use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

#[derive(Default, Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    #[default]
    Null,
}

impl Value {
    pub fn default_rt() -> RuntimeValue {
        Rc::new(RefCell::new(Value::default()))
    }
}

pub type RuntimeValue = Rc<RefCell<Value>>;
pub type EvaluateValue = Weak<RefCell<Value>>;
