use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use crate::errors::{Error, ErrorKind, Result};

use super::value::{EvaluateValue, RuntimeValue};

pub struct Environment {
    pub parent: Weak<RefCell<Environment>>,
    pub variables: HashMap<String, EvaluateValue>,
    pub constants: HashSet<String>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            parent: Weak::new(),
            variables: HashMap::new(),
            constants: HashSet::new(),
        }
    }

    pub fn with_parent(parent: Weak<RefCell<Environment>>) -> Self {
        Self {
            parent,
            variables: HashMap::new(),
            constants: HashSet::new(),
        }
    }
}

pub fn declare_var(
    env: Rc<RefCell<Environment>>,
    varname: String,
    value: RuntimeValue,
    constant: bool,
) -> Result<RuntimeValue> {
    let mut env = env.borrow_mut();
    if env.constants.contains(&varname) {
        env.constants.remove(&varname);
    }

    env.variables.insert(varname.clone(), Rc::downgrade(&value));
    if constant {
        env.constants.insert(varname);
    }
    Ok(value)
}

pub fn assign_var(
    env: Rc<RefCell<Environment>>,
    varname: String,
    value: RuntimeValue,
) -> Result<RuntimeValue> {
    let env = resolve(env, &varname)?;
    let mut env = env.borrow_mut();

    env.variables.insert(varname.clone(), Rc::downgrade(&value));
    Ok(value)
}

pub fn lookup_var(env: Rc<RefCell<Environment>>, varname: String) -> Result<RuntimeValue> {
    let env = resolve(env, &varname)?;
    let env = env.borrow();
    Ok(env.variables.get(&varname).unwrap().upgrade().unwrap())
}

pub fn resolve(
    env: Rc<RefCell<Environment>>,
    varname: &String,
) -> Result<Rc<RefCell<Environment>>> {
    let ref_env = env.borrow_mut();
    match (
        ref_env.variables.contains_key(varname),
        ref_env.parent.upgrade(),
    ) {
        (true, _) => Ok(env.clone()),
        (false, Some(env)) => resolve(env, varname),
        (false, None) => Err(Error::from_kind(ErrorKind::UnknownVariable(
            varname.clone(),
        ))),
    }
}
