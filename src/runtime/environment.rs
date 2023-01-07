use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use crate::{
    errors::{Error, ErrorKind, Result},
    frontend::ast::Stmt,
};

use super::value::{EvaluateValue, RuntimeValue};

pub enum EnvOrigin {
    Global,
    Function,
}

pub enum EnvState {
    Continue,
    Break,
    Return(RuntimeValue),
    None,
}

pub struct Environment {
    pub parent: Weak<RefCell<Environment>>,
    pub variables: HashMap<String, RuntimeValue>,
    pub constants: HashSet<String>,
    pub origin: EnvOrigin,
    pub state: EnvState,
}

impl Environment {
    pub fn new(origin: EnvOrigin) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Weak::new(),
            variables: HashMap::new(),
            constants: HashSet::new(),
            state: EnvState::None,
            origin,
        }))
    }

    pub fn with_parent(parent: Weak<RefCell<Environment>>, origin: EnvOrigin) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent,
            variables: HashMap::new(),
            constants: HashSet::new(),
            state: EnvState::None,
            origin,
        }))
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

    env.variables.insert(varname.clone(), value.clone());
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
    let env = resolve_var(env, &varname)?;
    let mut env = env.borrow_mut();

    env.variables.insert(varname.clone(), value.clone());
    Ok(value)
}

pub fn lookup_var(env: Rc<RefCell<Environment>>, varname: String) -> Result<RuntimeValue> {
    let env = resolve_var(env, &varname)?;
    let env = env.borrow();
    Ok(env.variables.get(&varname).unwrap().clone())
}

pub fn resolve_var(
    env: Rc<RefCell<Environment>>,
    varname: &String,
) -> Result<Rc<RefCell<Environment>>> {
    let ref_env = env.borrow();
    match (
        ref_env.variables.contains_key(varname),
        ref_env.parent.upgrade(),
    ) {
        (true, _) => Ok(env.clone()),
        (false, Some(env)) => resolve_var(env, varname),
        (false, None) => Err(Error::from_kind(ErrorKind::UnknownVariable(
            varname.clone(),
        ))),
    }
}
