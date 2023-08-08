use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use once_cell::sync::Lazy;

use crate::{
    ast::{Expression, Statement},
    environment::Environment,
};

const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";
const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
const ERROR_OBJ: &str = "ERROR";
const FUNCTION_OBJ: &str = "FUNCTION";
const STRING_OBJ: &str = "STRING";
const BUILT_IN_OBJ: &str = "BUILTIN";

type BUILTINFN = fn(Vec<Object>) -> Object;
pub static BUILTINS: Lazy<HashMap<&str, Object>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "len",
        Object::Builtin {
            builtin_function: |args: Vec<Object>| -> Object {
                if args.len() != 1 {
                    return Object::error(&format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::String { value } => Object::Integer {
                        value: value.len() as i64,
                    },
                    _ => Object::error(&format!(
                        "argument to `len` not supported, got {}",
                        args[0].r#type()
                    )),
                }
            },
        },
    );

    map
});

const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };
const NULL: Object = Object::NUll;

#[derive(Debug, Default, Clone)]
pub enum Object {
    #[default]
    NUll,
    Integer {
        value: i64,
    },
    String {
        value: String,
    },
    Boolean {
        value: bool,
    },
    ReturnValue {
        value: Box<Object>,
    },
    Error {
        message: String,
    },
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Arc<Mutex<Environment>>,
    },
    Builtin {
        builtin_function: BUILTINFN,
    },
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::NUll, Object::NUll) => true,
            (Object::Integer { value: l_val }, Object::Integer { value: r_val }) => l_val == r_val,
            (Object::String { value: l_val }, Object::String { value: r_val }) => l_val == r_val,
            (Object::Boolean { value: l_val }, Object::Boolean { value: r_val }) => l_val == r_val,
            (Object::ReturnValue { value: l_val }, Object::ReturnValue { value: r_val }) => {
                l_val == r_val
            }
            (Object::Error { message: l_message }, Object::Error { message: r_message }) => {
                l_message == r_message
            }
            (
                Object::Function {
                    parameters: l_param,
                    body: l_body,
                    env: l_env,
                },
                Object::Function {
                    parameters: r_param,
                    body: r_body,
                    env: r_env,
                },
            ) => {
                l_param == r_param && l_body == r_body && Environment::is_env_equal(&l_env, &r_env)
            }
            (
                Object::Builtin {
                    builtin_function: l_fun,
                },
                Object::Builtin {
                    builtin_function: r_fun,
                },
            ) => l_fun == r_fun,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl Object {
    pub fn new_integer(value: i64) -> Self {
        Object::Integer { value }
    }

    pub fn boolean(value: bool) -> Self {
        match value {
            true => TRUE,
            false => FALSE,
        }
    }

    pub fn null() -> Self {
        NULL
    }

    pub fn return_value(object: Object) -> Self {
        Object::ReturnValue {
            value: Box::new(object),
        }
    }

    pub fn error(message: &str) -> Self {
        Object::Error {
            message: message.to_string(),
        }
    }

    pub fn function(
        parameters: Vec<Expression>,
        env: Arc<Mutex<Environment>>,
        body: Statement,
    ) -> Self {
        Object::Function {
            parameters,
            body,
            env,
        }
    }

    pub fn r#type(&self) -> &str {
        match self {
            Object::Integer { .. } => INTEGER_OBJ,
            Object::Boolean { .. } => BOOLEAN_OBJ,
            Object::NUll => NULL_OBJ,
            Object::ReturnValue { .. } => RETURN_VALUE_OBJ,
            Object::Error { .. } => ERROR_OBJ,
            Object::Function { .. } => FUNCTION_OBJ,
            Object::String { .. } => STRING_OBJ,
            Object::Builtin { .. } => BUILT_IN_OBJ,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer { value } => value.to_string(),
            Object::Boolean { value } => value.to_string(),
            Object::NUll => "null".to_string(),
            Object::ReturnValue { value } => value.inspect(),
            Object::Error { message } => format!("ERROR: {}", message),
            Object::Function {
                parameters,
                body,
                env: _,
            } => format!(
                "fn({}) {{\n{}\n}}",
                parameters
                    .iter()
                    .map(Expression::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
                body
            ),
            Object::String { value } => value.to_owned(),
            Object::Builtin { .. } => "builtin function".to_string(),
        }
    }
}
