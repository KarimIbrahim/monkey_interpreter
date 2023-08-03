const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";
const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
const ERROR_OBJ: &str = "ERROR";

const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };
const NULL: Object = Object::NUll;

#[derive(Debug, Default, PartialEq, Eq)]
pub enum Object {
    #[default]
    NUll,
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    ReturnValue {
        value: Box<Object>
    },
    Error {
        message: String,
    },
}

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
        Object::ReturnValue { value: Box::new(object) }
    }

    pub fn error(message: &str) -> Self {
        Object::Error { message: message.to_string() }
    }
}

impl Object {
    pub fn r#type(&self) -> &str {
        match self {
            Object::Integer { value: _ } => INTEGER_OBJ,
            Object::Boolean { value: _ } => BOOLEAN_OBJ,
            Object::NUll => NULL_OBJ,
            Object::ReturnValue { value: _ } => RETURN_VALUE_OBJ,
            Object::Error { message: _ } => ERROR_OBJ,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer { value } => value.to_string(),
            Object::Boolean { value } => value.to_string(),
            Object::NUll => "null".to_string(),
            Object::ReturnValue { value } => value.inspect(),
            Object::Error { message } => format!("ERROR: {}", message),
        }
    }
}
