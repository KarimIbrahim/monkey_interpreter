const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";

const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };
const NULL: Object = Object::NUll;

#[derive(Debug, Default, PartialEq, Eq)]
pub enum Object {
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    #[default]
    NUll,
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
}

impl Object {
    pub fn r#type(&self) -> &str {
        match self {
            Object::Integer { value: _ } => INTEGER_OBJ,
            Object::Boolean { value: _ } => BOOLEAN_OBJ,
            Object::NUll => NULL_OBJ,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer { value } => value.to_string(),
            Object::Boolean { value } => value.to_string(),
            Object::NUll => "null".to_string(),
        }
    }
}
