use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        let mut env = Self::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        self.store
            .get(name)
            .cloned()
            .or_else(|| self.outer.as_ref().and_then(|rc| rc.borrow_mut().get(name)))
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name.to_owned(), val);
        self.store[&name].to_owned()
    }
}
