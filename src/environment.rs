use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, Mutex},
};

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Arc<Mutex<Environment>>>,
}

impl Eq for Environment {}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        if self.store == other.store {
            match (&self.outer, &other.outer) {
                (None, None) => true,
                (Some(arc_a), Some(arc_b)) => Environment::is_env_equal(&arc_a, &arc_b),
                _ => false,
            }
        } else {
            false
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Arc<Mutex<Environment>>) -> Self {
        let mut env = Self::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        self.store.get(name).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|rc| rc.lock().unwrap().get(name))
        })
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name.to_owned(), val);
        self.store[&name].to_owned()
    }

    pub fn is_env_equal(first: &Arc<Mutex<Environment>>, second: &Arc<Mutex<Environment>>) -> bool {
        match (first.lock(), second.lock()) {
            (Ok(env_a), Ok(env_b)) => env_a.deref() == env_b.deref(),
            _ => false,
        }
    }
}
