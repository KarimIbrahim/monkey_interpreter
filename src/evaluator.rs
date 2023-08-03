use crate::{ast::Node, object::Object};

pub fn eval<T: Node>(node: T) -> Object {
    node.eval()
}
