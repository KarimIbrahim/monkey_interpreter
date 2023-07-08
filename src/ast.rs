use crate::token::Token;

trait Node {
    fn token_literal(&self) -> String;
}

trait Statement: Node {
    fn statement_node(&self);
}

trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program<T: Statement + Node> {
    statements: Vec<T>,
}

impl<T> Program<T>
where
    T: Statement + Node,
{
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: dyn Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {
    }
}

struct Identifier {
    token: Token,
    value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {
    }
}

