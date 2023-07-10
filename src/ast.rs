use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Literal,
        value: Expression,
    },
}

impl Statement {
    pub fn statement_node(&self) {}
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { token, .. } => token.literal.to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub token: Token,
    pub value: String,
}

impl Literal {
    pub fn new(token: Token, value: String) -> Literal {
        Literal { token, value }
    }
}

impl Node for Literal {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier { token: Token, value: String },
}

impl Expression {
    pub fn expression_node(&self) {}
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier { token, .. } => token.literal.to_owned(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program { statements: Vec::new() }
    }

    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}
