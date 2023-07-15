use std::fmt::Display;

use crate::token::Token;

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

pub trait StatementNode: Node {
    fn statement_node(&self);
}

#[derive(Debug)]
pub struct Statement {
    token: Token,
    pub statement_content: StatementContent,
}

pub trait ExpressionNode: Node {
    fn expression_node(&self);
}

#[derive(Debug)]
pub struct Expression {
    token: Token,
    pub expression_content: ExpressionContent,
}

#[derive(Debug)]
pub enum StatementContent {
    Let { name: Literal, value: Expression },

    Return { return_value: Option<Expression> },

    Expression { expression: Expression },
}

impl Statement {
    pub fn new(token: Token, statement_content: StatementContent) -> Self {
        Statement {
            token,
            statement_content,
        }
    }
}

impl StatementNode for Statement {
    fn statement_node(&self) {}
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.statement_content {
            StatementContent::Let { name, value } => {
                stringify_let_statement(f, &self.token_literal(), &name, &value)
            }
            StatementContent::Return { return_value } => {
                stringify_return_statement(f, &self.token_literal(), &return_value)
            }
            StatementContent::Expression { expression } => {
                stringify_expression_statement(f, &expression)
            }
        }
    }
}

fn stringify_expression_statement(
    f: &mut std::fmt::Formatter<'_>,
    expression: &Expression,
) -> std::fmt::Result {
    write!(f, "{}", expression)
}

fn stringify_return_statement(
    f: &mut std::fmt::Formatter<'_>,
    token_literal: &String,
    return_value: &Option<Expression>,
) -> std::fmt::Result {
    write!(f, "{} ", token_literal).unwrap();

    if let Some(expression) = return_value {
        write!(f, "{}", expression).unwrap();
    }

    write!(f, ";")
}

fn stringify_let_statement(
    f: &mut std::fmt::Formatter<'_>,
    token_literal: &String,
    name: &Literal,
    value: &Expression,
) -> std::fmt::Result {
    write!(f, "{} {} = {};", token_literal, name, value)
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

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        stringify_expression_identifier(f, &self.value)
    }
}

#[derive(Debug)]
pub enum ExpressionContent {
    Identifier { value: String },
}

impl Expression {
    pub fn new(token: Token, expression_content: ExpressionContent) -> Self {
        Expression {
            token,
            expression_content,
        }
    }
}

impl ExpressionNode for Expression {
    fn expression_node(&self) {}
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression_content {
            ExpressionContent::Identifier { value } => stringify_expression_identifier(f, value),
        }
    }
}

fn stringify_expression_identifier(
    f: &mut std::fmt::Formatter<'_>,
    value: &str,
) -> std::fmt::Result {
    write!(f, "{}", value)
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement).unwrap();
        }

        Ok(())
    }
}
