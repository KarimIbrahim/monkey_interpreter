use std::{fmt::Display, ops::Deref};

use crate::token::Token;

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

pub trait StatementNode: Node {
    fn statement_node(&self);
}

#[derive(Debug, Clone)]
pub struct Statement {
    token: Token,
    pub statement_content: StatementContent,
}

pub trait ExpressionNode: Node {
    fn expression_node(&self);
}

#[derive(Debug, Clone)]
pub struct Expression {
    token: Token,
    pub expression_content: ExpressionContent,
}

#[derive(Debug, Clone)]
pub enum StatementContent {
    Let { name: Expression, value: Expression },
    Return { return_value: Option<Expression> },
    Expression { expression: Expression },
    BlockStatement { statements: Vec<Box<Statement>> },
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
                write!(f, "{} {} = {};", &self.token_literal(), name, value)
            }
            StatementContent::Return { return_value } => {
                stringify_return_statement(f, &self.token_literal(), &return_value)
            }
            StatementContent::Expression { expression } => write!(f, "{}", expression),
            StatementContent::BlockStatement { statements } => Ok(statements
                .iter()
                .for_each(|s| write!(f, "{}", s.deref()).unwrap())),
        }
    }
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

#[derive(Debug, Clone)]
pub enum ExpressionContent {
    Identifier {
        value: String,
    },
    IntegerLiteral {
        value: i64,
    },
    PrefixExpression {
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Boolean {
        value: bool,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    FucntionLiteral {
        parameters: Vec<Box<Expression>>,
        body: Box<Statement>,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
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
            ExpressionContent::Identifier { value } => write!(f, "{}", value),
            ExpressionContent::IntegerLiteral { value } => write!(f, "{}", value),
            ExpressionContent::PrefixExpression { operator, right } => {
                write!(f, "({}{})", operator, right)
            }
            ExpressionContent::InfixExpression {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            ExpressionContent::Boolean { value } => write!(f, "{}", value),
            ExpressionContent::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if{} {}", condition, consequence).unwrap();
                if let Some(alt) = alternative {
                    write!(f, "else {}", alt).unwrap();
                }
                Ok(())
            }
            ExpressionContent::FucntionLiteral { parameters, body } => write!(
                f,
                "{}({}){}",
                self.token_literal(),
                parameters
                    .iter()
                    .map(|b| b.deref().to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                body
            ),
            ExpressionContent::CallExpression {
                function,
                arguments,
            } => write!(
                f,
                "{}({})",
                function.deref(),
                arguments
                    .iter()
                    .map(|b| b.deref().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
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
