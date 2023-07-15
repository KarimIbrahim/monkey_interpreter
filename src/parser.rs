use std::mem;

use crate::{
    ast::{Expression, ExpressionContent, Literal, Program, Statement, StatementContent},
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Default)]
pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            ..Default::default()
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn next_token(&mut self) {
        self.current_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token.token_type != TokenType::EOF {
            let statement = self.parse_statement();

            if let Some(stmt) = statement {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<crate::ast::Statement> {
        match self.current_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.to_owned();

        if !self.expect_peek(&TokenType::IDENT) {
            return None;
        }

        let name = Literal::new(
            self.current_token.to_owned(),
            self.current_token.literal.to_owned(),
        );

        if !self.expect_peek(&TokenType::ASSIGN) {
            return None;
        }

        while !self.current_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::new(
            token.to_owned(),
            StatementContent::Let {
                name,
                value: Expression::new(
                    token,
                    ExpressionContent::Identifier {
                        value: "".to_string(),
                    },
                ),
            },
        ))
    }

    fn current_token_is(&self, token_type: &TokenType) -> bool {
        self.current_token.token_type == *token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn expect_peek(&mut self, token_type: &TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_error(&mut self, token_type: &TokenType) {
        let message = format!(
            "expected next token to be [{:?}], got [{:?}] instead.",
            token_type, self.peek_token.token_type
        );

        self.errors.push(message)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let statement = Statement::new(
            self.current_token.to_owned(),
            StatementContent::Return { return_value: None },
        );

        self.next_token();

        while !self.current_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(statement)
    }
}
