use std::{collections::HashMap, mem};

use once_cell::sync::Lazy;

use crate::{
    ast::{Expression, ExpressionContent, Program, Statement, StatementContent},
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFunction = fn(&mut Parser) -> Option<Expression>;
type InfixParseFunction = fn(&mut Parser, &Expression) -> Option<Expression>;

const LOWEST: usize = 1;
const EQUALS: usize = 2;
const LESSGREATER: usize = 3;
const SUM: usize = 4;
const PRODUCT: usize = 5;
const PREFIX: usize = 6;
const CALL: usize = 7;

static PRECEDENCES: Lazy<HashMap<TokenType, usize>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(TokenType::EQ, EQUALS);
    map.insert(TokenType::NOT_EQ, EQUALS);
    map.insert(TokenType::LT, LESSGREATER);
    map.insert(TokenType::GT, LESSGREATER);
    map.insert(TokenType::PLUS, SUM);
    map.insert(TokenType::MINUS, SUM);
    map.insert(TokenType::SLASH, PRODUCT);
    map.insert(TokenType::ASTERISK, PRODUCT);

    map
});

#[derive(Default)]
pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,
    current_token: Token,
    peek_token: Token,
    prefix_parse_functions: HashMap<TokenType, PrefixParseFunction>,
    infix_parse_functions: HashMap<TokenType, InfixParseFunction>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            ..Default::default()
        };

        parser.register_prefix(TokenType::IDENT, Self::parse_identifier);
        parser.register_prefix(TokenType::INT, Self::parse_integer_literal);
        parser.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);

        parser.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::NOT_EQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::LT, Self::parse_infix_expression);
        parser.register_infix(TokenType::GT, Self::parse_infix_expression);

        parser.register_prefix(TokenType::TRUE, Self::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Self::parse_boolean);

        parser.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::new(
            self.current_token.to_owned(),
            ExpressionContent::Boolean {
                value: self.current_token_is(&TokenType::TRUE),
            },
        ))
    }

    pub fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(LOWEST);

        if !self.expect_peek(&TokenType::RPAREN) {
            None
        } else {
            expression
        }
    }

    pub fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::new(
            self.current_token.to_owned(),
            ExpressionContent::Identifier {
                value: self.current_token.literal.to_owned(),
            },
        ))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let Ok(value) = self.current_token.literal.parse() else {
            self.errors.push(format!("could not parse [{}] as integer.", self.current_token.literal));
            return None;
        };

        Some(Expression::new(
            self.current_token.to_owned(),
            ExpressionContent::IntegerLiteral { value },
        ))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.current_token.literal.to_owned();
        let token = self.current_token.to_owned();

        self.next_token();
        let right = self.parse_expression(PREFIX)?;

        Some(Expression::new(
            token,
            ExpressionContent::PrefixExpression {
                operator,
                right: Box::new(right),
            },
        ))
    }

    fn parse_infix_expression(&mut self, left: &Expression) -> Option<Expression> {
        let token = self.current_token.to_owned();
        let operator = self.current_token.literal.to_owned();

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Expression::new(
            token,
            ExpressionContent::InfixExpression {
                left: Box::new(left.to_owned()),
                operator,
                right: Box::new(right),
            },
        ))
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
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.to_owned();

        if !self.expect_peek(&TokenType::IDENT) {
            return None;
        }

        let name = Expression::new(
            self.current_token.to_owned(),
            ExpressionContent::Identifier {
                value: self.current_token.literal.to_owned(),
            },
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

    fn register_prefix(&mut self, token_type: TokenType, parse_function: PrefixParseFunction) {
        self.prefix_parse_functions
            .insert(token_type, parse_function);
    }

    fn register_infix(&mut self, token_type: TokenType, parse_function: InfixParseFunction) {
        self.infix_parse_functions
            .insert(token_type, parse_function);
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let statement = Statement::new(
            self.current_token.to_owned(),
            StatementContent::Expression {
                expression: self.parse_expression(LOWEST)?,
            },
        );

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_expression(&mut self, precedence: usize) -> Option<Expression> {
        let Some(parse_prefix) = self.prefix_parse_functions
            .get(&self.current_token.token_type) else {
            self.no_prefix_parse_function_error(self.current_token.token_type);
            return None;
        };
        let mut left_expression = parse_prefix(self)?;

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            if !self
                .infix_parse_functions
                .contains_key(&self.peek_token.token_type)
            {
                return Some(left_expression);
            }

            self.next_token();
            left_expression =
                self.infix_parse_functions[&self.current_token.token_type](self, &left_expression)?;
        }

        Some(left_expression)
    }

    fn no_prefix_parse_function_error(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "no prefix parse function for [{:?}] found.",
            token_type
        ));
    }

    fn peek_precedence(&self) -> usize {
        *PRECEDENCES
            .get(&self.peek_token.token_type)
            .unwrap_or(&LOWEST)
    }

    fn current_precedence(&self) -> usize {
        *PRECEDENCES
            .get(&self.current_token.token_type)
            .unwrap_or(&LOWEST)
    }
}
