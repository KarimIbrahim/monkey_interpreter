use monkey_interpreter::{
    ast::{Expression, ExpressionContent, Literal, Program, Statement, StatementContent},
    token::{Token, TokenType},
};

#[test]
fn test_string() {
    let program = Program {
        statements: vec![Statement::new(
            Token::new(TokenType::LET, "let"),
            StatementContent::Let {
                name: Literal::new(Token::new(TokenType::IDENT, "myVar"), "myVar".to_string()),
                value: Expression::new(
                    Token::new(TokenType::IDENT, "anotherVar"),
                    ExpressionContent::Identifier {
                        value: "anotherVar".to_string(),
                    },
                ),
            },
        )],
    };

    assert_eq!(
        program.to_string(),
        "let myVar = anotherVar;",
        "program.to_string() wrong."
    )
}
