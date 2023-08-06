use monkey_interpreter::{lexer::Lexer, token::TokenType};

struct Test<'a> {
    pub expected_type: TokenType,
    pub expected_literal: &'a str,
}

impl<'a> Test<'a> {
    fn new(expected_type: TokenType, expected_literal: &'a str) -> Self {
        Test {
            expected_type,
            expected_literal,
        }
    }
}

#[test]
fn test_next_token() {
    let input = r#"let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
    "foobar"
    "foo bar"
    "#
    .to_string();

    let tests = vec![
        Test::new(TokenType::LET, "let"),
        Test::new(TokenType::IDENT, "five"),
        Test::new(TokenType::ASSIGN, "="),
        Test::new(TokenType::INT, "5"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::LET, "let"),
        Test::new(TokenType::IDENT, "ten"),
        Test::new(TokenType::ASSIGN, "="),
        Test::new(TokenType::INT, "10"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::LET, "let"),
        Test::new(TokenType::IDENT, "add"),
        Test::new(TokenType::ASSIGN, "="),
        Test::new(TokenType::FUNCTION, "fn"),
        Test::new(TokenType::LPAREN, "("),
        Test::new(TokenType::IDENT, "x"),
        Test::new(TokenType::COMMA, ","),
        Test::new(TokenType::IDENT, "y"),
        Test::new(TokenType::RPAREN, ")"),
        Test::new(TokenType::LBRACE, "{"),
        Test::new(TokenType::IDENT, "x"),
        Test::new(TokenType::PLUS, "+"),
        Test::new(TokenType::IDENT, "y"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::RBRACE, "}"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::LET, "let"),
        Test::new(TokenType::IDENT, "result"),
        Test::new(TokenType::ASSIGN, "="),
        Test::new(TokenType::IDENT, "add"),
        Test::new(TokenType::LPAREN, "("),
        Test::new(TokenType::IDENT, "five"),
        Test::new(TokenType::COMMA, ","),
        Test::new(TokenType::IDENT, "ten"),
        Test::new(TokenType::RPAREN, ")"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::BANG, "!"),
        Test::new(TokenType::MINUS, "-"),
        Test::new(TokenType::SLASH, "/"),
        Test::new(TokenType::ASTERISK, "*"),
        Test::new(TokenType::INT, "5"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::INT, "5"),
        Test::new(TokenType::LT, "<"),
        Test::new(TokenType::INT, "10"),
        Test::new(TokenType::GT, ">"),
        Test::new(TokenType::INT, "5"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::IF, "if"),
        Test::new(TokenType::LPAREN, "("),
        Test::new(TokenType::INT, "5"),
        Test::new(TokenType::LT, "<"),
        Test::new(TokenType::INT, "10"),
        Test::new(TokenType::RPAREN, ")"),
        Test::new(TokenType::LBRACE, "{"),
        Test::new(TokenType::RETURN, "return"),
        Test::new(TokenType::TRUE, "true"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::RBRACE, "}"),
        Test::new(TokenType::ELSE, "else"),
        Test::new(TokenType::LBRACE, "{"),
        Test::new(TokenType::RETURN, "return"),
        Test::new(TokenType::FALSE, "false"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::RBRACE, "}"),
        Test::new(TokenType::INT, "10"),
        Test::new(TokenType::EQ, "=="),
        Test::new(TokenType::INT, "10"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::INT, "10"),
        Test::new(TokenType::NOT_EQ, "!="),
        Test::new(TokenType::INT, "9"),
        Test::new(TokenType::SEMICOLON, ";"),
        Test::new(TokenType::STRING, "foobar"),
        Test::new(TokenType::STRING, "foo bar"),
        Test::new(TokenType::EOF, ""),
    ];

    let mut lexer = Lexer::new(input);

    for (i, test) in tests.iter().enumerate() {
        let token = lexer.next_token();

        assert_eq!(
            token.token_type, test.expected_type,
            "tests[{}] - tokentype wrong. Expected=[{:?}], got=[{:?}].",
            i, test.expected_type, token.token_type
        );

        assert_eq!(
            token.literal, test.expected_literal,
            "tests[{}] - literal wrong. Expected=[{}], got=[{}].",
            i, test.expected_literal, token.literal
        );
    }
}
