use monkey_interpreter::{
    ast::{Node, Statement, StatementContent},
    lexer::Lexer,
    parser::Parser,
};

struct Test<'a> {
    expected_identifier: &'a str,
}

impl<'a> Test<'a> {
    fn new(expected_identifier: &'a str) -> Test {
        Test {
            expected_identifier,
        }
    }
}

#[test]
fn test_let_statements() {
    let input = r"
    let x = 5;
    let y = 10;
    let foobar = 838383;
    "
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    check_parser_errors(&parser);

    assert_eq!(
        program.statements.len(),
        3,
        "program.statements does not contain 3 statements."
    );

    let tests = vec![Test::new("x"), Test::new("y"), Test::new("foobar")];

    for (i, test) in tests.iter().enumerate() {
        let statement = &program.statements[i];
        if !test_let_statement(statement, test.expected_identifier) {
            return;
        }
    }
}

#[test]
fn test_return_statements() {
    let input = r"
    return 5;
    return 10;
    return 993322;
    "
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    check_parser_errors(&parser);

    assert_eq!(
        program.statements.len(),
        3,
        "program.statements does not contain 3 statements."
    );

    for statement in program.statements {
        assert_eq!(
            statement.token_literal(),
            "return",
            "statement.token_literal is not 'return'."
        );

        assert!(
            matches!(statement.statement_content, StatementContent::Return { .. }),
            "statmement is not Statmement::Return."
        );
    }
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();

    if errors.len() == 0 {
        return;
    }

    eprintln!("parser has [{}] errors.", errors.len());
    for message in errors {
        eprintln!("parser error: [{}]", message);
    }

    panic!("parser failed to parse input program");
}

fn test_let_statement(statement: &Statement, identifier_name: &str) -> bool {
    assert_eq!(
        statement.token_literal(),
        "let",
        "statement.token_literal() not 'let'."
    );

    let StatementContent::Let { name, .. } = &statement.statement_content else { 
        eprintln!("statement is not a LetStatement. Got=[{statement:?}].");
        return false;
    };

    assert_eq!(
        name.value, identifier_name,
        "LetStatement.name.value is not [{identifier_name}]."
    );

    assert_eq!(
        name.token_literal(),
        identifier_name,
        "LetStatement.name.token_literal is not [{identifier_name}]."
    );

    true
}
