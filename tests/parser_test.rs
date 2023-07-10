use monkey_interpreter::{
    ast::{Node, Statement},
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

    assert_eq!(
        program.statements.len(),
        3,
        "program.statements does not contain 3 statements."
    );

    let tests = vec![
        Test::new("x"),
        Test::new("y"),
        Test::new("foobar"),
    ];

    for (i, test) in tests.iter().enumerate() {
        let statement = &program.statements[i];
        if !test_let_statement(statement, test.expected_identifier) {
            return
        }
    }
}

fn test_let_statement(statement: &Statement, identifier_name: &str) -> bool {
    if statement.token_literal() != "let" {
        eprintln!("statement.token_literal() not 'let'. Got=[{}].", statement.token_literal());
        return false;
    }

    if !matches!(statement, Statement::Let { .. }) {
        eprintln!("statement is not a LetStatement. Got=[{statement:?}].");
        return false;
    }
    let Statement::Let { name, .. } = statement;

    if name.value != identifier_name {
        eprintln!("LetStatement.name.value is not [{identifier_name}]. Got=[{}].", name.value);
        return false;
    }

    if name.token_literal() != identifier_name {
        eprintln!("LetStatement.name.token_literal is not [{identifier_name}]. Got=[{}].", name.token_literal());
        return false;
    }

    true
}
