use std::{any::{Any, TypeId}, ops::Deref};

use monkey_interpreter::{
    ast::{ExpressionContent, Node, Statement, StatementContent, Expression},
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
        test_let_statement(&program.statements[i], test.expected_identifier); 
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

fn test_let_statement(statement: &Statement, identifier_name: &str) {
    assert_eq!(
        statement.token_literal(),
        "let",
        "statement.token_literal() not 'let'."
    );

    let StatementContent::Let { name, .. } = &statement.statement_content else { 
        panic!("statement is not a LetStatement. Got=[{statement:?}].");
    };

    let ExpressionContent::Identifier { value } = &name.expression_content else {
        panic!("expression content is not an Identifier. Got=[{name}].");
    };

    assert_eq!(
        value, identifier_name,
        "LetStatement.name.value is not [{identifier_name}]."
    );

    assert_eq!(
        name.token_literal(),
        identifier_name,
        "LetStatement.name.token_literal is not [{identifier_name}]."
    );
}

#[test]
fn test_identifier_expression() {
    let input = "foobar".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    check_parser_errors(&parser);

    assert_eq!(
        program.statements.len(),
        1,
        "program.statements does not contain 1 statements."
    );

    let statement = &program.statements[0];
    assert_eq!(
        statement.token_literal(),
        "foobar",
        "statement.token_literal is not 'foobar'."
    );

    let StatementContent::Expression { expression } = &statement.statement_content else {
        panic!("statement content is not an Expression. Got=[{:?}].", statement.statement_content);
    };

    assert_eq!(
        expression.token_literal(),
        "foobar",
        "expression.token_literal is not 'foobar'."
    );

    let ExpressionContent::Identifier { value } = &expression.expression_content else {
        panic!("expression content is not an Identifier. Got=[{:?}].", expression.expression_content);
    };

    assert_eq!(value, "foobar", "identifier.value is not 'foobar'.");
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    check_parser_errors(&parser);

    assert_eq!(
        program.statements.len(),
        1,
        "program.statements does not contain 1 statements."
    );

    let statement = &program.statements[0];
    assert_eq!(
        statement.token_literal(),
        "5",
        "statement.token_literal is not '5'."
    );

    let StatementContent::Expression { expression } = &statement.statement_content else {
        panic!("statement content is not an Expression. Got=[{:?}].", statement.statement_content);
    };

    assert_eq!(
        expression.token_literal(),
        "5",
        "expression.token_literal is not '5'."
    );

    let ExpressionContent::IntegerLiteral { value } = &expression.expression_content else {
        panic!("expression content is not an IntegerLiteral. Got=[{:?}].", expression.expression_content);
    };

    assert_eq!(value, &5, "expression value is not 5.");
}

#[test]
fn test_parsing_prefix_expressions() {
    struct PrefixTest<'a> {
        input: String,
        operator: String,
        value: Box<&'a dyn Any>,
    }
    impl<'a> PrefixTest<'a> {
        pub fn new(input: String, operator: String, value: &'a dyn Any) -> Self {
            PrefixTest { input, operator, value: Box::new(value) }
        }
    }

    let prefix_tests = [
        PrefixTest::new("!5;".to_string(), "!".to_string(), &5),
        PrefixTest::new("-15;".to_string(), "-".to_string(), &15),
        PrefixTest::new("!true;".to_string(), "!".to_string(), &true),
        PrefixTest::new("!false;".to_string(), "!".to_string(), &false),
    ];

    for test in prefix_tests {

        let lexer = Lexer::new(test.input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements."
        );

        let statement = &program.statements[0];
        let StatementContent::Expression { expression } = &statement.statement_content else {
            panic!("statement is not an Expression. Got [{}].", statement);
        };

        let ExpressionContent::PrefixExpression { operator, right } = &expression.expression_content else {
            panic!("expression is not a PrefixExpression. Got [{:?}].", expression.expression_content);
        };

        assert_eq!(*operator, test.operator, "expression.operator does not match.");

        test_literal_expression(right, &test.value);
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct InfixTest<'a> {
        input: String,
        left_value: Box<&'a dyn Any>,
        operator: String,
        right_value: Box<&'a dyn Any>,
    }
    impl<'a> InfixTest<'a> {
        pub fn new(input: String, left_value: &'a dyn Any, operator: String, right_value: &'a dyn Any) -> Self {
            InfixTest { input, left_value: Box::new(left_value), operator, right_value: Box::new(right_value) }
        }
    }

    let infix_tests = [
        InfixTest::new("5 + 5;".to_string(), &5, "+".to_string(), &5),
        InfixTest::new("5 - 5;".to_string(), &5, "-".to_string(), &5),
        InfixTest::new("5 * 5;".to_string(), &5, "*".to_string(), &5),
        InfixTest::new("5 / 5;".to_string(), &5, "/".to_string(), &5),
        InfixTest::new("5 > 5;".to_string(), &5, ">".to_string(), &5),
        InfixTest::new("5 < 5;".to_string(), &5, "<".to_string(), &5),
        InfixTest::new("5 == 5;".to_string(), &5, "==".to_string(), &5),
        InfixTest::new("5 != 5;".to_string(), &5, "!=".to_string(), &5),
		InfixTest::new("foobar + barfoo;".to_string(), &"foobar", "+".to_string(), &"barfoo"),
		InfixTest::new("foobar - barfoo;".to_string(), &"foobar", "-".to_string(), &"barfoo"),
		InfixTest::new("foobar * barfoo;".to_string(), &"foobar", "*".to_string(), &"barfoo"),
		InfixTest::new("foobar / barfoo;".to_string(), &"foobar", "/".to_string(), &"barfoo"),
		InfixTest::new("foobar > barfoo;".to_string(), &"foobar", ">".to_string(), &"barfoo"),
		InfixTest::new("foobar < barfoo;".to_string(), &"foobar", "<".to_string(), &"barfoo"),
		InfixTest::new("foobar == barfoo;".to_string(), &"foobar", "==".to_string(), &"barfoo"),
		InfixTest::new("foobar != barfoo;".to_string(), &"foobar", "!=".to_string(), &"barfoo"),
		InfixTest::new("true == true".to_string(), &true, "==".to_string(), &true),
		InfixTest::new("true != false".to_string(), &true, "!=".to_string(), &false),
		InfixTest::new("false == false".to_string(), &false, "==".to_string(), &false),
    ];

    for test in infix_tests {

        let lexer = Lexer::new(test.input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements."
        );

        let statement = &program.statements[0];
        let StatementContent::Expression { expression } = &statement.statement_content else {
            panic!("statement is not an Expression. Got [{}].", statement);
        };

        test_infix_expression(expression, &test.left_value, test.operator, &test.right_value);
    }
}

fn test_integer_literal(expression: &Expression, value: i64) {
    let ExpressionContent::IntegerLiteral { value: exp_value } = expression.expression_content else {
        panic!("expression is not an IntegerLiteral. Got [{:?}].", expression.expression_content);
    };

    assert_eq!(exp_value, value, "Integer value does not match.");

    assert_eq!(expression.token_literal(), value.to_string(), "expression.token_literal does not match.");
}

#[test]
fn test_operator_precedence_parsing() {
    #[derive(Debug)]
    struct Test {
        input: String,
        expected: String,
    }
    impl Test {
        pub fn new(input: String, expected: String) -> Self {
            Test { input, expected }
        }
    }

    let tests = [
        Test::new("-a * b".to_string(),
            "((-a) * b)".to_string()),
        Test::new("!-a".to_string(),
            "(!(-a))".to_string()),
        Test::new("a + b + c".to_string(),
            "((a + b) + c)".to_string()),
        Test::new("a + b - c".to_string(),
            "((a + b) - c)".to_string()),
        Test::new("a * b * c".to_string(),
            "((a * b) * c)".to_string()),
        Test::new("a * b / c".to_string(),
            "((a * b) / c)".to_string()),
        Test::new("a + b / c".to_string(),
            "(a + (b / c))".to_string()),
        Test::new("a + b * c + d / e -f".to_string(),
            "(((a + (b * c)) + (d / e)) - f)".to_string()),
        Test::new("3 + 4; -5 * 5".to_string(),
            "(3 + 4)((-5) * 5)".to_string()),
        Test::new("5 > 4 == 3 < 4".to_string(),
            "((5 > 4) == (3 < 4))".to_string()),
        Test::new("5 < 4 != 3 > 4".to_string(),
            "((5 < 4) != (3 > 4))".to_string()),
        Test::new("3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string()),
        Test::new("true".to_string(), "true".to_string()),
        Test::new("false".to_string(), "false".to_string()),
        Test::new("3 > 5 == false".to_string(), "((3 > 5) == false)".to_string()),
        Test::new("3 < 5 == true".to_string(), "((3 < 5) == true)".to_string()),
    ];

    for test in tests {

        let lexer = Lexer::new(test.input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(test.expected, program.to_string(), "expected program does not match.");
    }
}

fn test_identifier(expression: &Expression, value: &String) {
    let ExpressionContent::Identifier { value: ident_value } = &expression.expression_content else {
        panic!("expression is not an Identifier. Got [{}].", expression);
    };

    assert_eq!(ident_value, value, "identifier value is does not match.");

    assert_eq!(&expression.token_literal(), value, "identifier token literal does not match.");
}

fn test_literal_expression(expression: &Expression, expected: &dyn Any) {
    let type_id = expected.deref().type_id();

    if type_id == TypeId::of::<i32>() {
        test_integer_literal(expression, (*expected.downcast_ref::<i32>().unwrap()).into());
    }
    if type_id == TypeId::of::<i64>() {
        test_integer_literal(expression, *expected.downcast_ref::<i64>().unwrap());
    }
    if type_id == TypeId::of::<String>() {
        test_identifier(expression, expected.downcast_ref::<String>().unwrap());
    }
    if type_id == TypeId::of::<bool>() {
        test_boolean_literal(expression, *expected.downcast_ref::<bool>().unwrap());
    }
}

fn test_boolean_literal(expression: &Expression, value: bool) {
    let ExpressionContent::Boolean { value: bool_value } = expression.expression_content else {
        panic!("expression is not a Boolean. Got [{}].", expression);
    };

    assert_eq!(value, bool_value, "boolean value does not match.");

    assert_eq!(expression.token_literal(), value.to_string(), "boolean token literal does not match.");
}

fn test_infix_expression(expression: &Expression, expected_left: &dyn Any, expected_operator: String, expected_right: &dyn Any) {
    let ExpressionContent::InfixExpression { left, operator, right } = &expression.expression_content else {
        panic!("expression is not an InfixExpression. Got [{}].", expression);
    };

    test_literal_expression(left, expected_left);

    assert_eq!(operator, &expected_operator, "expression operator does not match.");

    test_literal_expression(right, expected_right);
}
