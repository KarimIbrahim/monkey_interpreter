use std::{any::{Any, TypeId}, ops::Deref};

use monkey_interpreter::{
    ast::{ExpressionContent, Node, Statement, StatementContent, Expression},
    lexer::Lexer,
    parser::Parser,
};

#[test]
fn test_let_statements() {
    struct Test<'a> {
        input: &'a str,
        expected_identifier: &'a str,
        expected_value: Box<&'a dyn Any>,
    }
    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected_identifier: &'a str, expected_value: &'a dyn Any) -> Self {
            Test { input, expected_identifier, expected_value: Box::new(expected_value) }
        }
    }

    let prefix_tests = [
        Test::new("let x = 5;", "x", &5),
        Test::new("let y = true;", "y", &true),
        Test::new("let foobar = y", "foobar", &"y"),
    ];

    for test in prefix_tests {

        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements."
        );

        let statement = &program.statements[0];
        test_let_statement(statement, test.expected_identifier);

        let StatementContent::Let { name: _, value } = &statement.statement_content else {
            panic!("Statement is not a let statement. Got [{}].", statement);
        };

        test_literal_expression(value, &test.expected_value);
    }
}

#[test]
fn test_return_statements() {
    struct Test<'a> {
        input: &'a str,
        expected_value: Box<&'a dyn Any>,
    }
    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected_value: &'a dyn Any) -> Self {
            Test { input, expected_value: Box::new(expected_value) }
        }
    }

    let prefix_tests = [
        Test::new("return 5;", &5),
        Test::new("return true;", &true),
        Test::new("return foobar;", &"foobar"),
    ];

    for test in prefix_tests {

        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements."
        );

        let statement = &program.statements[0];
        let StatementContent::Return { return_value } = &statement.statement_content else {
            panic!("Statement is not a return statement. Got [{}].", statement);
        };

        assert_eq!(statement.token_literal(), "return".to_string(), "Token literal is not return.");

        test_literal_expression(return_value.as_ref().unwrap(), &test.expected_value);
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
        Test::new("1 + (2 + 3) + 4".to_string(), "((1 + (2 + 3)) + 4)".to_string()),
        Test::new("(5 + 5) * 2".to_string(), "((5 + 5) * 2)".to_string()),
        Test::new("2 / (5 + 5)".to_string(), "(2 / (5 + 5))".to_string()),
        Test::new("-(5 + 5)".to_string(), "(-(5 + 5))".to_string()),
        Test::new("!(true == true)".to_string(), "(!(true == true))".to_string()),
        Test::new("a + add(b * c) + d".to_string(), "((a + add((b * c))) + d)".to_string()),
        Test::new("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(), "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string()),
        Test::new("add(a + b + c * d / f + g)".to_string(), "add((((a + b) + ((c * d) / f)) + g))".to_string()),
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

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }".to_string();

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
    let StatementContent::Expression { expression } = &statement.statement_content else {
        panic!("statement is not an Expression. Got [{}].", statement);
    };

    let ExpressionContent::IfExpression { condition, consequence, alternative } = &expression.expression_content else {
        panic!("expression is not an IfExpression. Got [{:?}].", expression.expression_content);
    };

    test_infix_expression(condition, &"x".to_string(), "<".to_string(), &"y".to_string());

    let StatementContent::BlockStatement { statements } = &consequence.statement_content else {
        panic!("consequence is not a block statement. Got [{}].", consequence);
    };

    assert_eq!(statements.len(), 1, "consequence has wrong number of statements.");

    let StatementContent::Expression { expression } = &statements[0].statement_content else {
        panic!("Statements[0] is not an Expression. Got [{}].", statements[0].deref());
    };

    test_identifier(&expression, &"x".to_string());

    let None = alternative else {
        panic!("Alternative is not None. Got [{:?}].", alternative);
    };
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }".to_string();

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
    let StatementContent::Expression { expression } = &statement.statement_content else {
        panic!("statement is not an Expression. Got [{}].", statement);
    };

    let ExpressionContent::IfExpression { condition, consequence, alternative } = &expression.expression_content else {
        panic!("expression is not an IfExpression. Got [{:?}].", expression.expression_content);
    };

    test_infix_expression(condition, &"x".to_string(), "<".to_string(), &"y".to_string());

    let StatementContent::BlockStatement { statements } = &consequence.statement_content else {
        panic!("consequence is not a block statement. Got [{}].", consequence);
    };

    assert_eq!(statements.len(), 1, "consequence has wrong number of statements.");

    let StatementContent::Expression { expression } = &statements[0].statement_content else {
        panic!("Statements[0] is not an Expression. Got [{}].", statements[0].deref());
    };

    test_identifier(&expression, &"x".to_string());

    let Some(alt) = alternative else {
        panic!("Alternative is None.");
    };

    let StatementContent::BlockStatement { statements: alt_statements } = &alt.statement_content else {
        panic!("alternative is not a block statement. Got [{}].", alt);
    };

    assert_eq!(alt_statements.len(), 1, "alternative has wrong number of statements.");

    let StatementContent::Expression { expression: alt_expression } = &alt_statements[0].statement_content else {
        panic!("Statements[0] is not an Expression. Got [{}].", alt_statements[0].deref());
    };

    test_identifier(&alt_expression, &"y".to_string());
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }".to_string();

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
    let StatementContent::Expression { expression } = &statement.statement_content else {
        panic!("statement is not an Expression. Got [{}].", statement);
    };

    let ExpressionContent::FucntionLiteral { parameters, body } = &expression.expression_content else {
        panic!("expression is not an FunctionLiteral. Got [{:?}].", expression.expression_content);
    };

    assert_eq!(2, parameters.len(), "Fucntion literal has wrong number of parameters.");

    test_literal_expression(&parameters[0], &"x".to_string());
    test_literal_expression(&parameters[1], &"y".to_string());

    let StatementContent::BlockStatement { statements } = &body.statement_content else {
        panic!("Function body is not a BlockStatement. Got [{}].", body);
    };

    assert_eq!(1, statements.len(), "Function body has wrong number of statements.");

    let StatementContent::Expression { expression } = &statements[0].statement_content else {
        panic!("Fucntion body does not have an expression. Got [{}].", statements[0]);
    };

    test_infix_expression(&expression, &"x", "+".to_string(), &"y");
}

#[test]
fn test_function_parameter_parsing() {
    #[derive(Debug)]
    struct Test<'a> {
        input: &'a str,
        expected_params: Vec<&'a str>,
    }
    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected_params: Vec<&'a str>) -> Self {
            Test { input, expected_params }
        }
    }

    let tests = [
        Test::new("fn() {};",
            vec![]),
        Test::new("fn(x) {};",
            vec!["x"]),
        Test::new("fn(x, y, z) {};",
            vec!["x", "y", "z"]),
    ];

    for test in tests {

        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);

        let statement = &program.statements[0];
        let StatementContent::Expression { expression } = &statement.statement_content else {
            panic!("Statement is not an Expression. Got [{}].", statement);
        };

        let ExpressionContent::FucntionLiteral { parameters, body: _ } = &expression.expression_content else {
            panic!("Expression is not a FunctionLiteral. Got [{}].", expression);
        };

        assert_eq!(parameters.len(), test.expected_params.len(), "Parameters has the wrong length.");

        for (i, ident) in parameters.iter().enumerate() {
            test_literal_expression(ident, &test.expected_params[i]);
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);".to_string();

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
    let StatementContent::Expression { expression } = &statement.statement_content else {
        panic!("statement is not an Expression. Got [{}].", statement);
    };

    let ExpressionContent::CallExpression { function, arguments } = &expression.expression_content else {
        panic!("expression is not an CallExpression. Got [{:?}].", expression.expression_content);
    };

    test_identifier(&function, &"add".to_string());

    assert_eq!(3, arguments.len(), "Wrong number of arguments");

    test_literal_expression(&arguments[0], &1);
    test_infix_expression(&arguments[1], &2, "*".to_string(), &3);
    test_infix_expression(&arguments[2], &4, "+".to_string(), &5);
}

#[test]
fn test_call_expression_parameter_parsing() {
    #[derive(Debug)]
    struct Test<'a> {
        input: &'a str,
        expected_ident: &'a str,
        expected_arguments: Vec<&'a str>,
    }
    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected_ident: &'a str, expected_arguments: Vec<&'a str>) -> Self {
            Test { input, expected_ident, expected_arguments }
        }
    }

    let tests = [
        Test::new("add();",
            "add",
            vec![]),
        Test::new("add(1);",
            "add",
            vec!["1"]),
        Test::new("add(1, 2 * 3, 4 + 5)",
            "add",
            vec!["1", "(2 * 3)", "(4 + 5)"]),
    ];

    for test in tests {

        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);

        let statement = &program.statements[0];
        let StatementContent::Expression { expression } = &statement.statement_content else {
            panic!("Statement is not an Expression. Got [{}].", statement);
        };

        let ExpressionContent::CallExpression { function, arguments } = &expression.expression_content else {
            panic!("Expression is not a CallExpression. Got [{}].", expression);
        };

        test_identifier(&function, &test.expected_ident.to_string());

        assert_eq!(arguments.len(), test.expected_arguments.len(), "Arguments has the wrong length.");

        for (i, arg) in arguments.iter().enumerate() {
            assert_eq!(arg.to_string(), test.expected_arguments[i].to_string(), "Mismatching argument.");
        }
    }
}
