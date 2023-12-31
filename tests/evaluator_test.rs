use monkey_interpreter::ast::{ExpressionContent, Node};
use monkey_interpreter::environment::Environment;
use monkey_interpreter::lexer::Lexer;
use monkey_interpreter::object::Object;
use monkey_interpreter::parser::Parser;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

#[test]
fn test_eval_integer_expression() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: i64) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("5", 5),
        Test::new("10", 10),
        Test::new("-5", -5),
        Test::new("-10", -10),
        Test::new("5 + 5 + 5 + 5 - 10", 10),
        Test::new("2 * 2 * 2 * 2 * 2", 32),
        Test::new("-50 + 100 + -50", 0),
        Test::new("5 * 2 + 10", 20),
        Test::new("5 + 2 * 10", 25),
        Test::new("20 + 2 * -10", 0),
        Test::new("50 / 2 * 2 + 10", 60),
        Test::new("2 * (5 + 10)", 30),
        Test::new("3 * 3 * 3 + 10", 37),
        Test::new("3 * (3 * 3) + 10", 37),
        Test::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for test in tests {
        let evaluated = test_eval(&test.input);
        test_integer_object(evaluated, test.expected);
    }
}

fn test_eval(input: &str) -> Object {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let env = Arc::new(Mutex::new(Environment::new()));

    program.eval(Arc::clone(&env))
}

fn test_integer_object(obj: Object, expected: i64) {
    let Object::Integer { value } = obj else {
        panic!("object is not Integer. Got [{:?}].", obj);
    };

    assert_eq!(value, expected, "object has wrong value.");
}

#[test]
fn test_eval_boolean_expression() {
    struct Test<'a> {
        input: &'a str,
        expected: bool,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: bool) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("true", true),
        Test::new("false", false),
        Test::new("1 < 2", true),
        Test::new("1 > 2", false),
        Test::new("1 < 1", false),
        Test::new("1 > 1", false),
        Test::new("1 == 1", true),
        Test::new("1 != 1", false),
        Test::new("1 == 2", false),
        Test::new("1 != 2", true),
        Test::new("true == true", true),
        Test::new("false == false", true),
        Test::new("true == false", false),
        Test::new("true != false", true),
        Test::new("false != true", true),
        Test::new("(1 < 2) == true", true),
        Test::new("(1 < 2) == false", false),
        Test::new("(1 > 2) == true", false),
        Test::new("(1 > 2) == false", true),
    ];

    for test in tests {
        let evaluated = test_eval(&test.input);
        test_boolean_object(evaluated, test.expected);
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    let Object::Boolean { value } = obj else {
        panic!("object is not Boolean. Got [{:?}].", obj);
    };

    assert_eq!(value, expected, "object has wrong value.");
}

#[test]
fn test_bang_operator() {
    struct Test<'a> {
        input: &'a str,
        expected: bool,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: bool) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("!true", false),
        Test::new("!false", true),
        Test::new("!5", false),
        Test::new("!!true", true),
        Test::new("!!false", false),
        Test::new("!!5", true),
        Test::new("! false", true),
    ];

    for test in tests {
        let evaluated = test_eval(&test.input);
        test_boolean_object(evaluated, test.expected);
    }
}

#[test]
fn test_if_else_expression() {
    struct Test<'a> {
        input: &'a str,
        expected: Object,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: Object) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("if (true) { 10 }", Object::Integer { value: 10 }),
        Test::new("if (false) { 10 }", Object::null()),
        Test::new("if (1) { 10 }", Object::Integer { value: 10 }),
        Test::new("if (1 < 2) { 10 }", Object::Integer { value: 10 }),
        Test::new("if (1 > 2) { 10 }", Object::null()),
        Test::new(
            "if (1 > 2) { 10 } else { 20 }",
            Object::Integer { value: 20 },
        ),
        Test::new(
            "if (1 < 2) { 10 } else { 20 }",
            Object::Integer { value: 10 },
        ),
    ];

    for test in tests {
        let evaluated = test_eval(&test.input);
        assert_eq!(evaluated, test.expected, "object does not match.");
    }
}

#[test]
fn test_return_statements() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: i64) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("return 10;", 10),
        Test::new("return 10; 9;", 10),
        Test::new("return 2 * 5; 9;", 10),
        Test::new("9; return 2 * 5; 9;", 10),
        Test::new(
            r"
        if (10 > 1) {
            if(10 > 1) {
                return 10;
            }

            return 1;
        }
        ",
            10,
        ),
    ];

    for test in tests {
        let evaluated = test_eval(&test.input);
        test_integer_object(evaluated, test.expected);
    }
}

#[test]
fn test_error_handling() {
    struct Test<'a> {
        input: &'a str,
        expected_message: &'a str,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected_message: &'a str) -> Self {
            Test {
                input,
                expected_message,
            }
        }
    }

    let tests = vec![
        Test::new("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        Test::new("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        Test::new("-true;", "unknown operator: -BOOLEAN"),
        Test::new("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        Test::new("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        Test::new(
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        Test::new(
            r"
        if (10 > 1) {
            if (10 > 1) {
                return true + false;
            }

            return 1;
        }
        ",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        Test::new("foobar", "identifier not found: foobar"),
        Test::new(r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
    ];

    for test in tests {
        let evaluated = test_eval(&test.input);

        let Object::Error { message } = evaluated else {
            panic!("no error object returned. Got [{:?}].", evaluated);
        };

        assert_eq!(test.expected_message, message, "wrong error message.");
    }
}

#[test]
fn test_let_statements() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: i64) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("let a = 5; a;", 5),
        Test::new("let a = 5 * 5; a;", 25),
        Test::new("let a = 5; let b = a; b;", 5),
        Test::new("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for test in tests {
        test_integer_object(test_eval(test.input), test.expected);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input);

    let Object::Function { parameters, body, env: _ } = evaluated else {
        panic!("object is not a Function. Got [{:?}].", evaluated);
    };

    assert_eq!(1, parameters.len(), "function has wrong parameters.");

    let ExpressionContent::Identifier { value } = &parameters[0].expression_content else {
        panic!("parameter is not an Identifier. Got [{}].", parameters[0]);
    };
    assert_eq!("x", value, "parameter does not match.");

    assert_eq!("(x + 2)", body.to_string(), "body does not match.");
}

#[test]
fn test_function_application() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: i64) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("let identity = fn(x) { x;}; identity(5);", 5),
        Test::new("let identity = fn(x) { return x; }; identity(5);", 5),
        Test::new("let double = fn(x) { x * 2; }; double(5);", 10),
        Test::new("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        Test::new("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        Test::new("fn(x) { x; }(5)", 5),
    ];

    for test in tests {
        test_integer_object(test_eval(test.input), test.expected);
    }
}

#[test]
fn test_closures() {
    let input = r"
    let newAdder = fn(x) {
        fn(y) { x + y };
    };

    let addTwo = newAdder(2);
    addTwo(2);
    ";

    test_integer_object(test_eval(input), 4);
}

#[test]
fn test_string_literal() {
    let input = r#""Hello World!""#;

    let evaluated = test_eval(input);

    let Object::String { value } = evaluated else {
        panic!("object is not String. Got [{:?}].", evaluated);
    };

    assert_eq!("Hello World!", value, "String has wrong value.");
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;

    let evaluated = test_eval(input);

    let Object::String { value } = evaluated else {
        panic!("object is not String. Got [{:?}].", evaluated);
    };

    assert_eq!("Hello World!", value, "String has wrong value.");
}

#[test]
fn test_builtin_functions() {
    struct Test<'a> {
        input: &'a str,
        expected: Object,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: Object) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new(r#"len(" ")"#, Object::Integer { value: 1 }),
        Test::new(r#"len("four")"#, Object::Integer { value: 4 }),
        Test::new(r#"len("hello world")"#, Object::Integer { value: 11 }),
        Test::new(
            r#"len(1)"#,
            Object::error("argument to `len` not supported, got INTEGER"),
        ),
        Test::new(
            r#"len("one", "two")"#,
            Object::error("wrong number of arguments. got=2, want=1"),
        ),
    ];

    for test in tests {
        let evaluated = test_eval(test.input);

        match test.expected {
            Object::Integer { value } => test_integer_object(evaluated, value),
            Object::Error { message } => {
                let Object::Error { message: evaluated_message } = evaluated else {
                    panic!("object is not Error. Got [{:?}].", evaluated);
                };

                assert_eq!(message, evaluated_message, "wrong error message.");
            }
            _ => panic!("unexpected value: [{:?}].", test.expected),
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let evaluated = test_eval(input);

    let Object::Array { elements } = evaluated else {
        panic!("object is not an Array. Got [{:?}].", evaluated);
    };

    assert_eq!(3, elements.len(), "array has wrong number of elements.");

    test_integer_object(elements[0].deref().to_owned(), 1);
    test_integer_object(elements[1].deref().to_owned(), 4);
    test_integer_object(elements[2].deref().to_owned(), 6);
}

#[test]
fn test_array_index_expressions() {
    struct Test<'a> {
        input: &'a str,
        expected: Object,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, expected: Object) -> Self {
            Test { input, expected }
        }
    }

    let tests = vec![
        Test::new("[1, 2, 3][0]", Object::Integer { value: 1 }),
        Test::new("[1, 2, 3][1]", Object::Integer { value: 2 }),
        Test::new("[1, 2, 3][2]", Object::Integer { value: 3 }),
        Test::new("let i = 0; [1][i];", Object::Integer { value: 1 }),
        Test::new("[1, 2, 3][1 + 1];", Object::Integer { value: 3 }),
        Test::new(
            "let myArray = [1, 2, 3]; myArray[2];",
            Object::Integer { value: 3 },
        ),
        Test::new(
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Object::Integer { value: 6 },
        ),
        Test::new(
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            Object::Integer { value: 2 },
        ),
        Test::new("[1, 2, 3][3]", Object::null()),
        Test::new("[1, 2, 3][-1]", Object::null()),
    ];

    for test in tests {
        let evaluated = test_eval(test.input);

        match test.expected {
            Object::Integer { value } => test_integer_object(evaluated, value),
            e => assert_eq!(evaluated, e, "evaluated expressions mismatches."),
        }
    }
}
