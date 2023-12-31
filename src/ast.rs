use std::{
    fmt::Display,
    ops::Deref,
    sync::{Arc, Mutex},
};

use crate::{
    environment::Environment,
    object::{Object, BUILTINS},
    token::Token,
};

pub trait Node: Display {
    fn token_literal(&self) -> String;
    fn eval(&self, env: Arc<Mutex<Environment>>) -> Object;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
    token: Token,
    pub statement_content: StatementContent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    token: Token,
    pub expression_content: ExpressionContent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Node for Statement {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }

    fn eval(&self, env: Arc<Mutex<Environment>>) -> Object {
        match &self.statement_content {
            StatementContent::Let { name, value } => {
                let val = value.eval(Arc::clone(&env));
                if let Object::Error { message: _ } = val {
                    val
                } else {
                    if let ExpressionContent::Identifier { value } = &name.expression_content {
                        env.lock().unwrap().set(value.to_owned(), val)
                    } else {
                        Object::null()
                    }
                }
            }
            StatementContent::Return { return_value } => {
                let val = return_value
                    .as_ref()
                    .map_or_else(Object::null, |e| e.eval(env));
                if let Object::Error { message: _ } = val {
                    val
                } else {
                    Object::return_value(val)
                }
            }
            StatementContent::Expression { expression } => expression.eval(env),
            StatementContent::BlockStatement { statements } => {
                eval_block_statement(statements, env)
            }
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionContent {
    Identifier {
        value: String,
    },
    IntegerLiteral {
        value: i64,
    },
    StringLiteral {
        value: String,
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
    ArrayLiteral {
        elements: Vec<Box<Expression>>,
    },
    IndexExpression {
        left: Box<Expression>,
        index: Box<Expression>,
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

impl Node for Expression {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }

    fn eval(&self, env: Arc<Mutex<Environment>>) -> Object {
        match &self.expression_content {
            ExpressionContent::Identifier { value } => eval_identifier(value, env),
            ExpressionContent::IntegerLiteral { value } => Object::new_integer(*value),
            ExpressionContent::PrefixExpression { operator, right } => {
                eval_prefix_expression(operator, right, env)
            }
            ExpressionContent::InfixExpression {
                left,
                operator,
                right,
            } => eval_infix_expression(operator, left, right, env),
            ExpressionContent::Boolean { value } => Object::boolean(*value),
            ExpressionContent::IfExpression {
                condition,
                consequence,
                alternative,
            } => eval_if_expression(condition, consequence, alternative, env),
            ExpressionContent::FucntionLiteral { parameters, body } => Object::function(
                parameters
                    .iter()
                    .map(Box::deref)
                    .map(Expression::to_owned)
                    .collect(),
                env,
                body.deref().to_owned(),
            ),
            ExpressionContent::CallExpression {
                function,
                arguments,
            } => {
                let function = function.eval(Arc::clone(&env));
                if let Object::Error { .. } = function {
                    function
                } else {
                    let arguments = eval_expressions(arguments, env);
                    if arguments.len() == 1 {
                        if let Object::Error { .. } = arguments[0] {
                            return arguments[0].to_owned();
                        }
                    }

                    apply_function(function, arguments)
                }
            }
            ExpressionContent::StringLiteral { value } => Object::String {
                value: value.to_owned(),
            },
            ExpressionContent::ArrayLiteral { elements } => {
                let elements = eval_expressions(elements, env);
                if elements.len() == 1 {
                    if let Object::Error { .. } = elements[0] {
                        return elements[0].to_owned();
                    }
                }

                Object::Array {
                    elements: elements.into_iter().map(Box::new).collect(),
                }
            }
            ExpressionContent::IndexExpression { left, index } => {
                let left = left.eval(Arc::clone(&env));
                if let Object::Error { .. } = left {
                    return left;
                }

                let index = index.eval(Arc::clone(&env));
                if let Object::Error { .. } = index {
                    return index;
                }

                eval_index_expression(left, index)
            }
        }
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Array { elements }, Object::Integer { value }) =>
        eval_array_index_expression(elements, value),
        (l, _i) => Object::error(&format!("index operator not supported: {}", l.r#type())),
    }
}

fn eval_array_index_expression(elements: Vec<Box<Object>>, index: i64) -> Object {
    let index = index as usize;
    if !(0 .. elements.len()).contains(&index) {
        return Object::null()
    }

    *elements[index].to_owned()
}

fn apply_function(function: Object, arguments: Vec<Object>) -> Object {
    match function {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let extended_env = extend_function_env(parameters, env, arguments);
            let evaluated = body.eval(extended_env);
            unwrap_return_value(evaluated)
        }
        Object::Builtin { builtin_function } => builtin_function(arguments),
        _ => Object::error(&format!("not a function: {}", function.r#type())),
    }
}

fn unwrap_return_value(evaluated: Object) -> Object {
    if let Object::ReturnValue { value } = evaluated {
        return *value;
    }

    evaluated
}

fn extend_function_env(
    parameters: Vec<Expression>,
    env: Arc<Mutex<Environment>>,
    arguments: Vec<Object>,
) -> Arc<Mutex<Environment>> {
    let mut env = Environment::new_enclosed(env);

    for (i, param) in parameters.iter().enumerate() {
        let ExpressionContent::Identifier { value } = &param.expression_content else {
            panic!("expected Identifier, found [{}].", param);
        };
        env.set(value.to_owned(), arguments[i].to_owned());
    }

    Arc::new(Mutex::new(env))
}

fn eval_expressions(arguments: &Vec<Box<Expression>>, env: Arc<Mutex<Environment>>) -> Vec<Object> {
    let mut result = vec![];

    for b in arguments {
        let evaluated = b.eval(Arc::clone(&env));
        if let Object::Error { .. } = evaluated {
            return vec![evaluated];
        }
        result.push(evaluated);
    }

    return result;
}

fn eval_identifier(value: &String, env: Arc<Mutex<Environment>>) -> Object {
    if let Some(val) = env.lock().unwrap().get(value) {
        val
    } else if let Some(val) = BUILTINS.get(value as &str) {
        val.to_owned()
    } else {
        Object::error(&format!("identifier not found: {}", value))
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &Statement,
    alternative: &Option<Box<Statement>>,
    env: Arc<Mutex<Environment>>,
) -> Object {
    let condition = condition.eval(Arc::clone(&env));

    if let Object::Error { message: _ } = condition {
        return condition;
    }

    if is_truthy(condition) {
        consequence.eval(env)
    } else if let Some(alt) = alternative {
        alt.eval(env)
    } else {
        Object::null()
    }
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        Object::Integer { value } => value != 0,
        Object::Boolean { value } => value == true,
        Object::NUll => false,
        _ => false,
    }
}

fn eval_infix_expression(
    operator: &str,
    left: &Expression,
    right: &Expression,
    env: Arc<Mutex<Environment>>,
) -> Object {
    match (left.eval(Arc::clone(&env)), right.eval(Arc::clone(&env))) {
        (e @ Object::Error { message: _ }, _) | (_, e @ Object::Error { message: _ }) => e,
        (Object::Integer { value: l_val }, Object::Integer { value: r_val }) => {
            eval_integer_infix_expression(operator, l_val, r_val)
        }
        (Object::Boolean { value: l_val }, Object::Boolean { value: r_val })
            if operator == "==" =>
        {
            Object::boolean(l_val == r_val)
        }
        (Object::Boolean { value: l_val }, Object::Boolean { value: r_val })
            if operator == "!=" =>
        {
            Object::boolean(l_val != r_val)
        }
        (Object::String { value: l_val }, Object::String { value: r_val }) => {
            eval_string_infix_expression(operator, l_val, r_val)
        }
        (l, r) if l.r#type() != r.r#type() => Object::error(&format!(
            "type mismatch: {} {} {}",
            l.r#type(),
            operator,
            r.r#type()
        )),
        (l, r) => Object::error(&format!(
            "unknown operator: {} {} {}",
            l.r#type(),
            operator,
            r.r#type()
        )),
    }
}

fn eval_string_infix_expression(operator: &str, l_val: String, r_val: String) -> Object {
    if operator != "+" {
        return Object::error(&format!(
            "unknown operator: {} {} {}",
            "STRING", operator, "STRING"
        ));
    } else {
        Object::String {
            value: l_val + &r_val,
        }
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::new_integer(left + right),
        "-" => Object::new_integer(left - right),
        "*" => Object::new_integer(left * right),
        "/" => Object::new_integer(left / right),
        "<" => Object::boolean(left < right),
        ">" => Object::boolean(left > right),
        "==" => Object::boolean(left == right),
        "!=" => Object::boolean(left != right),
        _ => Object::error(&format!(
            "unknown operator: {} {} {}",
            "INTEGER", operator, "INTEGER"
        )),
    }
}

fn eval_prefix_expression(
    operator: &str,
    right: &Expression,
    env: Arc<Mutex<Environment>>,
) -> Object {
    let right = right.eval(env);

    if let Object::Error { message: _ } = right {
        return right;
    }

    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => Object::error(&format!("unknown operator: {}{}", operator, right.r#type())),
    }
}

fn eval_minus_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer { value } => Object::Integer { value: -value },
        _ => Object::error(&format!("unknown operator: -{}", right.r#type())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer { value } => Object::boolean(value == 0),
        Object::Boolean { value } => Object::boolean(!value),
        Object::NUll => Object::boolean(false),
        _ => Object::null(),
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression_content {
            ExpressionContent::Identifier { value } => write!(f, "{}", value),
            ExpressionContent::IntegerLiteral { value } => write!(f, "{}", value),
            ExpressionContent::StringLiteral { value } => write!(f, "{}", value),
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
            ExpressionContent::ArrayLiteral { elements } => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|b| b.deref().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExpressionContent::IndexExpression { left, index } => {
                write!(f, "({}[{}])", left, index)
            }
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

    fn eval(&self, env: Arc<Mutex<Environment>>) -> Object {
        let mut result = Object::NUll;

        for statement in &self.statements {
            result = statement.eval(Arc::clone(&env));

            match result {
                Object::ReturnValue { value } => return *value,
                Object::Error { message: _ } => return result,
                _ => (),
            }
        }

        result
    }
}

fn eval_block_statement(statements: &Vec<Box<Statement>>, env: Arc<Mutex<Environment>>) -> Object {
    let mut result = Object::NUll;

    for statement in statements {
        result = statement.eval(Arc::clone(&env));

        match result {
            Object::ReturnValue { value: _ } | Object::Error { message: _ } => return result,
            _ => (),
        }
    }

    result
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement).unwrap();
        }

        Ok(())
    }
}
