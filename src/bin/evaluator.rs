use monkey_interpreter::evaluator::eval;
use monkey_interpreter::lexer::Lexer;
use monkey_interpreter::parser::Parser;
use std::io::{stdin, stdout};
use unindent::unindent;

use std::io::BufRead;
use std::io::BufReader;

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = r#"           __,__
  .--.  .-"     "-.  .--.
 / .. \/  .-. .-.  \/ .. \ 
| |  '|  /   Y   \  |'  | |
| \   \  \ 0 | 0 /  /   / | 
 \ '- ,\.-"""""""-./, -' /
  ''-' /_   ^ ^   _\ '-''
      |  \._   _./  |
      \   \ '~' /   /
       '._ '-=-' _.'
          '-----'
"#;

fn main() {
    let welcome_string = format!(
        r"
    Hello {}! This is the Monkey programming language!
    Feel free to type in commands",
        whoami::username()
    );
    println!("{}", unindent(&welcome_string));
    start(stdin(), stdout());
}

fn start<I, T>(mut r#in: I, mut out: T)
where
    I: std::io::Read,
    T: std::io::Write,
{
    let mut reader = BufReader::new(&mut r#in);

    loop {
        write!(out, "{PROMPT}").unwrap();
        out.flush().unwrap();
        let mut input = String::new();
        reader.read_line(&mut input).unwrap();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().is_empty() == false {
            print_parser_errors(&mut out, parser.errors());
            continue;
        }

        let evaluated = eval(program);
        writeln!(out, "{}", evaluated.inspect()).unwrap();
    }
}

fn print_parser_errors<T>(out: &mut T, errors: &[String])
where
    T: std::io::Write,
{
    writeln!(out, "{}", MONKEY_FACE).unwrap();
    writeln!(out, "Woops! We ran into some monkey business here!").unwrap();
    writeln!(out, " parser errors:").unwrap();
    for msg in errors.iter() {
        writeln!(out, "\t{}", msg).unwrap();
    }
}
