use std::io::{stdin, stdout};
use monkey_interpreter::lexer::Lexer;
use unindent::unindent;

use std::io::BufRead;
use std::io::BufReader;

const PROMPT: &str = ">> ";

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

pub fn start<I, T>(mut r#in: I, mut out: T)
where
    I: std::io::Read,
    T: std::io::Write,
{
    let reader = BufReader::new(&mut r#in);

    write!(out, "{PROMPT}").unwrap();
    out.flush().unwrap();

    reader.lines().for_each(|l| {
        l.map(|input| Lexer::new(input).for_each(|token| write!(out, "{token:?}\n").unwrap()))
            .unwrap();
        write!(out, "{PROMPT}").unwrap();
        out.flush().unwrap();
    });
}

