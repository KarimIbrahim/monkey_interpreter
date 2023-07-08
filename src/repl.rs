use std::io::BufRead;
use std::io::BufReader;

use crate::lexer::Lexer;

const PROMPT: &str = ">> ";

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
