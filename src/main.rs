use std::io::{stdin, stdout};
use unindent::unindent;

use monkey_interpreter::repl;

fn main() {
    let welcome_string = format!(
        r"
    Hello {}! This is the Monkey programming language!
    Feel free to type in commands",
        whoami::username()
    );
    println!("{}", unindent(&welcome_string));
    repl::start(stdin(), stdout());
}
