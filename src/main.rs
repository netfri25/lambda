use std::io::Write;

use eval::Evaluator;
use lexer::Lexer;
use parser::Parser;

// FIXME: correct scoping (I believe that this is the issue)

mod ast;
mod token;
mod lexer;
mod parser;
mod eval;

const START: &str = r#"
succ = \n. \f. \x. f (n f x);
0 = \f. \x. x;
1 = succ 0;
2 = succ 1;
3 = succ 2;
4 = succ 3;
5 = succ 4;
6 = succ 5;
7 = succ 6;
8 = succ 7;
9 = succ 8;
10 = succ 9;
add = \n. \m. \f. \x. n f (m f x);
add_succ = \n. \m. add (succ n) (succ m);
mul = \n. \m. \f. n (m f);
pred = \n. \f. \x. n (\g. \h. h (g f)) (\u. x) (\u. u);
"#;

fn main() {
    // let mut parser = Parser::new(Lexer::new(text));
    // let prog = parser.parse().unwrap();
    // for stmt in &prog {
    //     println!("{}", stmt);
    // }

    // let mut eval = Evaluator::default();
    // let Some(res) = eval.eval_stmts(prog) else { return };
    // println!("{}", res);
    repl();
}

fn repl() {
    let stdin = std::io::stdin();
    let mut history = START.to_owned();
    let mut line = String::new();

    loop {
        print!("> ");
        std::io::stdout().flush().ok();
        line.clear();
        stdin.read_line(&mut line).ok();

        // remove trailing whitespace
        while line.ends_with(char::is_whitespace) {
            line.pop();
        }

        // exit on an empty line
        if line.is_empty() {
            break;
        }

        // append a semicolon if missing
        if !line.ends_with(";") {
            line.push(';');
        }

        let old_len = history.len();
        history.reserve(line.len() + 1);
        history.push('\n');
        history.push_str(&line);

        let lexer = Lexer::new(&history);
        let mut parser = Parser::new(lexer);
        let stmts = match parser.parse() {
            Ok(parsed) => parsed,
            Err(err) => {
                eprintln!("PARSER ERROR: {}", err);
                history.drain(old_len..);
                continue;
            }
        };

        let mut eval = Evaluator::default();
        if let Some(res) = eval.eval_stmts(stmts) {
            // res contains the result of the expression in the last line, then we should rewind
            // the line if it's not a declaration
            history.drain(old_len..);
            println!("=> {}", res);
            println!()
        }
    }
}
