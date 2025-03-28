use std::rc::Rc;

use eval::Evaluator;

mod ast;
mod token;
mod lexer;
mod parser;
mod eval;

fn main() {
    let text = r#"(\n. \m. \f. \x. n f (m f x)) (\f. \x. f (f x)) (\f. \x. f (f (f x)))"#;
    // let text = r#"(\f. \x. f (f (x x x))) (a b) (c d)"#;
    let expr = parser::Parser::new(lexer::Lexer::new(text)).parse_expr();
    println!("{:#?}", expr);
    let mut expr = Rc::new(expr.unwrap());
    println!("{}", expr);

    let mut eval = Evaluator::default();
    eval.eval_expr(&mut expr);
    println!("{}", expr);
}

