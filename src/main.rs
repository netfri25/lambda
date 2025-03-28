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
    let mut expr = expr.unwrap();
    println!("{}", expr);

    let mut eval = Evaluator::default();
    expr = eval.eval_expr(expr);
    println!("{}", expr);
}

