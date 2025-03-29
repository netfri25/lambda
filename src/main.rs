use eval::Evaluator;
use lexer::Lexer;
use parser::Parser;

// FIXME: correct scoping (I believe that this is the issue)

mod ast;
mod token;
mod lexer;
mod parser;
mod eval;

fn main() {
    let text = r#"
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
mul = \n. \m. \f. n (m f);
pred = \n. \f. \x. n (\g. \h. h (g f)) (\u. x) (\u. u);
fac = \n. \f. n (\f. \n. n (f (\f. \x. n f (f x)))) (\x. f) (\x. x);
fac 2;
"#;
    // let text = r#"(\f. \x. f (f (x x x))) (a b) (c d)"#;
    let mut parser = Parser::new(Lexer::new(text));
    let prog = parser.parse().unwrap();
    for stmt in &prog {
        println!("{}", stmt);
    }

    let mut eval = Evaluator::default();
    let Some(res) = eval.eval_stmts(prog) else { return };
    println!("{}", res);
}

