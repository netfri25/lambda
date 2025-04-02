use std::error::Error;
use std::hint::black_box;
use std::io::{self, BufRead, Write};
use std::thread;
use std::time::{Duration, Instant};

use reduce::Reducer;

mod parse;
mod reduce;

fn main() {
    let builder = thread::Builder::new()
        .name("repl".into())
        .stack_size(32 * 1024 * 1024); // 32 MB

    let handle = builder
        .spawn(|| {
            if let Err(err) = repl() {
                eprintln!("error: {}", err)
            }
        })
        .unwrap();

    handle.join().unwrap();
}

#[allow(unused)]
fn bench() {
    let seven = parse::parse(r#"7 = \f x. f (f (f (f (f (f (f x))))))"#).unwrap();
    let fac = parse::parse(r#"fac = \n f. n (\f n. n (f (\f x. n f (f x)))) (\x. f) (\x. x)"#).unwrap();
    let expr = parse::parse("fac 7").unwrap();
    loop {
        let mut reducer = Reducer::default();
        reducer.add_stmt(&seven);
        reducer.add_stmt(&fac);
        let node = reducer.add_stmt(&expr).unwrap();

        std::thread::sleep(Duration::from_millis(500));
        let start = Instant::now();
        let reduced = reducer.reduce_full(node);
        black_box(reducer.get_expr(&reduced).unwrap());
        let elapsed = start.elapsed();
        println!("{:?}", elapsed);
    }
}

const PRELUDE: &str = r#"
succ = \n f x. f (n f x)
0 = \f x. x
1 = succ 0
2 = succ 1
3 = succ 2
4 = succ 3
5 = succ 4
6 = succ 5
7 = succ 6
8 = succ 7
9 = succ 8
10 = succ 9
add = \m n f x. m f (n f x)
mul = \m n f. m (n f)
power = \m n. n m
fac = \n f. n (\f n. n (f (\f x. n f (f x)))) (\x. f) (\x. x)
fib = \n f. n (\c a b. c b (\x. a (b x))) (\x y. x) (\x. x) f
"#;

fn repl() -> Result<(), Box<dyn Error>> {
    let mut reducer = Reducer::default();

    for line in PRELUDE.lines().filter(|line| !line.trim().is_empty()) {
        let stmt = parse::parse(line).unwrap();
        reducer.add_stmt(&stmt);
    }

    let mut line = String::new();
    loop {
        print!("> ");
        io::stdout().flush()?;
        line.clear();
        io::stdin().lock().read_line(&mut line)?;
        if line.is_empty() {
            break;
        }

        let Some(stmt) = parse::parse(&line) else {
            eprintln!("parsing error");
            continue;
        };

        let Some(mut node) = reducer.add_stmt(&stmt) else {
            continue;
        };

        if cfg!(debug_assertions) {
            loop {
                let Some(expr) = reducer.get_expr(&node) else {
                    unreachable!("reducer should always be able to get the expression of a node that hasn't been removed yet.");
                };
                println!("==> {}", expr);
                io::stdin().lock().read_line(&mut String::new()).ok();

                let next_node = reducer.reduce_once(node.clone());
                if std::ptr::eq(next_node.as_ref(), node.as_ref()) {
                    break;
                }

                node = next_node;
            }
        } else {
            node = reducer.reduce_full(node);
        }

        let Some(expr) = reducer.get_expr(&node) else {
            unreachable!("reducer should always be able to get the expression of a node that hasn't been removed yet.");
        };
        println!("=> {}", expr);
    }

    Ok(())
}
