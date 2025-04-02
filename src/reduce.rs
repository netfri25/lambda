use std::collections::HashMap;
use std::rc::Rc;

use crate::parse::{Expr, Stmt};

#[derive(Debug, Clone)]
pub enum Node {
    Free { name: Rc<str> },
    Var { name: Rc<str> },
    App { func: Rc<Node>, arg: Rc<Node> },
    Abs { param: Rc<Node>, body: Rc<Node> },
}

#[derive(Default)]
pub struct Reducer {
    globals: HashMap<Box<str>, Rc<Node>>,
}

impl Reducer {
    // adds a statement, returning Rc<Node> when stmt is an expr, otherwise None
    pub fn add_stmt(&mut self, stmt: &Stmt) -> Option<Rc<Node>> {
        match stmt {
            Stmt::Expr(expr) => Some(self.add_expr(expr)),
            Stmt::Decl { name, expr } => {
                let node = self.add_expr(expr);
                self.globals.insert(name.as_ref().into(), node);
                None
            }
        }
    }

    // adds an expression, returning the Node
    pub fn add_expr(&self, expr: &Expr) -> Rc<Node> {
        Self::expr_to_node_rec(expr, &mut Default::default())
    }

    // creates an expression out of a node
    pub fn get_expr(&self, node: &Node) -> Option<Expr<'static>> {
        Self::get_expr_rec(node, &mut Default::default())
    }

    fn get_expr_rec<'a>(node: &'a Node, stack: &mut Vec<&'a Node>) -> Option<Expr<'static>> {
        Some(match node {
            Node::Free { name } => Expr::Var(name.as_ref().to_owned().into()),
            Node::Var { .. } => Expr::Var({
                let pos = stack
                    .iter()
                    .rev()
                    .position(|&node2| std::ptr::eq(node, node2))
                    .unwrap();
                format!("^{}", pos + 1).into()
            }),
            Node::App { func, arg } => {
                let func = Box::new(Self::get_expr_rec(func.as_ref(), stack)?);
                let arg = Box::new(Self::get_expr_rec(arg.as_ref(), stack)?);
                Expr::App { func, arg }
            }
            Node::Abs { param, body } => {
                stack.push(param.as_ref());
                let body = Box::new(Self::get_expr_rec(body.as_ref(), stack)?);
                stack.pop();
                let param = "_".into();
                Expr::Abs { param, body }
            }
        })
    }

    // applies reduction up to one step (does not mean one reduction)
    // if there's no reduction to be applied, the same `Rc<Node>` is returned
    pub fn reduce_once(&mut self, node: Rc<Node>) -> Rc<Node> {
        match node.as_ref() {
            Node::Free { ref name } => {
                if let Some(result) = self.globals.get(name.as_ref()) {
                    Self::deep_clone(result.clone())
                } else {
                    node
                }
            }
            Node::Var { .. } => node,
            Node::App { func, arg } => {
                if let Node::Abs { param, body } = func.as_ref() {
                    Self::replace(param.clone(), arg.clone(), body.clone())
                } else {
                    let new_func = self.reduce_once(func.clone());
                    if !std::ptr::eq(new_func.as_ref(), func.as_ref()) {
                        let func = new_func;
                        let arg = arg.clone();
                        return Rc::new(Node::App { func, arg });
                    }

                    let new_arg = self.reduce_once(arg.clone());
                    if !std::ptr::eq(new_arg.as_ref(), arg.as_ref()) {
                        let func = new_func;
                        let arg = new_arg;
                        Rc::new(Node::App { func, arg })
                    } else {
                        node
                    }
                }
            }
            Node::Abs { param, body } => {
                let new_body = self.reduce_once(body.clone());
                if !std::ptr::eq(body.as_ref(), new_body.as_ref()) {
                    let body = new_body;
                    let param = param.clone();
                    Rc::new(Node::Abs { param, body })
                } else {
                    node
                }
            }
        }
    }

    // fully reduces a node
    pub fn reduce_full(&mut self, mut node: Rc<Node>) -> Rc<Node> {
        loop {
            let reduced_node = self.reduce_once(node.clone());
            if std::ptr::eq(reduced_node.as_ref(), node.as_ref()) {
                return node;
            }

            node = reduced_node;
        }
    }

    fn deep_clone(node: Rc<Node>) -> Rc<Node> {
        match node.as_ref() {
            Node::Free { .. } => node,
            Node::Var { .. } => node,
            Node::App { func, arg } => {
                let func = Self::deep_clone(func.clone());
                let arg = Self::deep_clone(arg.clone());
                Rc::new(Node::App { func, arg })
            }
            Node::Abs { param, body } => {
                let Node::Var { name } = param.as_ref() else {
                    unreachable!("param should be a Node::Var");
                };

                let name = name.clone();
                let new_param = Rc::new(Node::Var { name });
                let body = Self::deep_clone(body.clone());
                let body = Self::replace(param.clone(), new_param.clone(), body);
                let param = new_param;
                Rc::new(Node::Abs { param, body })
            }
        }
    }

    // creates a clone of `node` and replaces all occurrences of `from` in `node` to `to` (recursively)
    fn replace(from: Rc<Node>, to: Rc<Node>, node: Rc<Node>) -> Rc<Node> {
        if std::ptr::eq(node.as_ref(), from.as_ref()) {
            return to;
        }

        match node.as_ref() {
            Node::Free { .. } => node,
            Node::Var { .. } => node,
            Node::App { func, arg } => {
                let func = Self::replace(from.clone(), to.clone(), func.clone());
                let arg = Self::replace(from, to, arg.clone());
                Rc::new(Node::App { func, arg })
            }
            Node::Abs { param, body } => {
                if std::ptr::eq(param.as_ref(), from.as_ref()) {
                    return node;
                }
                let body = Self::replace(from, to, body.clone());
                let param = param.clone();
                Rc::new(Node::Abs { param, body })
            }
        }
    }

    // converts an expression (from the Parser) to a Node
    fn expr_to_node_rec<'a>(
        expr: &'a Expr<'a>,
        scope: &mut HashMap<&'a str, Rc<Node>>,
    ) -> Rc<Node> {
        match expr {
            Expr::Var(name) => {
                if let Some(result) = scope.get(name.as_ref()) {
                    result.clone()
                } else {
                    let name = name.as_ref().into();
                    Rc::new(Node::Free { name })
                }
            }

            Expr::App { func, arg } => {
                let func = Self::expr_to_node_rec(func, scope);
                let arg = Self::expr_to_node_rec(arg, scope);
                Rc::new(Node::App { func, arg })
            }

            Expr::Abs { param, body } => {
                // create a param node
                let name = param.as_ref().into();
                let param_node = Rc::new(Node::Var { name });

                // insert the parameter with the node to the scope
                let old = scope.insert(param, param_node.clone());
                let body = Self::expr_to_node_rec(body, scope);

                // restore the old param, if exists
                if let Some(old) = old {
                    scope.insert(param, old);
                } else {
                    scope.remove(param.as_ref());
                }

                let param = param_node;
                Rc::new(Node::Abs { param, body })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    fn reduce(input: &str) -> String {
        let stmts: Box<[Stmt]> = input
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| parse(line).unwrap())
            .collect();
        let mut reducer = Reducer::default();
        if let Some(node) = stmts
            .iter()
            .map(|stmt| reducer.add_stmt(stmt))
            .last()
            .unwrap()
        {
            let node = reducer.reduce_full(node);
            reducer.get_expr(&node).unwrap().to_string()
        } else if let Stmt::Decl { name, .. } = stmts.last().unwrap() {
            let node = reducer.globals.get(name.as_ref()).unwrap();
            let node = reducer.reduce_full(node.clone());
            let expr = reducer.get_expr(&node).unwrap();
            format!("{} = {}", name, expr)
        } else {
            unreachable!("should be a statement")
        }
    }

    #[test]
    fn succ0() {
        let input = r#"(\n. \f. \x. f (n f x)) (\f. \x. x)"#;
        let output = r#"\_. \_. ^2 ^1"#;
        assert_eq!(reduce(input), output)
    }

    #[test]
    fn succ1() {
        let input = r#"(\n. \f. \x. f (n f x)) (\f. \x. f x)"#;
        let output = r#"\_. \_. ^2 (^2 ^1)"#;
        assert_eq!(reduce(input), output)
    }

    #[test]
    fn add_2_3() {
        let input = r#"(\n. \m. \f. \x. m f (n f x)) (\f. \x. f (f x)) (\f. \x. f (f (f x)))"#;
        let output = r#"\_. \_. ^2 (^2 (^2 (^2 (^2 ^1))))"#;
        assert_eq!(reduce(input), output)
    }

    #[test]
    fn fac_3() {
        let input = r#"(\n. \f. n (\f. \n. n (f (\f. \x. n f (f x)))) (\x. f) (\x. x)) (\f. \x. f (f (f x)))"#;
        let output = r#"\_. \_. ^2 (^2 (^2 (^2 (^2 (^2 ^1)))))"#;
        assert_eq!(reduce(input), output)
    }

    #[test]
    fn fib_6() {
        let input = r#"(\n. \f. n (\c. \a. \b. c b (\x. a (b x))) (\x. \y. x) (\x. x) f) (\f. \x. f (f (f (f (f (f x))))))"#;
        let output = r#"\_. \_. ^2 (^2 (^2 (^2 (^2 (^2 (^2 (^2 ^1)))))))"#;
        assert_eq!(reduce(input), output)
    }

    #[test]
    fn power_2_3() {
        let input = r#"(\b e. e b) (\f x. f (f x)) (\f x. f (f (f x)))"#;
        let output = r#"\_. \_. ^2 (^2 (^2 (^2 (^2 (^2 (^2 (^2 ^1)))))))"#;
        assert_eq!(reduce(input), output)
    }

    #[test]
    fn power_2_3_globals() {
        let input = r#"
0 = \f x. x
succ = \n f x. f (n f x)
1 = succ 0
2 = succ 1
3 = succ 2
power = \b e. e b
power 2 3
        "#;
        let output = r#"\_. \_. ^2 (^2 (^2 (^2 (^2 (^2 (^2 (^2 ^1)))))))"#;
        assert_eq!(reduce(input), output)
    }
}
