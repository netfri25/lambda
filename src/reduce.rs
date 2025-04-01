use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::parse::{Expr, Stmt};

slotmap::new_key_type! { pub struct NodeKey; }

#[derive(Debug, Clone)]
pub enum Node {
    Free { name: Rc<str> },
    Var { name: Rc<str> },
    App { func: NodeKey, arg: NodeKey },
    Abs { param: NodeKey, body: NodeKey },
}

#[derive(Default)]
pub struct Reducer {
    nodes: slotmap::SlotMap<NodeKey, Node>,
    globals: HashMap<Box<str>, NodeKey>,
}

impl Reducer {
    // adds a statement, returning NodeKey when stmt is an expr, otherwise None
    pub fn add_stmt(&mut self, stmt: &Stmt) -> Option<NodeKey> {
        match stmt {
            Stmt::Expr(expr) => Some(self.add_expr(expr)),
            Stmt::Decl { name, expr } => {
                let node_key = self.add_expr(expr);
                self.globals.insert(name.as_ref().into(), node_key);
                None
            }
        }
    }

    // adds an expression, returning it's NodeKey
    pub fn add_expr(&mut self, expr: &Expr) -> NodeKey {
        self.expr_to_node(expr, &mut Default::default())
    }

    // creates an expression out of the node_key
    pub fn get_expr(&self, node_key: NodeKey) -> Option<Expr<'static>> {
        self.get_expr_rec(node_key, &mut Default::default())
    }

    fn get_expr_rec(&self, node_key: NodeKey, stack: &mut Vec<NodeKey>) -> Option<Expr<'static>> {
        Some(match self.nodes.get(node_key)? {
            Node::Free { name } => Expr::Var(name.as_ref().to_owned().into()),
            Node::Var { .. } => Expr::Var({
                let pos = stack
                    .iter()
                    .rev()
                    .position(|key| *key == node_key)
                    .unwrap();
                format!("^{}", pos + 1).into()
            }),
            &Node::App { func, arg } => {
                let func = Box::new(self.get_expr_rec(func, stack)?);
                let arg = Box::new(self.get_expr_rec(arg, stack)?);
                Expr::App { func, arg }
            }
            &Node::Abs { param, body } => {
                stack.push(param);
                let body = Box::new(self.get_expr_rec(body, stack)?);
                stack.pop();
                let param = "_".into();
                Expr::Abs { param, body }
            }
        })
    }

    // applies reduction up to one step (does not mean one reduction)
    // if there's no reduction to be applied, the same `NodeKey` is returned
    pub fn reduce_once(&mut self, node_key: NodeKey) -> NodeKey {
        match self.nodes[node_key] {
            Node::Free { ref name } => {
                if let Some(&result) = self.globals.get(name.as_ref()) {
                    self.deep_clone(result)
                } else {
                    node_key
                }
            }
            Node::Var { .. } => node_key,
            Node::App { func, arg } => {
                if let Node::Abs { param, body } = self.nodes[func] {
                    self.replace(param, arg, body)
                } else {
                    let new_func = self.reduce_once(func);
                    if new_func != func {
                        let func = new_func;
                        return self.nodes.insert(Node::App { func, arg });
                    }

                    let new_arg = self.reduce_once(arg);
                    if new_arg != arg {
                        let func = new_func;
                        let arg = new_arg;
                        self.nodes.insert(Node::App { func, arg })
                    } else {
                        node_key
                    }
                }
            }
            Node::Abs { param, body } => {
                let new_body = self.reduce_once(body);
                if body != new_body {
                    let body = new_body;
                    self.nodes.insert(Node::Abs { param, body })
                } else {
                    node_key
                }
            }
        }
    }

    // fully reduces a ndoe
    pub fn reduce_full(&mut self, mut node_key: NodeKey) -> NodeKey {
        loop {
            let reduced_node_key = self.reduce_once(node_key);
            if reduced_node_key == node_key {
                return reduced_node_key;
            }

            node_key = reduced_node_key
        }
    }

    // deep clones a node
    fn deep_clone(&mut self, node_key: NodeKey) -> NodeKey {
        match self.nodes[node_key] {
            Node::Free { .. } => node_key,
            Node::Var { .. } => node_key,
            Node::App { func, arg } => {
                let func = self.deep_clone(func);
                let arg = self.deep_clone(arg);
                self.nodes.insert(Node::App { func, arg })
            }
            Node::Abs { param, body } => {
                let Node::Var { ref name } = self.nodes[param] else {
                    unreachable!("param is always Node::Var");
                };

                let name = format!("{}'", name).into();
                let new_param = self.nodes.insert(Node::Var { name });
                let body = self.deep_clone(body);
                let body = self.replace(param, new_param, body);
                let param = new_param;
                self.nodes.insert(Node::Abs { param, body })
            }
        }
    }

    // creates a clone of `node_key` and replaces all occurrences of `from` in `node_key` to `to` (recursively)
    fn replace(&mut self, from: NodeKey, to: NodeKey, node_key: NodeKey) -> NodeKey {
        if node_key == from {
            return to;
        }

        match self.nodes[node_key] {
            Node::Free { .. } => node_key,
            Node::Var { .. } => node_key,
            Node::App { func, arg } => {
                let func = self.replace(from, to, func);
                let arg = self.replace(from, to, arg);
                self.nodes.insert(Node::App { func, arg })
            }
            Node::Abs { param, body } => {
                if param == from {
                    return node_key;
                }
                let body = self.replace(from, to, body);
                self.nodes.insert(Node::Abs { param, body })
            }
        }
    }

    // removes all of the nodes that are not being used.
    // keeps every node that `to_keep` contains (recursively).
    // (may be possible to optimize by using ref counters, e.g. `Rc`-like))
    pub fn cleanup(&mut self, to_keep: impl IntoIterator<Item = NodeKey>) {
        let mut visited = HashSet::default();
        self.globals
            .values()
            .copied()
            .chain(to_keep)
            .for_each(|node| self.visit(node, &mut visited));
        let all_keys: HashSet<NodeKey> = self.nodes.keys().collect();
        let to_remove = all_keys.difference(&visited);
        for key in to_remove {
            self.nodes.remove(*key).expect("key should exist");
        }
    }

    // tries to visit all nodes, and collects them inside `keys`
    // this is useful for `self.cleanup`
    fn visit(&self, node_key: NodeKey, keys: &mut HashSet<NodeKey>) {
        keys.insert(node_key);
        match self.nodes[node_key] {
            Node::App { func, arg } => {
                self.visit(func, keys);
                self.visit(arg, keys);
            }
            Node::Abs { param, body } => {
                self.visit(param, keys);
                self.visit(body, keys);
            }
            _ => {}
        }
    }

    // converts an expression (from the Parser) to a Node
    fn expr_to_node<'a>(
        &mut self,
        expr: &'a Expr<'a>,
        scope: &mut HashMap<&'a str, NodeKey>,
    ) -> NodeKey {
        match expr {
            Expr::Var(name) => {
                if let Some(&result) = scope.get(name.as_ref()) {
                    result
                } else {
                    let name = name.as_ref().into();
                    self.nodes.insert(Node::Free { name })
                }
            }

            Expr::App { func, arg } => {
                let func = self.expr_to_node(func, scope);
                let arg = self.expr_to_node(arg, scope);
                self.nodes.insert(Node::App { func, arg })
            }

            Expr::Abs { param, body } => {
                // create a key to the parameter
                let name = param.as_ref().into();
                let key = self.nodes.insert(Node::Var { name });

                // insert the parameter with it's key to the scope
                let old = scope.insert(param, key);
                let body = self.expr_to_node(body, scope);

                // restore the old param, if exists
                if let Some(old) = old {
                    scope.insert(param, old);
                } else {
                    scope.remove(param.as_ref());
                }

                self.nodes.insert(Node::Abs { param: key, body })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    fn reduce(input: &str) -> String {
        let stmt = parse(input).unwrap();
        let mut reducer = Reducer::default();
        if let Some(node) = reducer.add_stmt(&stmt) {
            let node = reducer.reduce_full(node);
            reducer.get_expr(node).unwrap().to_string()
        } else if let Stmt::Decl { name, .. } = stmt {
            let node = reducer.globals.get(name.as_ref()).copied().unwrap();
            let node = reducer.reduce_full(node);
            let expr = reducer.get_expr(node).unwrap();
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
}
