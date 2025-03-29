use std::collections::HashMap;

use crate::ast::{Call, Decl, Expr, Lambda, Stmt, Var};

slotmap::new_key_type! { struct NodeID; }

#[derive(Debug, Clone, PartialEq, Eq)]
enum Node {
    Var,
    Lambda {
        param: NodeID,
        body: NodeID,
    },
    Call {
        func: NodeID,
        arg: NodeID,
    },
}

#[derive(Debug, Clone, Default)]
pub struct Evaluator {
    nodes: slotmap::SlotMap<NodeID, Node>,
    names: slotmap::SparseSecondaryMap<NodeID, Var>,
}

impl Evaluator {
    pub fn eval_stmts(&mut self, iter: impl IntoIterator<Item = Stmt>) -> Option<Expr> {
        iter.into_iter()
            .map(|stmt| self.eval_stmt(stmt))
            .last()
            .flatten()
    }

    pub fn eval_stmt(&mut self, stmt: Stmt) -> Option<Expr> {
        let is_expr = matches!(stmt, Stmt::Expr(_));
        let node = self.stmt_to_node(stmt);
        is_expr.then(|| self.node_to_expr(node))
    }

    fn stmt_to_node(&mut self, stmt: Stmt) -> NodeID {
        let mut scope = HashMap::default();
        match stmt {
            Stmt::Decl(decl) => {
                // let node = self.expr_to_node(decl.expr, &mut scope);
                // self.globals.insert(decl.name, node);
                // node
                todo!("globals")
            }
            Stmt::Expr(expr) => self.expr_to_node(expr, &mut scope),
        }
    }

    fn expr_to_node(&mut self, expr: Expr, scope: &mut HashMap<Var, Vec<NodeID>>) -> NodeID {
        match expr {
            Expr::Var(var) => {
                if let Some(id) = scope.get(&var).and_then(|ids| ids.last()) {
                    *id
                } else {
                    let id = self.nodes.insert(Node::Var);
                    self.names.insert(id, var);
                    id
                }
            }
            Expr::Lambda(lambda) => {
                // parameter variable node id
                let param = self.nodes.insert(Node::Var);

                scope.entry(lambda.param.clone())
                    .or_default()
                    .push(param);

                let body = self.expr_to_node(*lambda.body, scope);

                // pop the variable from the scope
                let ids = scope.get_mut(&lambda.param).expect("entry must exist");
                ids.pop();
                if ids.is_empty() {
                    scope.remove_entry(&lambda.param);
                }

                self.names.insert(param, lambda.param);
                self.nodes.insert(Node::Lambda { param, body })
            }
            Expr::Call(call) => {
                let func = self.expr_to_node(*call.func, scope);
                let arg = self.expr_to_node(*call.arg, scope);
                self.nodes.insert(Node::Call { func, arg })
            }
        }
    }

    fn node_to_expr(&self, id: NodeID) -> Expr {
        match self.nodes[id] {
            Node::Var => Expr::Var(self.names[id].clone()),
            Node::Lambda { param, body } => {
                let param_var = self.names[param].clone();
                let body_expr = self.node_to_expr(body);
                Expr::Lambda(Lambda::new(param_var, body_expr))
            },
            Node::Call { func, arg } => {
                let func_expr = self.node_to_expr(func);
                let arg_expr = self.node_to_expr(arg);
                Expr::Call(Call::new(func_expr, arg_expr))
            }
        }
    }

    fn eval_decl(&mut self, decl: Decl) {
        todo!()
    }

    fn eval_expr(&mut self, expr: Expr) -> Expr {
        todo!()
    }

    fn eval_var(&mut self, var: Var) -> Expr {
        todo!()
    }

    fn eval_lambda(&mut self, lambda: Lambda) -> Expr {
        todo!()
    }

    fn eval_call(&mut self, call: Call) -> Expr {
        todo!()
    }
}
