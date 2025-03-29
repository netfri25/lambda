use std::collections::HashMap;

use crate::ast::{Call, Expr, Lambda, Stmt, Var};

slotmap::new_key_type! { struct NodeID; }

#[derive(Debug, Clone, PartialEq, Eq)]
enum Node {
    Var(NodeID),
    Lambda { param: NodeID, body: NodeID },
    Call { func: NodeID, arg: NodeID },
}

#[derive(Debug, Clone, Default)]
pub struct Evaluator {
    decls: HashMap<Var, Expr>,
    nodes: slotmap::SlotMap<NodeID, Node>,
    names: slotmap::SparseSecondaryMap<NodeID, Var>,
    top_node: Option<NodeID>,
}

impl Evaluator {
    pub fn eval_stmts(&mut self, iter: impl IntoIterator<Item = Stmt>) -> Option<Expr> {
        iter.into_iter()
            .map(|stmt| self.eval_stmt(stmt))
            .last()
            .flatten()
    }

    pub fn eval_stmt(&mut self, stmt: Stmt) -> Option<Expr> {
        let node = self.stmt_to_node(stmt)?;
        // println!("expr: {}", self.node_to_expr(node));
        self.top_node = Some(node);
        self.eval_expr(node);
        Some(self.node_to_expr(node))
    }

    fn stmt_to_node(&mut self, stmt: Stmt) -> Option<NodeID> {
        let mut scope = HashMap::default();
        match stmt {
            Stmt::Decl(decl) => {
                let node = self.expr_to_node(decl.expr, &mut HashMap::default());
                self.eval_expr(node);
                let expr = self.node_to_expr(node);
                self.decls.insert(decl.name, expr);
                None
            }
            Stmt::Expr(expr) => Some(self.expr_to_node(expr, &mut scope)),
        }
    }

    fn expr_to_node(&mut self, expr: Expr, scope: &mut HashMap<Var, Vec<NodeID>>) -> NodeID {
        match expr {
            Expr::Var(var) => {
                if let Some(&id) = scope.get(&var).and_then(|ids| ids.last()) {
                    id
                } else if let Some(expr) = self.decls.get(&var) {
                    self.expr_to_node(expr.clone(), &mut HashMap::default())
                } else {
                    let id = self
                        .nodes
                        .try_insert_with_key(|id| Ok::<_, ()>(Node::Var(id)))
                        .expect("insertion should succeed");
                    self.names.insert(id, var);
                    id
                }
            }
            Expr::Lambda(lambda) => {
                // parameter variable node id
                let param = self
                    .nodes
                    .try_insert_with_key(|id| Ok::<_, ()>(Node::Var(id)))
                    .expect("insertion should succeed");

                scope.entry(lambda.param.clone()).or_default().push(param);

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
            Node::Var(var_id) => Expr::Var(self.names[var_id].clone()),
            Node::Lambda { param, body } => {
                let param_var = self.names[param].clone();
                let body_expr = self.node_to_expr(body);
                Expr::Lambda(Lambda::new(param_var, body_expr))
            }
            Node::Call { func, arg } => {
                let func_expr = self.node_to_expr(func);
                let arg_expr = self.node_to_expr(arg);
                Expr::Call(Call::new(func_expr, arg_expr))
            }
        }
    }

    fn eval_expr(&mut self, id: NodeID) {
        match self.nodes[id] {
            Node::Var(_) => (),
            Node::Lambda { body, .. } => self.eval_expr(body),
            Node::Call { func, arg } => {
                self.eval_expr(func); // should update func content, but the id should stay the same
                if let Node::Lambda { .. } = self.nodes[func] {
                    if let Some(top_node) = self.top_node {
                        println!("step: {}", self.node_to_expr(top_node));
                    }
                    let func = self.expr_to_node(self.node_to_expr(func), &mut HashMap::default());
                    let Node::Lambda { param, body } = self.nodes[func] else {
                        unreachable!();
                    };

                    self.nodes[param] = self.nodes[arg].clone();
                    self.nodes[id] = self.nodes[body].clone();

                    // NOTE: single stepping should not call `eval` again
                    self.eval_expr(id);
                } else {
                    self.eval_expr(arg);
                }
            }
        }
    }
}
