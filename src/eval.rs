use std::collections::HashMap;

use crate::ast::{Call, Decl, Expr, Lambda, Stmt, Var};

#[derive(Debug, Clone, Default)]
pub struct Evaluator {
    globals: HashMap<Var, Expr>,
}

impl Evaluator {
    pub fn eval_stmts(&mut self, iter: impl IntoIterator<Item = Stmt>) -> Option<Expr> {
        iter.into_iter()
            .map(|stmt| self.eval_stmt(stmt))
            .last()
            .flatten()
    }

    pub fn eval_stmt(&mut self, stmt: Stmt) -> Option<Expr> {
        match stmt {
            Stmt::Decl(decl) => {
                self.eval_decl(decl);
                None
            }
            Stmt::Expr(expr) => Some(self.eval_expr(expr)),
        }
    }

    fn eval_decl(&mut self, decl: Decl) {
        // let expr = self.eval_expr(decl.expr);
        self.globals.insert(decl.name, decl.expr);
    }

    fn eval_expr(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Var(var) => self.eval_var(var),
            Expr::Lambda(lambda) => self.eval_lambda(lambda),
            Expr::Call(call) => self.eval_call(call),
        }
    }

    fn eval_var(&mut self, var: Var) -> Expr {
        // TOOD: this can be slow because of cloning
        if let Some(expr) = self.globals.get(&var).cloned() {
            self.eval_expr(expr)
        } else {
            Expr::Var(var)
        }
    }

    fn eval_lambda(&mut self, mut lambda: Lambda) -> Expr {
        *lambda.body = self.eval_expr(*lambda.body);
        Expr::Lambda(lambda)
    }

    fn eval_call(&mut self, mut call: Call) -> Expr {
        *call.func = self.eval_expr(*call.func);

        if let Expr::Lambda(lambda) = *call.func {
            let expr = Self::replace_var(*lambda.body, lambda.param, *call.arg);
            self.eval_expr(expr)
        } else {
            *call.arg = self.eval_expr(*call.arg);
            Expr::Call(call)
        }
    }

    fn replace_var(expr: Expr, name: Var, value: Expr) -> Expr {
        match expr {
            Expr::Var(var) if var == name => value,
            Expr::Lambda(mut lambda) if lambda.param != name => {
                *lambda.body = Self::replace_var(*lambda.body, name, value);
                Expr::Lambda(lambda)
            }
            Expr::Call(mut call) => {
                // TOOD: this can be slow because of cloning
                *call.func = Self::replace_var(*call.func, name.clone(), value.clone());
                *call.arg = Self::replace_var(*call.arg, name, value);
                Expr::Call(call)
            }
            _ => expr,
        }
    }
}
