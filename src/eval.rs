use std::collections::HashMap;

use crate::ast::{Call, Expr, Lambda, Var};

#[derive(Debug, Clone)]
pub struct Evaluator {
    stack: Vec<Scope>,
}

impl Default for Evaluator {
    fn default() -> Self {
        let stack = vec![Scope::default()];
        Self { stack }
    }
}

impl Evaluator {
    pub fn eval_expr(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Var(var) => self.eval_var(var),
            Expr::Lambda(lambda) => self.eval_lambda(lambda),
            Expr::Call(call) => self.eval_call(call),
        }
    }

    fn eval_var(&mut self, var: Var) -> Expr {
        self.lookup(&var).cloned().unwrap_or(Expr::Var(var))
    }

    fn eval_lambda(&mut self, mut lambda: Lambda) -> Expr {
        *lambda.body = self.eval_expr(*lambda.body);
        Expr::Lambda(lambda)
    }

    fn eval_call(&mut self, mut call: Call) -> Expr {
        *call.func = self.eval_expr(*call.func);

        if let Expr::Lambda(lambda) = *call.func {
            self.push_scope();
            self.define(lambda.param, *call.arg);

            let result = self.eval_expr(*lambda.body);

            self.pop_scope();
            result
        } else {
            *call.arg = self.eval_expr(*call.arg);
            Expr::Call(call)
        }
    }

    fn push_scope(&mut self) {
        self.stack.push(Scope::default())
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn define(&mut self, var: Var, expr: impl Into<Expr>) {
        let expr = expr.into();
        self.stack.last_mut().unwrap().vars.insert(var, expr);
    }

    fn lookup(&self, var: &Var) -> Option<&Expr> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.vars.get(var))
    }
}

#[derive(Debug, Clone, Default)]
struct Scope {
    pub vars: HashMap<Var, Expr>,
}
