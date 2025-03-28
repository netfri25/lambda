use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expr, Var};

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
    pub fn eval_expr(&mut self, expr: &mut Rc<Expr>) {
        match expr.as_ref() {
            Expr::Var(_) => self.eval_var(expr),
            Expr::Lambda(_) => self.eval_lambda(expr),
            Expr::Call(_) => self.eval_call(expr),
        }
    }

    fn eval_var(&mut self, expr: &mut Rc<Expr>) {
        let Expr::Var(var) = expr.as_ref() else {
            return;
        };

        if let Some(new_expr) = self.lookup(var) {
            *expr = new_expr;
        }
    }

    fn eval_lambda(&mut self, expr: &mut Rc<Expr>) {
        let Expr::Lambda(lambda) = Rc::make_mut(expr) else {
            return;
        };

        self.eval_expr(lambda.body_mut())
    }

    fn eval_call(&mut self, expr: &mut Rc<Expr>) {
        let Expr::Call(call) = Rc::make_mut(expr) else {
            return;
        };

        self.eval_expr(call.func_mut());
        let arg = call.arg().clone();
        if let Expr::Lambda(lambda) = Rc::make_mut(call.func_mut()) {
            self.push_scope();
            self.define(
                lambda.param().clone(),
                arg,
            );

            *expr = lambda.body_mut().clone();
            self.eval_expr(expr);

            self.pop_scope();
        } else {
            self.eval_expr(call.arg_mut());
        }
    }

    fn push_scope(&mut self) {
        self.stack.push(Scope::default())
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn define(&mut self, var: Var, expr: impl Into<Rc<Expr>>) {
        let expr = expr.into();
        self.stack.last_mut().unwrap().vars.insert(var, expr);
    }

    fn lookup(&self, var: &Var) -> Option<Rc<Expr>> {
        self.stack.last().unwrap().vars.get(var).cloned()
    }
}

#[derive(Debug, Clone, Default)]
struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: HashMap<Var, Rc<Expr>>,
}
