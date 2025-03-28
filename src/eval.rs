use crate::ast::{Call, Expr, Lambda, Var};

#[derive(Debug, Clone, Default)]
pub struct Evaluator {
    stack: Vec<(Var, Expr)>,
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
        // TOOD: this is slow, because `cloned` can clone a very big expression
        self.lookup(&var).cloned().unwrap_or(Expr::Var(var))
    }

    fn eval_lambda(&mut self, mut lambda: Lambda) -> Expr {
        *lambda.body = self.eval_expr(*lambda.body);
        Expr::Lambda(lambda)
    }

    fn eval_call(&mut self, mut call: Call) -> Expr {
        *call.func = self.eval_expr(*call.func);

        if let Expr::Lambda(lambda) = *call.func {
            self.push_scope(lambda.param, *call.arg);
            let result = self.eval_expr(*lambda.body);
            self.pop_scope();
            result
        } else {
            *call.arg = self.eval_expr(*call.arg);
            Expr::Call(call)
        }
    }

    fn push_scope(&mut self, name: Var, value: Expr) {
        self.stack.push((name, value))
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn lookup(&self, var: &Var) -> Option<&Expr> {
        self.stack
            .iter()
            .rev()
            .find_map(|(name, expr)| (name == var).then_some(expr))
    }
}
