use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(Var),
    Lambda(Lambda),
    Call(Call),
}

impl Expr {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{}", var),
            Self::Lambda(lambda) => write!(f, "{}", lambda),
            Self::Call(call) => write!(f, "{}", call),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    func: Rc<Expr>,
    arg: Rc<Expr>,
}

impl Call {
    pub fn new(func: Expr, arg: Expr) -> Self {
        let func = Rc::new(func);
        let arg = Rc::new(arg);
        Self { func, arg }
    }

    pub fn func(&self) -> &Expr {
        &self.func
    }

    pub fn func_mut(&mut self) -> &mut Rc<Expr> {
        &mut self.func
    }

    pub fn arg(&self) -> &Rc<Expr> {
        &self.arg
    }

    pub fn arg_mut(&mut self) -> &mut Rc<Expr> {
        &mut self.arg
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.func().is_var() {
            write!(f, "{} ", self.func())?
        } else {
            write!(f, "({}) ", self.func())?
        }

        if self.arg().is_var() {
            write!(f, "{}", self.arg())
        } else {
            write!(f, "({})", self.arg())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    param: Var,
    body: Rc<Expr>,
}

impl Lambda {
    pub fn new(param: Var, body: Expr) -> Self {
        let body = Rc::new(body);
        Lambda { param, body }
    }

    pub fn param(&self) -> &Var {
        &self.param
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Rc<Expr> {
        &mut self.body
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\{}. {}", self.param(), self.body())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Var(Rc<str>);

impl Var {
    pub fn new(arg: impl Into<Rc<str>>) -> Self {
        let arg = arg.into();
        Var(arg)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
