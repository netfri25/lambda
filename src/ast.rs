use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Decl(Decl),
    Expr(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Decl(decl) => write!(f, "{};", decl),
            Stmt::Expr(expr) => write!(f, "{};", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
    pub name: Var,
    pub expr: Expr,
}

impl Decl {
    pub fn new(name: Var, expr: Expr) -> Self {
        Self { name, expr }
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.expr)
    }
}


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
    pub func: Box<Expr>,
    pub arg: Box<Expr>,
}

impl Call {
    pub fn new(func: Expr, arg: Expr) -> Self {
        let func = Box::new(func);
        let arg = Box::new(arg);
        Self { func, arg }
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.func.is_var() {
            write!(f, "{} ", self.func)?
        } else {
            write!(f, "({}) ", self.func)?
        }

        if self.arg.is_var() {
            write!(f, "{}", self.arg)
        } else {
            write!(f, "({})", self.arg)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub param: Var,
    pub body: Box<Expr>,
}

impl Lambda {
    pub fn new(param: Var, body: Expr) -> Self {
        let body = Box::new(body);
        Lambda { param, body }
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\{}. {}", self.param, self.body)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Var(Box<str>);

impl Var {
    pub fn new(arg: impl Into<Box<str>>) -> Self {
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
