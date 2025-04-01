use std::borrow::Cow;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Decl { name: Cow<'a, str>, expr: Expr<'a> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<'a> {
    Var(Cow<'a, str>),
    App {
        func: Box<Expr<'a>>,
        arg: Box<Expr<'a>>,
    },
    Abs {
        param: Cow<'a, str>,
        body: Box<Expr<'a>>,
    },
}

impl Expr<'_> {
    /// Returns `true` if the expr is [`Abs`].
    ///
    /// [`Abs`]: Expr::Abs
    #[must_use]
    pub fn is_abs(&self) -> bool {
        matches!(self, Self::Abs { .. })
    }

    /// Returns `true` if the expr is [`Var`].
    ///
    /// [`Var`]: Expr::Var
    #[must_use]
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(..))
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::App { func, arg } => {
                if func.is_abs() {
                    write!(f, "({}) ", func)?;
                } else {
                    write!(f, "{} ", func)?;
                }

                if arg.is_var() {
                    write!(f, "{}", arg)
                } else {
                    write!(f, "({})", arg)
                }
            }

            Expr::Abs { param, body } => write!(f, "\\{}. {}", param, body),
        }
    }
}

pub fn parse(input: &str) -> Option<Stmt> {
    let (expr, input) = parse_stmt(input)?;
    let input = input.trim_end();
    if input.is_empty() {
        Some(expr)
    } else {
        None
    }
}

type ParseOutput<'a, T> = Option<(T, &'a str)>;

fn parse_stmt(input: &str) -> ParseOutput<Stmt> {
    parse_decl(input).or_else(|| parse_expr(input).map(|(expr, input)| (Stmt::Expr(expr), input)))
}

fn parse_decl(input: &str) -> ParseOutput<Stmt> {
    let (name, input) = parse_name(input)?;
    let name = name.into();
    let input = input.trim_start();
    let input = input.strip_prefix("=")?;
    let input = input.trim_start();
    let (expr, input) = parse_expr(input)?;
    Some((Stmt::Decl { name, expr }, input))
}

fn parse_expr(input: &str) -> ParseOutput<Expr> {
    let (mut expr, mut input) = parse_single(input)?;

    while let Some((next_expr, next_input)) = parse_single(input) {
        expr = Expr::App {
            func: Box::new(expr),
            arg: Box::new(next_expr),
        };
        input = next_input;
    }

    Some((expr, input))
}

fn parse_single(input: &str) -> ParseOutput<Expr> {
    let input = input.trim_start();
    parse_closed(input)
        .or_else(|| parse_abs(input))
        .or_else(|| parse_var(input))
}

fn parse_closed(input: &str) -> ParseOutput<Expr> {
    let input = input.strip_prefix("(")?;
    let input = input.trim_start();
    let (expr, input) = parse_expr(input)?;
    let input = input.trim_start();
    let input = input.strip_prefix(")")?;
    Some((expr, input))
}

fn parse_abs(input: &str) -> ParseOutput<Expr> {
    let input = input.strip_prefix("\\")?;
    let input = input.trim_start();

    // parse the rest of the parameters
    let mut params = Vec::new();
    let mut input = input.trim_start();
    while let Some((next_param, next_input)) = parse_name(input) {
        input = next_input.trim_start();
        params.push(next_param);
    }

    if params.is_empty() {
        return None
    }

    let input = input.strip_prefix(".")?;
    let input = input.trim_start();

    // create the Abs expression
    let (body, input) = parse_expr(input)?;
    let body = Box::new(body);
    let param = Cow::Borrowed(params.pop()?);
    let mut expr = Expr::Abs { param, body };

    // load the rest of the parameters and create multiple layers of Abs
    while let Some(param) = params.pop() {
        expr = Expr::Abs {
            param: Cow::Borrowed(param),
            body: Box::new(expr),
        };
    }

    Some((expr, input))
}

fn parse_var(input: &str) -> ParseOutput<Expr> {
    let (name, input) = parse_name(input)?;
    Some((Expr::Var(Cow::Borrowed(name)), input))
}

fn parse_name(input: &str) -> ParseOutput<&str> {
    let name_len = input
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '^')
        .unwrap_or(input.len());
    if name_len != 0 {
        Some(input.split_at(name_len))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn var() {
        let input = "abc  ";
        let res = parse_var(input);
        assert_eq!(res, Some((Expr::Var("abc".into()), "  ")))
    }

    #[test]
    fn abs() {
        let input = r#"\x. x  "#;
        let res = parse_abs(input);
        assert_eq!(
            res,
            Some((
                Expr::Abs {
                    param: "x".into(),
                    body: Box::new(Expr::Var("x".into()))
                },
                "  "
            ))
        )
    }

    #[test]
    fn closed() {
        let input = r#"(\x. 1) x "#;
        let res = parse_closed(input);
        assert_eq!(
            res,
            Some((
                Expr::Abs {
                    param: "x".into(),
                    body: Box::new(Expr::Var("1".into())),
                },
                " x "
            ))
        );
    }

    #[test]
    fn big_expr() {
        let input = r#"(\n. \f. \x. f (n f x)) (\f. \x. f (f x))"#;
        let res = parse(input);
        assert_eq!(
            res,
            Some(Stmt::Expr(Expr::App {
                func: Box::new(Expr::Abs {
                    param: "n".into(),
                    body: Box::new(Expr::Abs {
                        param: "f".into(),
                        body: Box::new(Expr::Abs {
                            param: "x".into(),
                            body: Box::new(Expr::App {
                                func: Box::new(Expr::Var("f".into())),
                                arg: Box::new(Expr::App {
                                    func: Box::new(Expr::App {
                                        func: Box::new(Expr::Var("n".into())),
                                        arg: Box::new(Expr::Var("f".into()))
                                    }),
                                    arg: Box::new(Expr::Var("x".into()))
                                })
                            })
                        })
                    })
                }),
                arg: Box::new(Expr::Abs {
                    param: "f".into(),
                    body: Box::new(Expr::Abs {
                        param: "x".into(),
                        body: Box::new(Expr::App {
                            func: Box::new(Expr::Var("f".into())),
                            arg: Box::new(Expr::App {
                                func: Box::new(Expr::Var("f".into())),
                                arg: Box::new(Expr::Var("x".into()))
                            })
                        })
                    })
                })
            })
        ));
    }

    #[test]
    fn expr_with_spacing() {
        let input = r#"(\  n  .   \  f  .   \  x . f (n f x)) (\f. \x. f (f x)) a b"#;
        let res = parse(input);
        assert_eq!(
            res,
            Some(Stmt::Expr(Expr::App {
                func: Box::new(Expr::App {
                    func: Box::new(Expr::App {
                        func: Box::new(Expr::Abs {
                            param: "n".into(),
                            body: Box::new(Expr::Abs {
                                param: "f".into(),
                                body: Box::new(Expr::Abs {
                                    param: "x".into(),
                                    body: Box::new(Expr::App {
                                        func: Box::new(Expr::Var("f".into())),
                                        arg: Box::new(Expr::App {
                                            func: Box::new(Expr::App {
                                                func: Box::new(Expr::Var("n".into())),
                                                arg: Box::new(Expr::Var("f".into()))
                                            }),
                                            arg: Box::new(Expr::Var("x".into()))
                                        })
                                    })
                                })
                            })
                        }),
                        arg: Box::new(Expr::Abs {
                            param: "f".into(),
                            body: Box::new(Expr::Abs {
                                param: "x".into(),
                                body: Box::new(Expr::App {
                                    func: Box::new(Expr::Var("f".into())),
                                    arg: Box::new(Expr::App {
                                        func: Box::new(Expr::Var("f".into())),
                                        arg: Box::new(Expr::Var("x".into()))
                                    })
                                })
                            })
                        })
                    }),
                    arg: Box::new(Expr::Var("a".into())),
                }),
                arg: Box::new(Expr::Var("b".into()))
            })
        ));
    }

    #[test]
    fn multiple_params() {
        let input1 = r#"\a. \b. \c. \d. a b c d"#;
        let input2 = r#"\a b c d. a b c d"#;
        assert_eq!(parse(input1).unwrap(), parse(input2).unwrap());
    }
}
