use crate::ast::{Call, Expr, Lambda, Var};
use crate::token::{Token, TokenKind};

pub type Result<T> = ::std::result::Result<T, Error>;

pub struct Parser<'a, I> {
    iter: I,
    peek: Token<'a>,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("expected {expected} but got {actual}")]
    Unexpected {
        expected: TokenKind,
        actual: TokenKind,
    },

    #[error("invalid expression")]
    InvalidExpr,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    pub fn new(iter: impl IntoIterator<IntoIter = I>) -> Self {
        let mut iter = iter.into_iter();
        let peek = iter.next().unwrap_or_default();
        Self { iter, peek }
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        let mut expr = None;
        while !is_terminal(self.peek().kind) {
            let next_expr = match self.peek().kind {
                TokenKind::Backslash => self.parse_lambda().map(Expr::Lambda)?,
                TokenKind::LParen => self.parse_closed()?,
                TokenKind::Ident => self.parse_var().map(Expr::Var)?,
                _ => break,
            };

            expr = if let Some(func) = expr {
                Some(Expr::Call(Call::new(func, next_expr)))
            } else {
                Some(next_expr)
            }
        }

        expr.ok_or(Error::InvalidExpr)
    }

    fn parse_lambda(&mut self) -> Result<Lambda> {
        self.expect(TokenKind::Backslash)?;
        let param = self.parse_var()?;
        self.expect(TokenKind::Dot)?;
        let body = self.parse_expr()?;
        Ok(Lambda::new(param, body))
    }

    fn parse_closed(&mut self) -> Result<Expr> {
        self.expect(TokenKind::LParen)?;
        let inner = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;
        Ok(inner)
    }

    fn parse_var(&mut self) -> Result<Var> {
        let name = self.expect(TokenKind::Ident)?;
        Ok(Var::new(name.text))
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>> {
        if self.peek.kind == kind {
            Ok(self.next_token())
        } else {
            Err(Error::Unexpected {
                expected: kind,
                actual: self.peek.kind,
            })
        }
    }

    fn next_token(&mut self) -> Token<'a> {
        let after = self.iter.next().unwrap_or_default();
        std::mem::replace(&mut self.peek, after)
    }

    fn peek(&self) -> &Token<'a> {
        &self.peek
    }
}

fn is_terminal(kind: TokenKind) -> bool {
    use TokenKind::*;
    matches!(kind, RParen)
}
