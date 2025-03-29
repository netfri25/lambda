use std::collections::VecDeque;

use crate::ast::{Call, Decl, Expr, Lambda, Stmt, Var};
use crate::token::{Token, TokenKind};

pub type Result<T> = ::std::result::Result<T, Error>;

pub struct Parser<'a, I> {
    iter: I,
    peek: VecDeque<Token<'a>>,
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
        let iter = iter.into_iter();
        let peek = Default::default();
        Self { iter, peek }
    }

    pub fn parse(&mut self) -> Result<Box<[Stmt]>> {
        let mut stmts = vec![];
        while self.peek(0).kind != TokenKind::Eof {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts.into_boxed_slice())
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let res = if self.peek(0).kind == TokenKind::Ident
        && self.peek(1).kind == TokenKind::Equals
        {
            self.parse_decl().map(Stmt::Decl)?
        } else {
            self.parse_expr().map(Stmt::Expr)?
        };

        if self.peek(0).kind == TokenKind::Semicolon {
            self.expect(TokenKind::Semicolon)?;
        }

        Ok(res)
    }

    fn parse_decl(&mut self) -> Result<Decl> {
        let name = self.parse_var()?;
        self.expect(TokenKind::Equals)?;
        let expr = self.parse_expr()?;
        Ok(Decl::new(name, expr))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        let mut expr = None;
        while !is_terminal(self.peek(0).kind) {
            let next_expr = match self.peek(0).kind {
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
        if self.peek(0).kind == kind {
            Ok(self.next_token())
        } else {
            Err(Error::Unexpected {
                expected: kind,
                actual: self.peek(0).kind,
            })
        }
    }

    fn next_token(&mut self) -> Token<'a> {
        self.peek(0); // makes sure the token exists in the peek list
        self.peek.pop_front().unwrap()
    }

    fn peek(&mut self, index: usize) -> &Token<'a> {
        if self.peek.len() <= index {
            self.peek
                .extend(self.iter.by_ref().take(index - self.peek.len() + 1));
        }

        debug_assert!(index < self.peek.len());
        &self.peek[index]
    }
}

fn is_terminal(kind: TokenKind) -> bool {
    use TokenKind::*;
    matches!(kind, RParen | Semicolon | Eof)
}
