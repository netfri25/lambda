use crate::ast::{Call, Expr, Lambda, Var};
use crate::token::{Token, TokenKind};

pub struct Parser<'a, I> {
    iter: I,
    peek: Token<'a>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    pub fn new(iter: impl IntoIterator<IntoIter = I>) -> Self {
        let mut iter = iter.into_iter();
        let peek = iter
            .next()
            .unwrap_or_else(|| Token::new("", TokenKind::Eof));
        Self { iter, peek }
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        let mut expr = None;
        while !is_terminal(self.peek().kind()) {
            let next_expr = match self.peek().kind() {
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

        expr
    }

    fn parse_lambda(&mut self) -> Option<Lambda> {
        self.expect(TokenKind::Backslash)?;
        let param = self.parse_var()?;
        self.expect(TokenKind::Dot)?;
        let body = self.parse_expr()?;
        Some(Lambda::new(param, body))
    }

    fn parse_closed(&mut self) -> Option<Expr> {
        self.expect(TokenKind::LParen)?;
        let inner = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;
        Some(inner)
    }

    fn parse_var(&mut self) -> Option<Var> {
        let name = self.expect(TokenKind::Ident)?;
        Some(Var::new(name.text()))
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Token<'a>> {
        if self.peek.kind() == kind {
            Some(self.next_token())
        } else {
            None
        }
    }

    fn next_token(&mut self) -> Token<'a> {
        let after = self
            .iter
            .next()
            .unwrap_or_else(|| Token::new("", TokenKind::Eof));
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
