use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    text: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self { text }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.text = self.text.trim_start();
        let tkn = self.lex_token();
        self.text = &self.text[tkn.text().len()..];
        tkn
    }

    fn lex_token(&self) -> Token<'a> {
        if let Some(kind) = self.lex_single() {
            return Token::new(self.text.get(..1).unwrap_or_default(), kind);
        }

        if let Some(tkn) = self.lex_ident() {
            return tkn;
        }

        Token::new(&self.text[..1], TokenKind::Illegal)
    }

    fn lex_ident(&self) -> Option<Token<'a>> {
        let mut chars = self.text.chars();
        if chars.next().is_none_or(|c| !c.is_alphabetic() && c != '_') {
            return None;
        }

        let len = chars
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .count();
        let len = 1 + len; // because of the first character

        Some(Token::new(&self.text[..len], TokenKind::Ident))
    }

    fn lex_single(&self) -> Option<TokenKind> {
        let Some(c) = self.text.chars().next() else {
            return Some(TokenKind::Eof);
        };

        Some(match c {
            '\\' => TokenKind::Backslash,
            '.' => TokenKind::Dot,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            _ => return None,
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let tkn = self.next_token();
        if tkn.kind() == TokenKind::Eof {
            return None
        }

        Some(tkn)
    }
}
