use crate::token::{Loc, Span, Token, TokenKind};

pub struct Lexer<'a> {
    text: &'a str,
    loc: Loc,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        let loc = Loc::new(1, 1);
        let text = text.trim_end();
        Self { text, loc }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        let tkn = self.lex_token();
        self.text = &self.text[tkn.text.len()..];
        self.loc = tkn.span.end;
        tkn
    }

    fn skip_whitespace(&mut self) {
        let mut count = 0;

        for c in self.text.chars().take_while(|c| c.is_whitespace()) {
            count += 1;

            if c == '\n' {
                self.loc.row += 1;
                self.loc.col = 1;
            } else {
                self.loc.col += 1;
            }
        }

        self.text = self.text.get(count..).unwrap_or_default()
    }

    fn lex_token(&self) -> Token<'a> {
        let start = self.loc;
        let end = self.loc.add_col(1);
        let span = Span::new(start, end);

        if let Some(kind) = self.lex_single() {
            return Token::new(self.text.get(..1).unwrap_or_default(), kind, span);
        }

        if let Some(tkn) = self.lex_ident() {
            return tkn;
        }

        Token::new(&self.text[..1], TokenKind::Illegal, span)
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

        let start = self.loc;
        let end = self.loc.add_col(len.try_into().unwrap());
        let span = Span::new(start, end);
        Some(Token::new(&self.text[..len], TokenKind::Ident, span))
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
        Some(self.next_token())
    }
}
