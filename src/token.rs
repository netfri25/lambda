#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    text: &'a str,
    kind: TokenKind,
}

impl<'a> Token<'a> {
    pub fn new(text: &'a str, kind: TokenKind) -> Self {
        Self { text, kind }
    }

    pub fn text(&self) -> &'a str {
        self.text
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Backslash,
    Dot,
    LParen,
    RParen,

    Eof,
    Illegal,
}
