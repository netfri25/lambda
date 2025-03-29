#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub text: &'a str,
    pub kind: TokenKind,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(text: &'a str, kind: TokenKind, span: Span) -> Self {
        Self { text, kind, span }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Backslash,
    Dot,
    LParen,
    RParen,
    Semicolon,
    Equals,

    #[default]
    Eof,
    Illegal,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::Backslash => write!(f, "backslash"),
            TokenKind::Dot => write!(f, "dot"),
            TokenKind::LParen => write!(f, "left paren"),
            TokenKind::RParen => write!(f, "right paren"),
            TokenKind::Semicolon => write!(f, "semicolon"),
            TokenKind::Equals => write!(f, "equals"),
            TokenKind::Eof => write!(f, "end of file"),
            TokenKind::Illegal => write!(f, "illegal token"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    pub row: u16,
    pub col: u16,
}

impl Loc {
    pub fn new(row: u16, col: u16) -> Self {
        Self { row, col }
    }

    pub fn add_col(mut self, amount: u16) -> Self {
        self.col += amount;
        self
    }
}

impl Default for Loc {
    fn default() -> Self {
        Loc::new(1, 1)
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Self {
        Self { start, end }
    }
}
