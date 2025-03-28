#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    text: &'a str,
    kind: TokenKind,
    span: Span,
}

impl<'a> Token<'a> {
    pub fn new(text: &'a str, kind: TokenKind, span: Span) -> Self {
        Self { text, kind, span }
    }

    pub fn text(&self) -> &'a str {
        self.text
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Backslash,
    Dot,
    LParen,
    RParen,

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

    pub fn next_row(mut self) -> Self {
        self.row += 1;
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
