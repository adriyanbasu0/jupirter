use logos::Logos;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
#[logos(skip r"//[^\n]*\n")]
#[logos(skip r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/")]
pub enum TokenKind {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("var")]
    Var,
    #[token("syscall")]
    Syscall,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("struct")]
    Struct,
    #[token("union")]
    Union,
    #[token("enum")]
    Enum,
    #[token("sizeof")]
    Sizeof,
    #[token("alignof")]
    Alignof,
    #[token("offsetof")]
    Offsetof,
    #[token("asm")]
    Asm,
    #[token("noreturn")]
    Noreturn,
    #[token("pub")]
    Pub,
    #[token("priv")]
    Priv,
    #[token("as")]
    As,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("defer")]
    Defer,
    #[token("alloc")]
    Alloc,
    #[token("free")]
    Free,
    #[token("capability")]
    Capability,
    #[token("topology")]
    Topology,
    #[token("bitregion")]
    BitRegion,
    #[token("entropy")]
    Entropy,
    #[token("paddr")]
    PAddr,
    #[token("numa")]
    Numa,
    #[token("bits")]
    Bits,
    #[token("uninitialized")]
    Uninitialized,
    #[token("initialized")]
    Initialized,
    #[token("tainted")]
    Tainted,
    #[token("KiB")]
    KiB,
    #[token("MiB")]
    MiB,
    #[token("GiB")]
    GiB,
    #[token("TiB")]
    TiB,
    #[token("PiB")]
    PiB,
    #[token("memory")]
    Memory,
    #[token("cache")]
    Cache,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex(r"[0-9][0-9_]*", priority = 1)]
    Integer,
    #[regex(r"0x[0-9a-fA-F_]+")]
    HexInteger,
    #[regex(r"0o[0-7_]+")]
    OctInteger,
    #[regex(r"0b[01_]+")]
    BinInteger,
    #[regex(r#""([^"]|\\.)*""#)]
    String,
    #[regex(r"'([^\\']|\\.)'")]
    Char,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("%=")]
    PercentEq,
    #[token("<<=")]
    LShiftEq,
    #[token(">>=")]
    RShiftEq,
    #[token("&=")]
    AndEq,
    #[token("|=")]
    OrEq,
    #[token("^=")]
    XorEq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("!")]
    Not,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("@")]
    At,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("=")]
    Eq,
    #[token("i8")]
    I8,
    #[token("i16")]
    I16,
    #[token("i32")]
    I32,
    #[token("i64")]
    I64,
    #[token("u8")]
    U8,
    #[token("u16")]
    U16,
    #[token("u32")]
    U32,
    #[token("u64")]
    U64,
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,
    #[token("usize")]
    Usize,
    #[token("isize")]
    Isize,
    #[token("bool")]
    Bool,
    #[token("void")]
    Void,

    Error,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::Syscall => write!(f, "syscall"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::Union => write!(f, "union"),
            TokenKind::Enum => write!(f, "enum"),
            TokenKind::Sizeof => write!(f, "sizeof"),
            TokenKind::Alignof => write!(f, "alignof"),
            TokenKind::Offsetof => write!(f, "offsetof"),
            TokenKind::Asm => write!(f, "asm"),
            TokenKind::Noreturn => write!(f, "noreturn"),
            TokenKind::Pub => write!(f, "pub"),
            TokenKind::Priv => write!(f, "priv"),
            TokenKind::As => write!(f, "as"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Defer => write!(f, "defer"),
            TokenKind::Alloc => write!(f, "alloc"),
            TokenKind::Free => write!(f, "free"),
            TokenKind::Capability => write!(f, "capability"),
            TokenKind::Topology => write!(f, "topology"),
            TokenKind::BitRegion => write!(f, "bitregion"),
            TokenKind::Entropy => write!(f, "entropy"),
            TokenKind::PAddr => write!(f, "paddr"),
            TokenKind::Numa => write!(f, "numa"),
            TokenKind::Bits => write!(f, "bits"),
            TokenKind::Uninitialized => write!(f, "uninitialized"),
            TokenKind::Initialized => write!(f, "initialized"),
            TokenKind::Tainted => write!(f, "tainted"),
            TokenKind::KiB => write!(f, "KiB"),
            TokenKind::MiB => write!(f, "MiB"),
            TokenKind::GiB => write!(f, "GiB"),
            TokenKind::TiB => write!(f, "TiB"),
            TokenKind::PiB => write!(f, "PiB"),
            TokenKind::Memory => write!(f, "memory"),
            TokenKind::Cache => write!(f, "cache"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Integer => write!(f, "integer"),
            TokenKind::HexInteger => write!(f, "hex integer"),
            TokenKind::OctInteger => write!(f, "octal integer"),
            TokenKind::BinInteger => write!(f, "binary integer"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Char => write!(f, "character"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::LShift => write!(f, "<<"),
            TokenKind::RShift => write!(f, ">>"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::PlusEq => write!(f, "+="),
            TokenKind::MinusEq => write!(f, "-="),
            TokenKind::StarEq => write!(f, "*="),
            TokenKind::SlashEq => write!(f, "/="),
            TokenKind::PercentEq => write!(f, "%="),
            TokenKind::LShiftEq => write!(f, "<<="),
            TokenKind::RShiftEq => write!(f, ">>="),
            TokenKind::AndEq => write!(f, "&="),
            TokenKind::OrEq => write!(f, "|="),
            TokenKind::XorEq => write!(f, "^="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::NotEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::AndAnd => write!(f, "&&"),
            TokenKind::OrOr => write!(f, "||"),
            TokenKind::Not => write!(f, "!"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::At => write!(f, "@"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::I8 => write!(f, "i8"),
            TokenKind::I16 => write!(f, "i16"),
            TokenKind::I32 => write!(f, "i32"),
            TokenKind::I64 => write!(f, "i64"),
            TokenKind::U8 => write!(f, "u8"),
            TokenKind::U16 => write!(f, "u16"),
            TokenKind::U32 => write!(f, "u32"),
            TokenKind::U64 => write!(f, "u64"),
            TokenKind::F32 => write!(f, "f32"),
            TokenKind::F64 => write!(f, "f64"),
            TokenKind::Usize => write!(f, "usize"),
            TokenKind::Isize => write!(f, "isize"),
            TokenKind::Bool => write!(f, "bool"),
            TokenKind::Void => write!(f, "void"),
            TokenKind::Error => write!(f, "error"),
        }
    }
}

impl TokenKind {
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            TokenKind::I8
                | TokenKind::I16
                | TokenKind::I32
                | TokenKind::I64
                | TokenKind::U8
                | TokenKind::U16
                | TokenKind::U32
                | TokenKind::U64
                | TokenKind::F32
                | TokenKind::F64
                | TokenKind::Usize
                | TokenKind::Isize
                | TokenKind::Bool
                | TokenKind::Void
        )
    }
}

pub fn lex(source: &str) -> Result<Vec<Token>, Vec<()>> {
    let mut lexer = TokenKind::lexer(source);
    let mut tokens = Vec::new();
    let mut line = 1;
    let mut column = 1;

    while let Some(result) = lexer.next() {
        match result {
            Ok(kind) => {
                let text = lexer.slice().to_string();
                let span = Span {
                    start: lexer.span().start,
                    end: lexer.span().end,
                    line,
                    column,
                };
                tokens.push(Token { kind, text, span });
                column += lexer.slice().len();
            }
            Err(()) => {
                return Err(vec![]);
            }
        }

        if lexer.slice().contains('\n') {
            line += 1;
            column = 1;
        }
    }

    Ok(tokens)
}
