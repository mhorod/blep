use std::fmt::Debug;
use std::hash::Hash;

use crate::lexing::Categories;
use crate::lexing::LexerSymbol;
use crate::lexing::LexerSymbol::*;
use crate::regex::{Regex, Regex::*};

macro_rules! char {
    ($c:expr) => {
        Atom(Concrete($c))
    };
}

macro_rules! token_categories {
    ($($category:expr => $regex:expr),*) => {
           vec![
                $($crate::lexing::Category { category: $category, regex: $regex } ),*
            ]
    };
}

macro_rules! keywords {
    ($($name:ident => $s:expr),*) => {
        vec![
            $($crate::lexing::Category { category: Keyword($name), regex: string_atom($s) } ),*
        ]
    };
}

pub fn blep_token_categories() -> Categories<TokenCategory> {
    [blep_keywords(), blep_symbols(), blep_atoms()]
        .into_iter()
        .flat_map(|x| x.into_iter())
        .collect()
}

fn blep_atoms() -> Categories<TokenCategory> {
    use LiteralKind::*;
    use TokenCategory::*;

    token_categories!(
        TypeName => Atom(Uppercase) + ((Atom(Lowercase) | Atom(Uppercase) | Atom(Digit)).star()),
        Identifier => (Atom(Lowercase) | char!('_'))
        + (char!('_') | Atom(Lowercase) | Atom(Uppercase) | Atom(Digit)).star(),
        Whitespace => (char!(' ') | char!('\t') | char!('\n')).star(),
        Comment => string_atom("//") + Atom(Not('\n')).star(),
        Literal(IntLiteral) => Atom(Digit) + Atom(Digit).star(),
        Literal(FloatLiteral) => Atom(Digit) + Atom(Digit).star() + char!('.') + Atom(Digit).plus(),
        Literal(BoolLiteral) => string_atom("True") | string_atom("False"),
        Literal(StringLiteral) => char!('"') + Atom(Not('"')).star() + char!('"')
    )
}

fn blep_keywords() -> Categories<TokenCategory> {
    use KeywordKind::*;
    use TokenCategory::*;
    keywords!(
        Let => "let",
        Pure => "pure",
        Static => "static",
        Fun => "fun",
        Enum => "enum",
        Public => "public",
        Private => "private",
        Internal => "internal",
        Return => "return",
        If => "if",
        Else => "else",

        // loop keywords
        For => "for",
        While => "while",
        Loop => "loop",
        Do => "do",
        In => "in",
        Break => "break",
        Continue => "continue",

        // module keywords
        Module => "module",
        Import => "import",
        Qualified => "qualified",
        As => "as",

        // mutability keywords
        Mut => "mut",
        MutRef => "&mut",
        MutPtr => "*mut",

        // type keywords
        NewType => "newtype",
        TypeAlias => "typealias",

        // Data structure keywords
        Struct => "struct",
        Class => "class",
        Interface => "interface",
        Implements => "implements"
    )
}

fn blep_symbols() -> Categories<TokenCategory> {
    use DelimKind::*;
    use SymbolKind::*;
    use TokenCategory::*;

    token_categories!(
        // delimiters
        OpenDelim(Paren) => char!('('),
        CloseDelim(Paren) => char!(')'),
        OpenDelim(Bracket) => char!('['),
        CloseDelim(Bracket) => char!(']'),
        OpenDelim(Brace) => char!('{'),
        CloseDelim(Brace) => char!('}'),

        // symbols
        Symbol(Eq) => char!('='),
        Symbol(Semicolon) => char!(';'),
        Symbol(Colon) => char!(':'),
        Symbol(DoubleColon) => string_atom("::"),
        Symbol(Comma) => char!(','),
        Symbol(Dot) => char!('.'),
        Symbol(DotDot) => string_atom(".."),
        Symbol(DotDotEq) => string_atom("..="),

        // operators
        Symbol(Arrow) => string_atom("->"),
        Symbol(Ampersand) => char!('&'),
        Symbol(Asterisk) => char!('*'),
        Symbol(Slash) => char!('/'),
        Symbol(Percent) => char!('%'),
        Symbol(Plus) => char!('+'),
        Symbol(Minus) => char!('-'),
        Symbol(Bang) => char!('!'),
        Symbol(Tilde) => char!('~'),

        Symbol(Lt) => char!('<'),
        Symbol(Leq) => string_atom("<="),
        Symbol(Gt) => char!('>'),
        Symbol(Geq) => string_atom(">="),
        Symbol(ShiftLeft) => string_atom("<<"),
        Symbol(ShiftRight) => string_atom(">>"),
        Symbol(EqEq) => string_atom("=="),
        Symbol(NotEq) => string_atom("!="),
        Symbol(Caret) => char!('^'),
        Symbol(Pipe) => char!('|'),
        Symbol(DoubleAmpersand) => string_atom("&&"),
        Symbol(DoublePipe) => string_atom("||"),

        // assignment operators
        Symbol(PlusEq) => string_atom("+="),
        Symbol(MinusEq) => string_atom("-="),
        Symbol(AsteriskEq) => string_atom("*="),
        Symbol(SlashEq) => string_atom("/="),
        Symbol(PercentEq) => string_atom("%="),
        Symbol(CaretEq) => string_atom("^="),
        Symbol(AmpersandEq) => string_atom("&="),
        Symbol(PipeEq) => string_atom("|="),
        Symbol(ShiftLeftEq) => string_atom("<<="),
        Symbol(ShiftRightEq) => string_atom(">>=")
    )
}

fn string_atom(s: &str) -> Regex<LexerSymbol> {
    Concat(s.chars().map(|c| Atom(Concrete(c))).collect())
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum TokenCategory {
    Keyword(KeywordKind),
    OpenDelim(DelimKind),
    CloseDelim(DelimKind),
    Symbol(SymbolKind),
    Whitespace,
    Comment,
    Identifier,
    TypeName,
    Literal(LiteralKind),
    Eof,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum KeywordKind {
    Let,
    If,
    Else,
    Fun,
    Static,
    Pure,
    Enum,
    Public,
    Private,
    Internal,
    Return,

    // loops
    For,
    While,
    Loop,
    Do,
    In,
    Break,
    Continue,

    // modules
    Module,
    Import,
    Qualified,
    As,
    Hiding,

    Mut,
    MutRef,
    MutPtr,

    NewType,
    TypeAlias,

    Struct,
    Class,
    Interface,
    Implements,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum DelimKind {
    Paren,
    Bracket,
    Brace,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum SymbolKind {
    // syntactical
    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    Dot,
    Arrow,
    DotDot,
    DotDotEq,

    // operators
    Eq,
    EqEq,
    NotEq,
    Ampersand,
    Asterisk,
    Percent,
    Slash,
    Plus,
    Minus,
    Bang,
    Tilde,

    Lt,
    Leq,
    Gt,
    Geq,
    ShiftLeft,
    ShiftRight,
    Caret,
    Pipe,
    DoubleAmpersand,
    DoublePipe,
    AmpersandEq,
    AsteriskEq,
    PercentEq,
    SlashEq,
    PlusEq,
    MinusEq,
    ShiftLeftEq,
    ShiftRightEq,
    CaretEq,
    PipeEq,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum LiteralKind {
    IntLiteral,
    FloatLiteral,
    BoolLiteral,
    StringLiteral,
}
