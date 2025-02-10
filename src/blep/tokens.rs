use std::fmt::Debug;
use std::hash::Hash;

use crate::lexing::Categories;
use crate::lexing::LexerSymbol;
use crate::lexing::LexerSymbol::*;
use crate::regex::{Regex, Regex::*};
use crate::token_categories;

macro_rules! char {
    ($c:expr) => {
        Atom(Concrete($c))
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
        Literal(IntLiteral) => Atom(Digit) + Atom(Digit).star(),
        Literal(StringLiteral) => char!('"') + Atom(Not('"')).star() + char!('"')
    )
}

fn blep_keywords() -> Categories<TokenCategory> {
    use KeywordKind::*;
    use TokenCategory::*;
    token_categories!(
        // keywords
        Keyword(Let) => string_atom("let"),
        Keyword(In) => string_atom("in"),
        Keyword(Pure) => string_atom("pure"),
        Keyword(Fun) => string_atom("fun"),
        Keyword(Enum) => string_atom("enum"),
        Keyword(Public) => string_atom("public"),
        Keyword(Private) => string_atom("private"),
        Keyword(Return) => string_atom("return"),

        // module keywords
        Keyword(Module) => string_atom("module"),
        Keyword(Import) => string_atom("import"),
        Keyword(Qualified) => string_atom("qualified"),
        Keyword(As) => string_atom("as"),

        // mutability keywords
        Keyword(Mut) => string_atom("mut"),
        Keyword(MutRef) => string_atom("&mut"),
        Keyword(MutPtr) => string_atom("*mut"),

        // type keywords
        Keyword(NewType) => string_atom("newtype"),
        Keyword(TypeAlias) => string_atom("typealias"),

        // Data structure keywords
        Keyword(Struct) => string_atom("struct"),
        Keyword(Class) => string_atom("class"),
        Keyword(Interface) => string_atom("interface"),
        Keyword(Implements) => string_atom("implements")
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

        // operators
        Symbol(Arrow) => string_atom("->"),
        Symbol(Ampersand) => char!('&'),
        Symbol(Asterisk) => char!('*'),
        Symbol(Slash) => char!('/'),
        Symbol(Percent) => char!('/'),
        Symbol(Plus) => char!('+'),
        Symbol(Minus) => char!('-'),
        Symbol(Lt) => char!('<'),
        Symbol(Leq) => string_atom("<="),
        Symbol(Gt) => char!('<'),
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
        Symbol(DoubleAmpersandEq) => string_atom("&&="),
        Symbol(DoublePipeEq) => string_atom("||=")
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
    Identifier,
    TypeName,
    Literal(LiteralKind),
    Eof,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum KeywordKind {
    Let,
    In,
    If,
    Then,
    Else,
    Fun,
    Pure,
    Enum,
    Public,
    Private,
    Return,

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
    Eq,
    EqEq,
    NotEq,
    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    Dot,
    Arrow,
    Ampersand,
    Asterisk,
    Percent,
    Slash,
    Plus,
    Minus,
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
    ShiftRightEQ,
    CaretEq,
    PipeEq,
    DoubleAmpersandEq,
    DoublePipeEq,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum LiteralKind {
    IntLiteral,
    StringLiteral,
}
