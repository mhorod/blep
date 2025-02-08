use std::fmt::Debug;
use std::hash::Hash;

use crate::lexing::Categories;
use crate::lexing::LexerSymbol;
use crate::lexing::LexerSymbol::*;
use crate::regex::{Regex, Regex::*};
use crate::token_categories;

pub fn blep_token_categories() -> Categories<TokenCategory> {
    use DelimKind::*;
    use KeywordKind::*;
    use SymbolKind::*;
    use LiteralKind::*;
    use TokenCategory::*;
    token_categories!(
        // keywords
        Keyword(KwLet) => string_atom("let"),
        Keyword(KwIn) => string_atom("in"),

        Identifier => Atom(Lowercase) + (Atom(Lowercase) | Atom(Uppercase) | Atom(Digit)).star(),
        Whitespace => (Atom(Concrete(' ')) | Atom(Concrete('\t'))).star(),
        Literal(IntLiteral) => Atom(Digit) + Atom(Digit).star(),

        // delimeters
        OpenDelim(Paren) => Atom(Concrete('(')),
        CloseDelim(Paren) => Atom(Concrete(')')),
        OpenDelim(Bracket) => Atom(Concrete('[')),
        CloseDelim(Bracket) => Atom(Concrete(']')),
        OpenDelim(Brace) => Atom(Concrete('{')),
        CloseDelim(Brace) => Atom(Concrete('}')),

        // symbols
        Symbol(Assign) => Atom(Concrete('='))

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
    Literal(LiteralKind),
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum KeywordKind {
    KwLet,
    KwIn,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum DelimKind {
    Paren,
    Bracket,
    Brace,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum SymbolKind {
    Assign,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum LiteralKind {
    IntLiteral,
    StringLiteral,
}
