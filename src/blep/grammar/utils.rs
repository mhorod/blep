use crate::regex::{Regex, Regex::*};
use crate::blep::tokens::*;
use crate::blep::grammar::GrammarSymbol;

#[macro_export]
macro_rules! tok {
    ($token:expr) => {
        Atom(Token($token))
    };
}

#[macro_export]
macro_rules! kw {
    ($token:expr) => {
        Atom(Token(Keyword($token)))
    };
}

#[macro_export]
macro_rules! symb {
    ($token:expr) => {
        Atom(Token(Symbol($token)))
    };
}

#[macro_export]
macro_rules! lit {
    ($token:expr) => {
        Atom(Token(Literal($token)))
    };
}

#[macro_export]
macro_rules! regex_grammar {
    ($start:expr, $($symbol:expr => $regex:expr),*) => {
        $crate::parsing::grammar::regex::RegexGrammar {
            start: $start,
            productions: std::collections::HashMap::from([$(($symbol, $regex)),*])
        }

    };
}

#[macro_export]
macro_rules! grammar_productions {
    ($start:expr, $($symbol:expr => $regex:expr),*) => {
        std::collections::HashMap::from([$(($symbol, $regex)),*])
    };
}
pub fn optional<T>(regex: Regex<T>) -> Regex<T> {
    regex | Regex::Epsilon
}

pub fn interspersed(
    expr: Regex<GrammarSymbol>,
    separator: Regex<GrammarSymbol>,
) -> Regex<GrammarSymbol> {
    expr.clone() + (separator + expr).star()
}

pub fn separated(expr: Regex<GrammarSymbol>, separator: Regex<GrammarSymbol>) -> Regex<GrammarSymbol> {
    expr.clone() + (separator.clone() + expr).star() + optional(separator)
}

pub fn delimited(expr: Regex<GrammarSymbol>, delim_kind: DelimKind) -> Regex<GrammarSymbol> {
    use GrammarSymbol::Token;
    use TokenCategory::*;
    tok!(OpenDelim(delim_kind)) + expr + tok!(CloseDelim(delim_kind))
}

pub fn empty_delims(delim_kind: DelimKind) -> Regex<GrammarSymbol> {
    use GrammarSymbol::Token;
    use TokenCategory::*;
    tok!(OpenDelim(delim_kind)) + tok!(CloseDelim(delim_kind))
}
