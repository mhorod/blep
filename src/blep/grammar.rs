use super::tokens::*;
use crate::regex::Regex::*;
use crate::{parsing::grammar::Grammar, regex_grammar};

macro_rules! tok {
    ($token:expr) => {
        Atom(Token($token))
    };
}

macro_rules! kw {
    ($token:expr) => {
        Atom(Token(Keyword($token)))
    };
}

macro_rules! symb {
    ($token:expr) => {
        Atom(Token(Symbol($token)))
    };
}

macro_rules! lit {
    ($token:expr) => {
        Atom(Token(Literal($token)))
    };
}

pub fn blep_grammar() -> Grammar<GrammarSymbol> {
    use GrammarSymbol::*;
    use KeywordKind::*;
    use LiteralKind::*;
    use SymbolKind::*;
    use TokenCategory::*;

    regex_grammar!(
    Start,
    Start => Atom(LetDecl),
    LetDecl => kw!(KwLet) + tok!(Identifier) + symb!(Assign) + Atom(Expr) + kw!(KwIn) + Atom(Expr),
    Expr => lit!(IntLiteral) | tok!(Identifier)
    )
    .into()
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum GrammarSymbol {
    Start,
    Token(TokenCategory),
    LetDecl,
    Expr,
}
