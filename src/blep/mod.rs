pub mod grammar;
pub mod lexer;
pub mod parser;
pub mod tokens;

use crate::lexing::Token;
use crate::parsing::grammar::analyze;
use crate::parsing::parsers::llone::LLOneParser;
use grammar::{blep_grammar, GrammarSymbol};
use tokens::{blep_token_categories, TokenCategory};

use crate::lexing::lexer::Lexer;


use lexer::BlepLexer;
use parser::BlepParser;

pub fn blep_lexer() -> BlepLexer {
    BlepLexer::new(Lexer::from_categories(blep_token_categories()))
}

pub fn blep_parser() -> BlepParser {
    BlepParser::new(LLOneParser::new(analyze(blep_grammar())))
}
