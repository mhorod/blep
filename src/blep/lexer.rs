use super::tokens::TokenCategory;
use crate::lexing::{lexer::Lexer, Token};

pub struct BlepLexer {
    lexer: Lexer<TokenCategory>,
}

impl BlepLexer {
    pub fn lex(&self, input: String) -> Vec<Token<TokenCategory>> {
        self.lexer
            .lex(input)
            .into_iter()
            .filter(|t| !matches!(t.category, TokenCategory::Whitespace))
            .filter(|t| !matches!(t.category, TokenCategory::Comment))
            .collect()
    }

    pub fn new(lexer: Lexer<TokenCategory>) -> Self {
        Self { lexer }
    }
}
