use crate::lexing::Token;

use crate::parsing::parsers::{
    llone::{LLOneParser, ParsingResult},
    Categorized, TokenStream,
};

use super::{grammar::GrammarSymbol, tokens::TokenCategory};

pub struct BlepParser {
    parser: LLOneParser<GrammarSymbol>,
}

impl BlepParser {
    pub fn new(parser: LLOneParser<GrammarSymbol>) -> Self {
        Self { parser }
    }

    pub fn parse(
        &self,
        mut tokens: Vec<Token<TokenCategory>>,
    ) -> ParsingResult<GrammarSymbol, Token<TokenCategory>> {
        let end = tokens.last().unwrap().range.end;
        tokens.push(Token {
            range: end..end,
            content: String::new(),
            category: TokenCategory::Eof,
        });
        self.parser.parse(TokenStream::from_iter(tokens))
    }
}

impl Categorized<GrammarSymbol> for Token<TokenCategory> {
    fn get_category(&self) -> GrammarSymbol {
        GrammarSymbol::Token(self.category)
    }
}
