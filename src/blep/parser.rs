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
    ) -> ParsingResult<GrammarSymbol, Token<GrammarSymbol>> {
        let end = tokens.last().map(|t| t.range.end).unwrap_or(0);
        tokens.push(Token {
            range: end..end,
            content: String::new(),
            category: TokenCategory::Eof,
        });
        let tokens = tokens.into_iter().map(|t| Token {
            range: t.range,
            content: t.content,
            category: GrammarSymbol::Token(t.category)
        });

        self.parser.parse(TokenStream::from_iter(tokens))
    }
}

impl Categorized<GrammarSymbol> for Token<GrammarSymbol> {
    fn get_category(&self) -> &GrammarSymbol {
        &self.category
    }
}
