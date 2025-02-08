pub mod lexer;

use std::fmt::Debug;

use crate::regex::Regex;

#[derive(Eq, PartialEq, Hash, Debug)]
pub struct Token<C> {
    pub content: String,
    pub category: C,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum LexerSymbol {
    Concrete(char),
    Lowercase,
    Uppercase,
    Digit,
}

pub type Categories<C> = Vec<Category<C>>;

#[derive(Debug)]
pub struct Category<C> {
    pub category: C,
    pub regex: Regex<LexerSymbol>,
}

#[macro_export]
macro_rules! token_categories {
    ($($category:expr => $regex:expr),*) => {
           vec![
                $($crate::lexing::Category { category: $category, regex: $regex } ),*
            ]
    };
}
