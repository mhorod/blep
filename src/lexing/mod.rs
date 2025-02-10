pub mod lexer;

use std::fmt::Debug;
use std::ops::Range;
use crate::regex::Regex;

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Token<C> {
    pub range: Range<usize>,
    pub content: String,
    pub category: C,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum LexerSymbol {
    Concrete(char),
    Digit,
    Lowercase,
    Uppercase,
    Not(char),
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
