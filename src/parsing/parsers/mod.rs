pub mod llone;

use std::collections::VecDeque;

pub struct TokenStream<T> {
    index: usize,
    tokens: VecDeque<T>,
}

pub trait Token<T> {
    fn content(&self) -> &T;
}

impl<T> Token<T> for T {
    fn content(&self) -> &Self {
        self
    }
}

impl<T> TokenStream<T> {
    pub fn peek(&self) -> Option<&T> {
        self.tokens.front()
    }
    pub fn take(&mut self) -> T {
        self.tokens.pop_front().unwrap()
    }
}

impl<T> FromIterator<T> for TokenStream<T> {
    fn from_iter<A: IntoIterator<Item = T>>(iter: A) -> Self {
        Self {
            index: 0,
            tokens: iter.into_iter().collect(),
        }
    }
}
