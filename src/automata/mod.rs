pub mod dfa;
pub mod nfa;

use std::hash::Hash;

pub type State = u32;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Symbol<T> {
    Empty,
    Symbol(T),
}
