mod first;
mod follow;
mod nullable;
pub mod regex;

use std::fmt::Debug;
use std::hash::Hash;

use crate::parsing::automata::dfa::Dfa;
use crate::parsing::grammar::{
    first::find_first_symbols, follow::find_follow_symbols, nullable::find_nullable_symbols,
};
use std::collections::HashSet;

pub struct Grammar<T> {
    pub start: T,
    pub productions: Vec<Production<T>>,
}

pub struct Production<T> {
    pub symbol: T,
    pub produces: Dfa<T>,
}

pub fn analyze<T: Copy + Eq + Hash + Debug>(grammar: &Grammar<T>) {
    println!("{:?}", grammar.get_all_symbols());
    let nullable = find_nullable_symbols(grammar);
    let first = find_first_symbols(grammar, &nullable);
    let follow = find_follow_symbols(grammar, &nullable, &first);

    println!("Nullable: {:?}", nullable);
    println!("First: {:?}", first);
    println!("Follow: {:?}", follow);
}

impl<T: Hash + Eq> Grammar<T> {
    pub fn get_all_symbols(&self) -> HashSet<&T> {
        self.productions
            .iter()
            .flat_map(|p| std::iter::once(&p.symbol).chain(p.produces.get_all_symbols()))
            .collect()
    }
}
