mod first;
mod follow;
mod nullable;
pub mod regex;

use std::fmt::Debug;
use std::hash::Hash;

use crate::parsing::automata::{dfa::Dfa, State};
use crate::parsing::grammar::{
    first::find_first_symbols, follow::find_follow_symbols, nullable::find_nullable_symbols,
};
use std::collections::{HashMap, HashSet};

pub struct Grammar<T> {
    pub start: T,
    pub productions: HashMap<T, Dfa<T>>,
}

pub struct GrammarAnalysis<T> {
    nullable: HashSet<T>,
    first: HashMap<State, HashSet<T>>,
    follow: HashMap<T, HashSet<T>>,
}

pub fn analyze<T: Copy + Eq + Hash + Debug>(grammar: &Grammar<T>) -> GrammarAnalysis<T> {
    println!("{:?}", grammar.get_all_symbols());
    let nullable = find_nullable_symbols(grammar);
    let first = find_first_symbols(grammar, &nullable);
    let follow = find_follow_symbols(grammar, &nullable, &first.for_states);

    println!("Nullable: {:?}", nullable);
    println!("First for states: {:?}", first.for_states);
    println!("First for symbols: {:?}", first.for_symbols);
    println!("Follow: {:?}", follow);

    GrammarAnalysis {
        nullable,
        first: first.for_states,
        follow,
    }
}

impl<T: Hash + Eq> Grammar<T> {
    pub fn get_all_symbols(&self) -> HashSet<&T> {
        self.productions
            .iter()
            .flat_map(|(symbol, dfa)| std::iter::once(symbol).chain(dfa.get_all_symbols()))
            .collect()
    }
}
