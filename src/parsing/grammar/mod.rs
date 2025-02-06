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

pub type Production<'a, T> = (&'a T, &'a Dfa<T>);
pub struct Grammar<T> {
    pub start: T,
    pub productions: HashMap<T, Dfa<T>>,
}


pub struct AnalyzedGrammar<T> {
    pub grammar: Grammar<T>,
    pub nullable: HashSet<T>,
    pub first: HashMap<T, HashSet<T>>,
    pub follow: HashMap<T, HashSet<T>>,
}

impl<T: Eq + Hash + Copy> AnalyzedGrammar<T> {
    pub fn get_first_plus(&self, symbol: &T) -> HashSet<T> {
        if self.nullable.contains(symbol) {
                [self.first.get(symbol), self.follow.get(symbol)]
                    .into_iter()
                    .flat_map(|x| x.into_iter())
                    .flat_map(|x| x.iter())
                    .copied()
                    .collect()
        } else {
            self.first.get(symbol).unwrap_or(&HashSet::new()).clone()
        }
    }
}

pub fn analyze<T: Copy + Eq + Hash + Debug>(grammar: Grammar<T>) -> AnalyzedGrammar<T> {
    println!("{:?}", grammar.get_all_symbols());
    let nullable = find_nullable_symbols(&grammar);
    let first = find_first_symbols(&grammar, &nullable);
    let follow = find_follow_symbols(&grammar, &nullable, &first.for_states);

    println!("Nullable: {:?}", nullable);
    println!("First for states: {:?}", first.for_states);
    println!("First for symbols: {:?}", first.for_symbols);
    println!("Follow: {:?}", follow);

    AnalyzedGrammar {
        grammar,
        nullable,
        first: first.for_symbols,
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

    pub fn get_start_production(&self) -> Production<T> {
        self.get_production(&self.start).unwrap()
    }

    pub fn get_production(&self, symbol: &T) -> Option<Production<T>> {
        self.productions.get_key_value(symbol) 
    }
}
