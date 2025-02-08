use crate::automata::{dfa::*, nfa};
use crate::parsing::grammar::*;
use crate::regex::Regex;
use std::hash::Hash;

use std::collections::HashSet;

type Dfa<T> = dfa::Dfa<T, ()>;

pub struct RegexGrammar<T> {
    pub start: T,
    pub productions: HashMap<T, Regex<T>>,
}

#[macro_export]
macro_rules! regex_grammar {
    ($start:expr, $($symbol:expr => $regex:expr),*) => {
        $crate::parsing::grammar::regex::RegexGrammar {
            start: $start,
            productions: std::collections::HashMap::from([$(($symbol, $regex)),*
            ])
        }

    };
}

fn add_to_states<T: Hash + Eq>(dfa: Dfa<T>, add: u32) -> Dfa<T> {
    let start = dfa.start + add;
    let accepting: HashMap<State, ()> = dfa
        .get_accepting_states()
        .iter()
        .map(|x| (x + add, ()))
        .collect();
    let states = dfa.states;
    let transitions: DfaTransitions<T> = dfa
        .transitions
        .into_iter()
        .map(|((state, symbol), next_state)| ((state + add, symbol), next_state + add))
        .collect();

    Dfa {
        start,
        transitions,
        accepting,
        states,
    }
}

impl<T: Clone + Eq + Hash> From<RegexGrammar<T>> for Grammar<T> {
    fn from(grammar: RegexGrammar<T>) -> Self {
        let productions: HashMap<T, Dfa<T>> = grammar
            .productions
            .into_iter()
            .map(|(symbol, regex)| (symbol, Dfa::from_regex(regex)))
            .scan(0, |add, (symbol, dfa)| {
                let states = dfa.states;
                let result = add_to_states(dfa, *add);
                *add += states;
                Some((symbol, result))
            })
            .collect();

        Grammar {
            start: grammar.start,
            productions,
        }
    }
}
