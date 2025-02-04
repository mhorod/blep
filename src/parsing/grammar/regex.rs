use crate::parsing::automata::{dfa::*, nfa};
use crate::parsing::grammar::*;
use crate::parsing::regex::Regex;
use std::hash::Hash;

use std::collections::HashSet;

pub struct RegexGrammar<T> {
    pub start: T,
    pub productions: Vec<RegexProduction<T>>,
}

pub struct RegexProduction<T> {
    pub symbol: T,
    pub produces: Regex<T>,
}

#[macro_export]
macro_rules! regex_grammar {
    ($start:expr, $($symbol:expr => $regex:expr),*) => {
        $crate::parsing::grammar::regex::RegexGrammar {
            start: $start,
            productions: vec![
                $($crate::parsing::grammar::regex::RegexProduction{
                    symbol: $symbol,
                    produces: $regex
                }),*
            ]
        }

    };
}

fn add_to_states<T: Hash + Eq>(dfa: Dfa<T>, add: u32) -> Dfa<T> {
    let start = dfa.start + add;
    let accepting: HashSet<u32> = dfa.accepting.iter().map(|x| x + add).collect();
    let states = dfa.states;
    let transitions: DfaTransitions<T> = dfa.transitions.
        into_iter()
        .map(|((state, symbol), next_state)| ((state + add, symbol), next_state + add))
        .collect();


    
    Dfa {
        start,
        transitions,
        accepting,
        states
    }
}

impl<T: Clone + Eq + Hash> From<RegexGrammar<T>> for Grammar<T> {
    fn from(grammar: RegexGrammar<T>) -> Self {
        let productions: Vec<Production<T>> = grammar
            .productions
            .into_iter()
            .map(|p| (p.symbol, nfa::Nfa::from_regex(p.produces)))
            .map(|(symbol, nfa)| (symbol, Dfa::from_nfa(nfa)))
            .scan(0, |add, (symbol, dfa)| { 
                let states = dfa.states;
                let result = add_to_states(dfa, *add);
                *add += states;
                Some((symbol, result))
            })
            .map(|(symbol, dfa)| Production { symbol, produces: dfa } )
            .collect();


        Grammar {
            start: grammar.start,
            productions,
        }
    }
}
