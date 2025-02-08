use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::graphs::get_transitive_closure;
use crate::automata::State;
use crate::parsing::grammar::Grammar;

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
enum GeneralSymbol<T> {
    State(State),
    Symbol(T),
}

pub struct FirstSymbols<T> {
    pub for_states: HashMap<State, HashSet<T>>,
    pub for_symbols: HashMap<T, HashSet<T>>,
}

pub fn find_first_symbols<T: Hash + Eq + Copy>(
    grammar: &Grammar<T>,
    nullable: &HashSet<T>,
) -> FirstSymbols<T> {
    let mut graph: HashMap<GeneralSymbol<T>, HashSet<GeneralSymbol<T>>> = HashMap::new();

    for symbol in grammar.get_all_symbols() {
        insert(
            &mut graph,
            GeneralSymbol::Symbol(*symbol),
            GeneralSymbol::Symbol(*symbol),
        )
    }

    for (symbol, dfa) in &grammar.productions {
        insert(
            &mut graph,
            GeneralSymbol::Symbol(*symbol),
            GeneralSymbol::State(dfa.start),
        );
    }

    for ((state, symbol), next_state) in grammar
        .productions
        .values()
        .flat_map(|dfa| dfa.transitions.iter())
    {
        insert(
            &mut graph,
            GeneralSymbol::State(*state),
            GeneralSymbol::Symbol(*symbol),
        );

        if nullable.contains(symbol) {
            insert(
                &mut graph,
                GeneralSymbol::State(*state),
                GeneralSymbol::State(*next_state),
            );
        }
    }

    let graph = get_transitive_closure(&graph);

    let for_states = graph
        .iter()
        .flat_map(|(key, value)| match key {
            GeneralSymbol::State(s) => vec![(s, value)],
            _ => vec![],
        })
        .map(|(key, value)| (*key, leave_symbols(value)))
        .collect();

    let for_symbols = graph
        .iter()
        .flat_map(|(key, value)| match key {
            GeneralSymbol::Symbol(s) => vec![(s, value)],
            _ => vec![],
        })
        .map(|(key, value)| (*key, leave_symbols(&value)))
        .collect();

    FirstSymbols {
        for_states,
        for_symbols,
    }
}

fn insert<T: Eq + Hash + Copy>(map: &mut HashMap<T, HashSet<T>>, key: T, value: T) {
    map.entry(key)
        .and_modify(|s| {
            s.insert(value);
        })
        .or_insert(HashSet::from([value]));
}

fn leave_symbols<T: Eq + Hash + Copy>(xs: &HashSet<GeneralSymbol<T>>) -> HashSet<T> {
    xs.iter()
        .flat_map(|s| match s {
            GeneralSymbol::Symbol(symbol) => vec![*symbol],
            _ => vec![],
        })
        .collect()
}
