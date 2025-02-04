use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::graphs::get_transitive_closure;
use crate::parsing::automata::State;
use crate::parsing::grammar::Grammar;

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
enum GeneralSymbol<T> {
    State(State),
    Symbol(T),
}

pub fn find_first_symbols<T: Hash + Eq + Copy>(
    grammar: &Grammar<T>,
    nullable: &HashSet<T>,
) -> HashMap<State, HashSet<T>> {
    let mut graph: HashMap<GeneralSymbol<T>, HashSet<GeneralSymbol<T>>> = HashMap::new();

    for production in &grammar.productions {
        insert(&mut graph,
            GeneralSymbol::Symbol(production.symbol),
            GeneralSymbol::State(production.produces.start)
        );
    }

    for ((state, symbol), next_state) in grammar
        .productions
        .iter()
        .flat_map(|p| p.produces.transitions.iter())
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

    graph
        .into_iter()
        .flat_map(|(key, value)| match key {
            GeneralSymbol::State(s) => vec![(s, value)],
            _ => vec![],
        })
        .map(|(key, value)| (key, leave_symbols(&value)))
        .collect()
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
