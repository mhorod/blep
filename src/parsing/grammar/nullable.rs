use crate::parsing::automata::State;
use crate::parsing::grammar::*;

use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;


struct NullableFinder<T> {
    queue: VecDeque<State>,
    reversed_transitions: HashMap<State, Vec<(State, T)>>,
    start_states: HashMap<State, T>,
    conditionally_nullable: HashMap<T, Vec<State>>,
    nullable_states: HashSet<State>,
    nullable_symbols: HashSet<T>,
}

pub fn find_nullable_symbols<T: Hash + Eq + Copy>(grammar: &Grammar<T>) -> HashSet<T> {
    NullableFinder::new(grammar).find_nullable()
}


impl<T: Hash + Eq + Copy> NullableFinder<T> {
    fn new(grammar: &Grammar<T>) -> Self {
        Self {
            queue: accepting_states(grammar).into_iter().collect(),
            reversed_transitions: reversed_transitions(grammar),
            start_states: start_states(grammar),
            conditionally_nullable: HashMap::new(),
            nullable_states: HashSet::new(),
            nullable_symbols: HashSet::new(),
        }
    }

    fn find_nullable(mut self) -> HashSet<T> {
        while let Some(current) = self.queue.pop_front() {
            self.nullable_states.insert(current);
            self.mark_conditionally_nullable_before(current);
            self.resolve_conditionally_nullable_after(current);
        }

        self.nullable_symbols.into_iter().collect()
    }

    fn mark_conditionally_nullable_before(&mut self, state: State) {
        if let Some(previous) = self.reversed_transitions.get(&state) {
            for (state, symbol) in previous {
                self.conditionally_nullable
                    .entry(*symbol)
                    .and_modify(|v: &mut Vec<_>| v.push(*state))
                    .or_insert(vec![*state]);
            }
        }
    }

    fn resolve_conditionally_nullable_after(&mut self, state: State) {
        if let Some(symbol) = self.start_states.get(&state) {
            self.nullable_symbols.insert(symbol.clone());
            if let Some(states) = self.conditionally_nullable.get(symbol) {
                states.iter().for_each(|s| {
                    if !self.nullable_states.contains(s) {
                        self.nullable_states.insert(*s);
                        self.queue.push_back(*s);
                    }
                });
                self.conditionally_nullable.insert(symbol.clone(), vec![]);
            }
        }
    }
}

fn reversed_transitions<T: Clone>(grammar: &Grammar<T>) -> HashMap<State, Vec<(State, T)>> {
    let mut result: HashMap<State, Vec<(State, T)>> = HashMap::new();

    for production in &grammar.productions {
        for (k, v) in &production.produces.transitions {
            result
                .entry(*v)
                .and_modify(|vec: &mut Vec<(State, T)>| vec.push(k.clone()))
                .or_insert(vec![k.clone()]);
        }
    }

    result
}

fn accepting_states<T>(grammar: &Grammar<T>) -> Vec<State> {
    grammar
        .productions
        .iter()
        .flat_map(|production| production.produces.accepting.iter())
        .copied()
        .collect()
}

fn start_states<T: Clone>(grammar: &Grammar<T>) -> HashMap<State, T> {
    grammar
        .productions
        .iter()
        .map(|production| (production.produces.start, production.symbol.clone()))
        .collect()
}
