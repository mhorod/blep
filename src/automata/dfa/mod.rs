mod joining;

use crate::automata::Symbol;
use crate::automata::{nfa::Nfa, State};
use crate::regex::Regex;

use std::collections::{hash_map::Entry, HashMap, HashSet, VecDeque};
use std::hash::Hash;

pub type DfaTransitions<T> = HashMap<(State, T), State>;

#[derive(Debug)]
pub struct Dfa<T, R> {
    pub start: State,
    pub transitions: DfaTransitions<T>,
    pub accepting: HashMap<State, R>,
    pub states: u32,
}

struct EpsClosure {
    closure: HashMap<State, HashSet<State>>,
}

impl EpsClosure {
    fn from_nfa<T: Eq + Hash>(nfa: &Nfa<T>) -> Self {
        let mut closure = HashMap::new();
        for s in 0..nfa.states {
            closure.insert(s, Self::closure_from(s, nfa));
        }

        EpsClosure { closure }
    }

    fn closure_from<T: Eq + Hash>(source: State, nfa: &Nfa<T>) -> HashSet<State> {
        let mut result = HashSet::new();
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();
        queue.push_back(source);
        while !queue.is_empty() {
            let current = queue.pop_front().unwrap();
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current);

            result.insert(current);
            if let Some(v) = nfa.transitions.get(&(current, Symbol::Empty)) {
                queue.extend(v)
            }
        }

        result
    }

    fn close(&self, states: &[State]) -> Vec<State> {
        let mut result = HashSet::new();
        for state in states.iter() {
            if let Some(closure) = self.closure.get(state) {
                result.extend(closure);
            }
        }
        let mut result: Vec<State> = result.into_iter().collect();
        result.sort();
        result
    }
}

struct StateSymbols<T> {
    state_symbols: HashMap<State, HashSet<T>>,
}

impl<T: Eq + Hash + Clone> StateSymbols<T> {
    fn from(nfa: &Nfa<T>) -> Self {
        let mut result: HashMap<State, HashSet<T>> = HashMap::new();

        for (state, symbol) in nfa.transitions.keys() {
            let symbol = if let Symbol::Symbol(value) = symbol {
                value
            } else {
                continue;
            };

            result
                .entry(*state)
                .and_modify(|v: &mut HashSet<_>| {
                    v.insert(symbol.clone());
                })
                .or_insert(HashSet::from([symbol.clone()]));
        }

        Self {
            state_symbols: result,
        }
    }

    fn get(&self, states: &[State]) -> HashSet<T> {
        let mut result: HashSet<T> = HashSet::new();
        for state in states {
            if let Some(symbols) = self.state_symbols.get(state) {
                result.extend(symbols.clone());
            }
        }

        result
    }
}

impl<T, R> Dfa<T, R> {
    pub fn is_accepting(&self, state: State) -> bool {
        self.accepting.contains_key(&state)
    }

    pub fn get_accepting_states(&self) -> HashSet<State> {
        self.accepting.keys().copied().collect()
    }

    pub fn get_result(&self, state: &State) -> &R {
        self.accepting.get(state).unwrap()
    }

    pub fn map_results<S, F>(self, mapper: F) -> Dfa<T, S>
    where
        F: Fn(R) -> S,
    {
        let accepting = self
            .accepting
            .into_iter()
            .map(|(state, result)| (state, mapper(result)))
            .collect();
        Dfa {
            start: self.start,
            transitions: self.transitions,
            accepting,
            states: self.states,
        }
    }

    pub fn get_all_symbols(&self) -> Vec<&T> {
        self.transitions
            .iter()
            .map(|((_, symbol), _)| symbol)
            .collect()
    }
}

impl<T: Eq + Hash, R> Dfa<T, R> {
    pub fn next(&self, state: State, symbol: T) -> Option<State> {
        self.transitions.get(&(state, symbol)).copied()
    }
}


impl<T: Eq + Hash + Clone> Dfa<T, ()> {
    pub fn from_regex(regex: Regex<T>) -> Self {
        Self::from_nfa(Nfa::from_regex(regex))
    }

    pub fn from_nfa(nfa: Nfa<T>) -> Self {
        let state_symbols = StateSymbols::from(&nfa);
        let mut transitions: DfaTransitions<T> = HashMap::new();
        let mut visited_states: HashMap<Vec<State>, State> = HashMap::new();
        let mut accepting_states: HashSet<State> = HashSet::new();
        let eps_closure = EpsClosure::from_nfa(&nfa);

        let mut queue: VecDeque<Vec<State>> = VecDeque::new();
        let initial = eps_closure.close(&[nfa.start]);
        queue.push_back(initial.clone());
        visited_states.insert(initial, 0);
        let mut state_count = 1;

        while !queue.is_empty() {
            let states = queue.pop_front().unwrap();

            let current_state_index = *visited_states.get(&states).unwrap();
            if states.iter().any(|s| *s == nfa.accepting) {
                accepting_states.insert(current_state_index);
            }

            let symbols = state_symbols.get(&states);

            for symbol in symbols {
                let next_states: Vec<State> = states
                    .iter()
                    .flat_map(|state| {
                        nfa.transitions
                            .get(&(*state, Symbol::Symbol(symbol.clone())))
                            .cloned()
                            .into_iter()
                    })
                    .flatten()
                    .collect();

                let next_states = eps_closure.close(&next_states);

                let next_state_index = if let Some(i) = visited_states.get(&next_states) {
                    *i
                } else {
                    let result = state_count;
                    visited_states.insert(next_states.clone(), state_count);
                    state_count += 1;
                    queue.push_back(next_states);
                    result
                };

                transitions.insert((current_state_index, symbol.clone()), next_state_index);
            }
        }

        let accepting = accepting_states.into_iter().map(|s| (s, ())).collect();
        Dfa {
            start: 0,
            transitions,
            accepting,
            states: state_count,
        }
    }
}
