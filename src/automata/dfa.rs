use itertools::Itertools;

use crate::automata::Symbol;
use crate::automata::{nfa::Nfa, State};
use crate::regex::Regex;

use std::collections::{hash_map::Entry, HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

pub type DfaTransitions<T> = HashMap<(State, T), State>;

#[derive(Debug)]
pub struct Dfa<T, R> {
    pub start: State,
    pub transitions: DfaTransitions<T>,
    symbols_from_state: HashMap<State, Vec<T>>,
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
    fn from_nfa(nfa: &Nfa<T>) -> Self {
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

impl<T: Clone, R> Dfa<T, R> {
    pub fn new(
        start: State,
        transitions: DfaTransitions<T>,
        accepting: HashMap<State, R>,
        states: u32,
    ) -> Self {
        let symbols_from_state: HashMap<State, Vec<T>> =
            transitions.keys().cloned().into_group_map();
        Self {
            start,
            transitions,
            symbols_from_state,
            accepting,
            states,
        }
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
        Dfa::new(self.start, self.transitions, accepting, self.states)
    }
}

impl<T, R> Dfa<T, R> {
    pub fn get_symbols_from_state(&self, state: State) -> Option<&Vec<T>> {
        self.symbols_from_state.get(&state)
    }
    pub fn is_accepting(&self, state: State) -> bool {
        self.accepting.contains_key(&state)
    }

    pub fn get_accepting_states(&self) -> HashSet<State> {
        self.accepting.keys().copied().collect()
    }

    pub fn get_result(&self, state: &State) -> &R {
        self.accepting.get(state).unwrap()
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

pub trait Wildcards<T> {
    fn is_wildcard(&self, symbol: &T) -> bool;
    fn matches(&self, wildcard: &T, symbol: &T) -> bool;
    fn intersection(&self, s1: &T, s2: &T) -> Option<T>;
}

struct NoWildcards {}
impl<T> Wildcards<T> for NoWildcards {
    fn is_wildcard(&self, symbol: &T) -> bool {
        false
    }
    fn matches(&self, wildcard: &T, symbol: &T) -> bool {
        false
    }
    fn intersection(&self, s1: &T, s2: &T) -> Option<T> {
        None
    }
}

struct FromNfaBuilder<'a, T, W: Wildcards<T>> {
    nfa: Nfa<T>,
    wildcards: &'a W,

    state_symbols: StateSymbols<T>,
    eps_closure: EpsClosure,

    transitions: DfaTransitions<T>,
    accepting_states: HashSet<State>,
    visited_states: HashMap<Vec<State>, State>,

    state_count: u32,
}

impl<T: Eq + Hash + Clone + Debug> Dfa<T, ()> {
    pub fn from_regex(regex: Regex<T>) -> Self {
        let wildcards = NoWildcards {};
        Self::from_nfa(Nfa::from_regex(regex), &wildcards)
    }

    pub fn from_regex_with_wildcards<W: Wildcards<T>>(regex: Regex<T>, wildcards: &W) -> Self {
        Self::from_nfa(Nfa::from_regex(regex), wildcards)
    }

    pub fn from_nfa<W: Wildcards<T>>(nfa: Nfa<T>, wildcards: &W) -> Self {
        FromNfaBuilder::new(nfa, wildcards).build()
    }
}

impl<'a, T: Eq + Hash + Clone + Debug, W: Wildcards<T>> FromNfaBuilder<'a, T, W> {
    fn new(nfa: Nfa<T>, wildcards: &'a W) -> Self {
        let state_symbols = StateSymbols::from_nfa(&nfa);
        let eps_closure = EpsClosure::from_nfa(&nfa);

        Self {
            nfa,
            wildcards,

            state_symbols,
            eps_closure,

            transitions: HashMap::new(),
            visited_states: HashMap::new(),
            accepting_states: HashSet::new(),
            state_count: 1,
        }
    }

    fn build(mut self) -> Dfa<T, ()> {
        let initial = self.eps_closure.close(&[self.nfa.start]);
        self.visited_states.insert(initial.clone(), 0);
        let mut nfa_states = VecDeque::from([initial]);

        while let Some(states) = nfa_states.pop_front() {
            let state_index = *self.visited_states.get(&states).unwrap();
            self.mark_accepting(&states, state_index);
            self.add_next_states(&mut nfa_states, &states, state_index);
        }

        self.construct_dfa()
    }

    fn mark_accepting(&mut self, states: &[State], index: u32) {
        if states.iter().any(|s| *s == self.nfa.accepting) {
            self.accepting_states.insert(index);
        }
    }

    fn add_next_states(
        &mut self,
        queue: &mut VecDeque<Vec<State>>,
        states: &Vec<State>,
        state_index: u32,
    ) {
        let symbols = self.state_symbols.get(states);
        let (wildcards, atoms): (Vec<_>, Vec<_>) = symbols
            .into_iter()
            .partition(|s| self.wildcards.is_wildcard(s));

        let wildcards = self.wildcards_with_intersections(wildcards);

        for symbol in atoms.iter().chain(wildcards.iter()) {
            let mut next_states: HashSet<State> = HashSet::new();
            for state in states {
                for transition_symbol in self.get_transition_symbols(symbol, &wildcards) {
                    next_states.extend(self.get_transitions_on_symbol(*state, transition_symbol));
                }
            }

            let next_states: Vec<State> = next_states.into_iter().collect();
            let next_states = self.eps_closure.close(&next_states);

            let next_state_index: u32 = match self.visited_states.entry(next_states.clone()) {
                Entry::Vacant(e) => {
                    let result = self.state_count;
                    self.state_count += 1;
                    queue.push_back(next_states);
                    e.insert(result);
                    result
                }

                Entry::Occupied(e) => *e.get(),
            };

            self.transitions
                .insert((state_index, symbol.clone()), next_state_index);
        }
    }

    fn wildcards_with_intersections(&self, wildcards: Vec<T>) -> HashSet<T> {
        let mut result: HashSet<T> = HashSet::new();
        let mut to_process = VecDeque::from(wildcards);
        while let Some(wildcard_symbol) = to_process.pop_front() {
            for other in &result {
                if let Some(s) = self.wildcards.intersection(&wildcard_symbol, other) {
                    to_process.push_back(s);
                }
            }
            result.insert(wildcard_symbol);
        }

        result
    }

    fn get_transitions_on_symbol(&self, state: State, symbol: Symbol<T>) -> Vec<State> {
        self.nfa
            .transitions
            .get(&(state, symbol))
            .into_iter()
            .flat_map(|x| x.iter().copied())
            .collect()
    }

    fn get_transition_symbols(&self, symbol: &T, wildcards: &HashSet<T>) -> Vec<Symbol<T>> {
        let mut result = vec![Symbol::Symbol(symbol.clone())];
        let matching_wildcards = wildcards
            .iter()
            .filter(|w| self.wildcards.matches(w, symbol));
        result.extend(matching_wildcards.map(|w| Symbol::Symbol(w.clone())));

        result
    }

    fn construct_dfa(self) -> Dfa<T, ()> {
        let accepting = self.accepting_states.into_iter().map(|s| (s, ())).collect();
        Dfa::new(0, self.transitions, accepting, self.state_count)
    }
}
