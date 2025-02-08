use std::collections::{hash_map::Entry, HashMap, HashSet, VecDeque};
use std::hash::Hash;

use crate::automata::dfa::*;
use crate::automata::State;

struct DfaJoiner<T, R> {
    transitions: DfaTransitions<T>,
    accepting: HashMap<State, Vec<R>>,
    states: u32,
}

impl<T: Eq + Hash, R> DfaJoiner<T, R> {
    fn new() -> Self {
        Self {
            transitions: DfaTransitions::new(),
            accepting: HashMap::new(),
            states: 1,
        }
    }

    fn add_all(mut self, automata: Vec<Dfa<T, R>>) -> Self {
        automata.into_iter().for_each(|a| self.add(a));
        self
    }

    fn build(self) -> Dfa<T, Vec<R>> {
        Dfa {
            transitions: self.transitions,
            accepting: self.accepting,
            start: 0,
            states: self.states,
        }
    }

    fn add(&mut self, automaton: Dfa<T, R>) {
        let mut transposed = Self::transpose(automaton.transitions);
        let mut mapping: HashMap<State, State> = HashMap::new();
        let mut queue = VecDeque::from([automaton.start]);
        mapping.insert(0, automaton.start);

        while let Some(state) = queue.pop_front() {
            let state_mapped = *mapping.get(&state).unwrap();

            for (symbol, next) in transposed
                .remove(&state)
                .into_iter()
                .flat_map(|x| x.into_iter())
            {
                if let Entry::Vacant(e) = mapping.entry(next) {
                    e.insert(self.add_transition(state_mapped, symbol));
                    queue.push_back(next);
                }
            }
        }

        self.add_accepting(&mapping, automaton.accepting);
    }

    fn add_transition(&mut self, state: State, symbol: T) -> State {
        match self.transitions.entry((state, symbol)) {
            Entry::Vacant(e) => {
                let result = self.states;
                e.insert(result);
                self.states += 1;
                result
            }
            Entry::Occupied(e) => *e.get(),
        }
    }

    fn add_accepting(&mut self, mapping: &HashMap<State, State>, accepting: HashMap<State, R>) {
        for (state, result) in accepting.into_iter() {
            let mapped = mapping.get(&state).unwrap();
            if let Some(v) = self.accepting.get_mut(mapped) {
                v.push(result);
            } else {
                self.accepting.insert(*mapped, vec![result]);
            }
        }
    }

    fn transpose(transitions: DfaTransitions<T>) -> HashMap<State, Vec<(T, State)>> {
        let mut result: HashMap<State, Vec<(T, State)>> = HashMap::new();

        for ((state, symbol), next_state) in transitions {
            let entry = (symbol, next_state);
            if let Some(v) = result.get_mut(&state) {
                v.push(entry);
            } else {
                result.insert(state, vec![entry]);
            }
        }

        result
    }
}

impl<T: Eq + Hash, R> Dfa<T, Vec<R>> {
    pub fn joined(automata: Vec<Dfa<T, R>>) -> Self {
        DfaJoiner::new().add_all(automata).build()
    }
}
