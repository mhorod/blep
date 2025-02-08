use crate::automata::{Symbol, State};
use crate::regex::Regex;
use std::collections::HashMap;
use std::hash::Hash;

type NfaTransitions<T> = HashMap<(State, Symbol<T>), Vec<State>>;

#[derive(Debug)]
pub struct Nfa<T> {
    pub start: State,
    pub transitions: NfaTransitions<T>,
    pub accepting: State,
    pub states: u32,
}

macro_rules! transitions {
    ($($from:expr, $via:expr => $to:expr);*) => {
        std::collections::HashMap::from([
            $((($from, $via),$to)),*
        ])
    };
}

impl<T: Eq + Hash + Clone> Nfa<T> {
    pub fn from_regex(regex: Regex<T>) -> Self {
        use Regex::*;
        match regex {
            Atom(a) => Self::atom_nfa(a),
            Union(regexes) => Self::nfa_union(regexes.into_iter().map(Self::from_regex).collect()),
            Concat(regexes) => {
                Self::nfa_concat(regexes.into_iter().map(Self::from_regex).collect())
            }
            Star(regex) => Self::nfa_star(Self::from_regex(*regex)),
        }
    }

    fn atom_nfa(atom: T) -> Self {
        Nfa {
            start: 0,
            accepting: 1,
            states: 2,
            transitions: transitions!(0, Symbol::Symbol(atom) => vec![1]),
        }
    }

    fn insert_nfa_transitions(dst: &mut NfaTransitions<T>, nfa: &Self, add: State) {
        nfa.transitions.iter().for_each(|(key, val)| {
            dst.insert(
                (key.0 + add, key.1.clone()),
                val.iter().map(|x| x + add).collect(),
            );
        })
    }

    fn add_transition(dst: &mut NfaTransitions<T>, key: (State, Symbol<T>), val: State) {
        dst.entry(key)
            .and_modify(|v: &mut Vec<_>| v.push(val))
            .or_insert(vec![val]);
    }

    fn nfa_union(nfas: Vec<Self>) -> Self {
        let inner_states: State = nfas.iter().map(|nfa| nfa.states).sum();
        let mut add = 2;
        let mut transitions: NfaTransitions<T> = HashMap::new();

        transitions.insert((0, Symbol::Empty), vec![]);
        for nfa in nfas.iter() {
            Self::insert_nfa_transitions(&mut transitions, nfa, add);
            Self::add_transition(&mut transitions, (0, Symbol::Empty), nfa.start + add);
            Self::add_transition(&mut transitions, (nfa.accepting + add, Symbol::Empty), 1);
            add += nfa.states;
        }

        Nfa {
            start: 0,
            accepting: 1,
            states: inner_states + 2,
            transitions,
        }
    }

    fn nfa_concat(nfas: Vec<Self>) -> Self {
        let inner_states: State = nfas.iter().map(|nfa| nfa.states).sum();
        let mut add = 2;
        let mut transitions: NfaTransitions<T> = HashMap::new();
        let mut last_end = 0;
        for nfa in nfas.iter() {
            Self::insert_nfa_transitions(&mut transitions, nfa, add);
            Self::add_transition(&mut transitions, (last_end, Symbol::Empty), nfa.start + add);
            last_end = nfa.accepting + add;
            add += nfa.states;
        }
        Self::add_transition(&mut transitions, (last_end, Symbol::Empty), 1);
        Nfa {
            start: 0,
            accepting: 1,
            states: inner_states + 2,
            transitions,
        }
    }

    fn nfa_star(nfa: Self) -> Self {
        let mut transitions: NfaTransitions<T> = HashMap::new();
        Self::insert_nfa_transitions(&mut transitions, &nfa, 1);
        transitions.insert((0, Symbol::Empty), vec![nfa.start + 1]);
        Self::add_transition(&mut transitions, (nfa.accepting + 1, Symbol::Empty), 0);
        Nfa {
            start: 0,
            accepting: 0,
            states: nfa.states + 1,
            transitions,
        }
    }
}
