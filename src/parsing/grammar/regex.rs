use crate::automata::dfa::*;
use crate::parsing::grammar::*;
use crate::regex::Regex;
use std::hash::Hash;

type Dfa<T> = dfa::Dfa<T, ()>;

pub type Productions<T> = HashMap<T, Regex<T>>;
pub struct RegexGrammar<T> {
    pub start: T,
    pub productions: Productions<T>,
}

fn add_to_states<T: Clone + Hash + Eq>(dfa: Dfa<T>, add: u32) -> Dfa<T> {
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

    Dfa::new(start, transitions, accepting, states)
}

impl<T: Clone + Eq + Hash + Debug> From<RegexGrammar<T>> for Grammar<T> {
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
