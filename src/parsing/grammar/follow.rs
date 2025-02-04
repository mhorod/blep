extern crate itertools;

use std::fmt::Debug;
use std::hash::Hash;

use itertools::Itertools;

use crate::parsing::automata::State;
use crate::parsing::grammar::Grammar;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum Fact<T> {
    Follows(T, T),
    FirstPlusOf(T, State),
}

struct FollowFinder<'a, T> {
    accepting_states: HashMap<T, &'a HashSet<State>>,
    entering_transitions: HashMap<State, Vec<T>>,
    reversed_nullable_transitions: HashMap<State, Vec<State>>,
    first: &'a HashMap<State, HashSet<T>>,
}

pub fn find_follow_symbols<T: Eq + Hash + Copy + Debug>(
    grammar: &Grammar<T>,
    nullable: &HashSet<T>,
    first: &HashMap<State, HashSet<T>>,
) -> HashMap<T, HashSet<T>> {
    FollowFinder::new(grammar, nullable, first).find_follow_symbols()
}

impl<'a, T: Eq + Hash + Copy + Debug> FollowFinder<'a, T> {
    fn new(
        grammar: &'a Grammar<T>,
        nullable: &'a HashSet<T>,
        first: &'a HashMap<State, HashSet<T>>,
    ) -> Self {
        Self {
            accepting_states: get_accepting_states(grammar),
            entering_transitions: get_entering_transitions(grammar),
            reversed_nullable_transitions: get_reversed_nullable_transitions(grammar, nullable),
            first,
        }
    }

    fn find_follow_symbols(self) -> HashMap<T, HashSet<T>> {
        let mut facts: HashSet<Fact<T>> = HashSet::new();
        let mut facts_to_process: VecDeque<Fact<T>> = self.initial_facts();

        while let Some(current_fact) = facts_to_process.pop_front() {
            if facts.contains(&current_fact) {
                continue;
            }
            facts_to_process.extend(self.fact_implications(&current_fact));
            facts.insert(current_fact);
        }

        follow_from_facts(facts)
    }

    fn initial_facts(&self) -> VecDeque<Fact<T>> {
        self.first
            .iter()
            .flat_map(|(state, symbols)| {
                symbols
                    .iter()
                    .map(|symbol| Fact::FirstPlusOf(*symbol, *state))
            })
            .collect()
    }

    fn fact_implications(&self, fact: &Fact<T>) -> Vec<Fact<T>> {
        match fact {
            Fact::Follows(before, after) => self
                .accepting_states
                .get(before)
                .into_iter()
                .flat_map(|x| x.iter())
                .map(|s| Fact::FirstPlusOf(*after, *s))
                .collect(),

            Fact::FirstPlusOf(symbol, state) => {
                let follow_implications = self
                    .entering_transitions
                    .get(state)
                    .into_iter()
                    .flat_map(|x| x.iter())
                    .map(|s| Fact::Follows(*s, *symbol));

                let first_implications = self
                    .reversed_nullable_transitions
                    .get(state)
                    .into_iter()
                    .flat_map(|x| x.iter())
                    .map(|s| Fact::FirstPlusOf(*symbol, *s));

                follow_implications.chain(first_implications).collect()
            }
        }
    }
}

fn get_accepting_states<T: Eq + Hash + Copy>(grammar: &Grammar<T>) -> HashMap<T, &HashSet<State>> {
    grammar
        .productions
        .iter()
        .map(|p| (p.symbol, &p.produces.accepting))
        .collect()
}

fn get_entering_transitions<T: Eq + Hash + Copy>(grammar: &Grammar<T>) -> HashMap<State, Vec<T>> {
    grammar
        .productions
        .iter()
        .flat_map(|p| p.produces.transitions.iter())
        .map(|((_, symbol), next_state)| (*next_state, *symbol))
        .into_grouping_map()
        .collect()
}

fn get_reversed_nullable_transitions<T: Eq + Hash + Copy>(
    grammar: &Grammar<T>,
    nullable: &HashSet<T>,
) -> HashMap<State, Vec<State>> {
    grammar
        .productions
        .iter()
        .flat_map(|p| p.produces.transitions.iter())
        .filter(|((_, symbol), _)| nullable.contains(symbol))
        .map(|((state, _), next_state)| (*next_state, *state))
        .into_grouping_map()
        .collect()
}

fn follow_from_facts<T: Eq + Hash + Copy>(facts: HashSet<Fact<T>>) -> HashMap<T, HashSet<T>> {
    facts
        .into_iter()
        .flat_map(|fact| follow_from_fact(fact).into_iter())
        .into_grouping_map()
        .collect()
}

fn follow_from_fact<T: Eq + Hash + Copy>(fact: Fact<T>) -> Option<(T, T)> {
    match fact {
        Fact::Follows(before, after) => Some((before, after)),
        _ => None,
    }
}
