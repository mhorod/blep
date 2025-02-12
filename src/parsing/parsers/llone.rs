use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use crate::automata::{dfa, State};
use crate::parsing::grammar::{AnalyzedGrammar, Grammar};
use crate::parsing::parsers::{Categorized, TokenStream};

type Dfa<T> = dfa::Dfa<T, ()>;

pub type ActionTable<T> = HashMap<State, HashMap<T, (T, State)>>;

pub struct LLOneParser<T> {
    grammar: Grammar<T>,
    action_table: ActionTable<T>,
}

#[derive(Debug)]
pub enum ParseTree<T, U> {
    Leaf(U),
    Node(T, Vec<ParseTree<T, U>>),
}

impl <T, U: Categorized<T>> ParseTree<T, U> {
    pub fn get_category(&self) -> &T {
        match self {
            ParseTree::Leaf(token) => token.get_category(),
            ParseTree::Node(symbol, _) => symbol
        }
    }
}

#[derive(Debug)]
pub enum ParseError<T> {
    UnexpectedEof,
    UnexpectedToken(T),
}

enum Action<T, U> {
    Accept,
    ParseLeaf(State),
    ParseTree(T, State),
    Fail(ParseError<U>),
}

pub type ParsingResult<T, U> = Result<ParseTree<T, U>, ParseError<U>>;

impl<T: PartialEq + Eq + Hash + Copy + Debug> LLOneParser<T> {
    pub fn new(analyzed_grammar: AnalyzedGrammar<T>) -> Self {
        let mut action_table: ActionTable<T> = ActionTable::new();

        for (production_symbol, dfa) in analyzed_grammar.grammar.productions.iter() {
            for ((state, symbol), next_state) in &dfa.transitions {
                for first_plus_symbol in analyzed_grammar.get_first_plus(symbol) {
                    let state_action_table = action_table.entry(*state).or_default();
                    if let Some(existing) = state_action_table.get(&first_plus_symbol) {
                        panic!(
                            "Not an LL1 grammar, ambiguous action on symbol {:?} in state {:?} in production {:?}. Existing action: {:?}",
                            first_plus_symbol, state, production_symbol, existing
                        );
                    }
                    state_action_table.insert(first_plus_symbol, (*symbol, *next_state));
                }
            }
        }

        Self {
            grammar: analyzed_grammar.grammar,
            action_table,
        }
    }

    pub fn parse<U: Categorized<T> + Clone>(&self, mut tokens: TokenStream<U>) -> ParsingResult<T, U> {
        self._parse(self.grammar.start, &mut tokens)
    }

    fn _parse<U: Categorized<T> + Clone>(
        &self,
        production_symbol: T,
        tokens: &mut TokenStream<U>,
    ) -> ParsingResult<T, U> {
        let dfa = self.grammar.productions.get(&production_symbol).unwrap();
        let mut state = dfa.start;
        let mut children: Vec<ParseTree<T, U>> = Vec::new();
        loop {
            match self.get_action(dfa, state, tokens) {
                Action::Accept => return Ok(ParseTree::Node(production_symbol, children)),
                Action::Fail(error) => return Err(error),
                Action::ParseLeaf(next_state) => {
                    state = next_state;
                    children.push(ParseTree::Leaf(tokens.take()))
                }
                Action::ParseTree(symbol, next_state) => {
                    state = next_state;
                    children.push(self._parse(symbol, tokens)?);
                }
            }
        }
    }

    fn get_action<U: Categorized<T> + Clone>(
        &self,
        dfa: &Dfa<T>,
        state: State,
        tokens: &TokenStream<U>,
    ) -> Action<T, U> {
        match (tokens.peek(), self.action_table.get(&state)) {
            (None, _) | (_, None) => self.accept_or_fail(dfa, state, tokens),
            (Some(token), Some(table)) => match table.get(token.get_category()) {
                None => self.accept_or_fail(dfa, state, tokens),
                Some((symbol, next_state)) => {
                    if token.get_category() == symbol {
                        Action::ParseLeaf(*next_state)
                    } else {
                        Action::ParseTree(*symbol, *next_state)
                    }
                }
            },
        }
    }

    fn accept_or_fail<U: Categorized<T> + Clone>(
        &self,
        dfa: &Dfa<T>,
        state: State,
        tokens: &TokenStream<U>,
    ) -> Action<T, U> {
        if dfa.is_accepting(state) {
            Action::Accept
        } else {
            match tokens.peek() {
                None => Action::Fail(ParseError::UnexpectedEof),
                Some(token) => Action::Fail(ParseError::UnexpectedToken(token.clone())),
            }
        }
    }
}
