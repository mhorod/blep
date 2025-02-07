use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use crate::parsing::automata::{dfa::Dfa, State};
use crate::parsing::grammar::{AnalyzedGrammar, Grammar, Production};
use crate::parsing::parsers::{Token, TokenStream};

pub type ActionTable<T> = HashMap<State, HashMap<T, (T, State)>>;

pub struct LLOneParser<'a, T> {
    grammar: &'a Grammar<T>,
    action_table: ActionTable<T>,
}

#[derive(Debug)]
pub enum ParseTree<T, U> {
    Leaf(U),
    Node(T, Vec<ParseTree<T, U>>),
}

#[derive(Debug)]
pub enum ParseError<T> {
    UnexpectedEof,
    UnexpectedToken(T),
}

enum Action<T> {
    Accept,
    ParseLeaf(State),
    ParseTree(T, State),
    Fail(ParseError<T>),
}

type ParsingResult<T, U> = Result<ParseTree<T, U>, ParseError<T>>;

impl<'a, T: PartialEq + Eq + Hash + Copy + Debug> LLOneParser<'a, T> {
    pub fn new(analyzed_grammar: &'a AnalyzedGrammar<T>) -> Self {
        let mut action_table: ActionTable<T> = ActionTable::new();

        for dfa in analyzed_grammar.grammar.productions.values() {
            for ((state, symbol), next_state) in &dfa.transitions {
                for first_plus_symbol in analyzed_grammar.get_first_plus(symbol) {
                    let state_action_table = action_table.entry(*state).or_default();
                    if state_action_table.contains_key(&first_plus_symbol) {
                        panic!(
                            "Not an LL1 grammar, ambiguous action on ({:?}, {:?})",
                            state, symbol
                        );
                    }
                    state_action_table.insert(first_plus_symbol, (*symbol, *next_state));
                }
            }
        }

        Self {
            grammar: &analyzed_grammar.grammar,
            action_table,
        }
    }

    pub fn parse<U: Token<T>>(&self, mut tokens: TokenStream<U>) -> ParsingResult<T, U> {
        self._parse(self.grammar.start, &mut tokens)
    }

    fn _parse<U: Token<T>>(
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

    fn get_action<U: Token<T>>(
        &'a self,
        dfa: &Dfa<T>,
        state: State,
        tokens: &'a TokenStream<U>,
    ) -> Action<T> {
        match (tokens.peek(), self.action_table.get(&state)) {
            (None, _) | (_, None) => self.accept_or_fail(dfa, state, tokens),
            (Some(token), Some(table)) => match table.get(token.content()) {
                None => self.accept_or_fail(dfa, state, tokens),
                Some((symbol, next_state)) => {
                    if token.content() == symbol {
                        Action::ParseLeaf(*next_state)
                    } else {
                        Action::ParseTree(*symbol, *next_state)
                    }
                }
            },
        }
    }

    fn accept_or_fail<U: Token<T>>(
        &self,
        dfa: &Dfa<T>,
        state: State,
        tokens: &TokenStream<U>,
    ) -> Action<T> {
        if dfa.accepting.contains(&state) {
            Action::Accept
        } else {
            match tokens.peek() {
                None => Action::Fail(ParseError::UnexpectedEof),
                Some(token) => Action::Fail(ParseError::UnexpectedToken(*token.content())),
            }
        }
    }
}
