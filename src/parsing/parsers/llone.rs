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
        self._parse(self.grammar.get_start_production(), &mut tokens)
    }

    fn _parse<U: Token<T>>(
        &self,
        production: Production<T>,
        tokens: &mut TokenStream<U>,
    ) -> ParsingResult<T, U> {
        let (production_symbol, dfa) = production;
        let mut state = dfa.start;
        let mut children: Vec<ParseTree<T, U>> = Vec::new();
        loop {
            let action = tokens.peek().map(|t| t.content()).and_then(|t| {
                self.action_table
                    .get(&state)
                    .and_then(|action_table| action_table.get(t))
                    .map(|r| (t, r))
            });

            if let Some((token, (symbol, next_state))) = action {
                state = *next_state;
                if symbol == token {
                    children.push(ParseTree::Leaf(tokens.take()))
                } else {
                    let child =
                        self._parse(self.grammar.get_production(symbol).unwrap(), tokens)?;
                    children.push(child);
                }
            } else if dfa.accepting.contains(&state) {
                return Ok(ParseTree::Node(*production_symbol, children));
            } else {
                return match tokens.peek() {
                    None => Err(ParseError::UnexpectedEof),
                    Some(t) => Err(ParseError::UnexpectedToken(*t.content())),
                };
            }
        }
    }
}
