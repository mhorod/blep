use std::fmt::Debug;

use crate::automata::{dfa::Dfa, dfa::Wildcards, State};
use crate::lexing::{Categories, LexerSymbol, Token};

pub struct Lexer<C> {
    automata: Vec<Dfa<LexerSymbol, C>>,
}

struct LexerWildcards {}

impl Wildcards<LexerSymbol> for LexerWildcards {
    fn is_wildcard(&self, symbol: &LexerSymbol) -> bool {
        !matches!(symbol, LexerSymbol::Concrete(_))
    }

    fn matches(&self, wildcard: &LexerSymbol, symbol: &LexerSymbol) -> bool {
        use LexerSymbol::*;
        match (wildcard, symbol) {
            (Concrete(s1), Concrete(s2)) => *s1 == *s2,
            (Lowercase, Concrete(s)) => s.is_lowercase(),
            (Uppercase, Concrete(s)) => s.is_uppercase(),
            (Digit, Concrete(s)) => s.is_ascii_digit(),
            (Not(s1), Concrete(s2)) => *s1 != *s2,
            (_, _) => false,
        }
    }

    fn intersection(&self, s1: &LexerSymbol, s2: &LexerSymbol) -> Option<LexerSymbol> {
        None
    }
}

struct LexingProcess<'a, C> {
    lexer: &'a Lexer<C>,
    wildcards: LexerWildcards,
    index: usize,
    begin_index: usize,
    state: Vec<(usize, State)>,
    last_accepted: Option<(usize, C)>,
    input: Vec<char>,
    tokens: Vec<Token<C>>,
}

impl<C: Copy + Ord + Debug> Lexer<C> {
    pub fn from_categories(categories: Categories<C>) -> Self {
        let wildcards = LexerWildcards {};
        let automata: Vec<Dfa<LexerSymbol, C>> = categories
            .into_iter()
            .map(|c| {
                Dfa::from_regex_with_wildcards(c.regex, &wildcards).map_results(|_| c.category)
            })
            .collect();

        automata.iter().for_each(|a| { println!("{:?}\n", a); } );

        Self { automata }
    }

    pub fn lex(&self, input: String) -> Vec<Token<C>> {
        let mut process = LexingProcess {
            lexer: self,
            wildcards: LexerWildcards {},
            index: 0,
            begin_index: 0,
            state: self.start_state(),
            last_accepted: None,
            input: input.chars().collect(),
            tokens: vec![],
        };
        process.start();
        process.tokens
    }

    fn start_state(&self) -> Vec<(usize, State)> {
        self.automata.iter().map(|a| a.start).enumerate().collect()
    }
}

impl<C: Copy + Ord + Debug> LexingProcess<'_, C> {
    fn start(&mut self) {
        loop {
            if self.index == self.input.len() {
                if self.begin_index == self.index {
                    return;
                } else if self.last_accepted.is_some() {
                    self.rollback_and_accept();
                } else {
                    panic!("Unexpected eof");
                }
            } else {
                let next_state = self.get_next_state(self.input[self.index]);
                if !next_state.is_empty() {
                    self.index += 1;
                    self.state = next_state;
                    self.check_accepting();
                } else if self.last_accepted.is_some() {
                    self.rollback_and_accept();
                } else {
                    panic!("Unexpected character: {:?}", self.input[self.index]);
                }
            }
        }
    }

    fn check_accepting(&mut self) {
        let accepting: Option<C> = self
            .state
            .iter()
            .filter(|(i, s)| self.lexer.automata[*i].is_accepting(*s))
            .map(|(i, s)| *self.lexer.automata[*i].get_result(s))
            .min();

        if let Some(c) = accepting {
            self.last_accepted = Some((self.index, c));
        }
    }

    fn get_next_state(&self, current_char: char) -> Vec<(usize, State)> {
        self.state
            .iter()
            .map(|s| self.get_next_state_for_dfa(*s, current_char))
            .flat_map(|x| x.into_iter())
            .collect()
    }

    fn get_next_state_for_dfa(
        &self,
        state: (usize, State),
        current_char: char,
    ) -> Option<(usize, State)> {
        let (index, state) = state;
        let dfa = &self.lexer.automata[index];
        if let Some(next_state) = dfa.next(state, LexerSymbol::Concrete(current_char)) {
            Some((index, next_state))
        } else if let Some(symbols) = dfa.get_symbols_from_state(state) {
            let wildcard = symbols
                .iter()
                .filter(|w| {
                    self.wildcards
                        .matches(w, &LexerSymbol::Concrete(current_char))
                })
                .min();
            wildcard
                .and_then(|w| dfa.next(state, *w))
                .map(|next_state| (index, next_state))
        } else {
            None
        }
    }

    fn rollback_and_accept(&mut self) {
        let (char_index, category) = self.last_accepted.unwrap();
        let content: String = self.input[self.begin_index..char_index].iter().collect();
        self.tokens.push(Token {
            content,
            category,
            range: self.begin_index..char_index,
        });
        self.index = char_index;
        self.begin_index = char_index;
        self.last_accepted = None;
        self.state = self.lexer.start_state();
    }
}
