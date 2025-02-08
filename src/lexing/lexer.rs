use crate::automata::{dfa::Dfa, State};
use crate::lexing::{Categories, LexerSymbol, Token};

pub struct Lexer<C> {
    dfa: Dfa<LexerSymbol, C>,
}

struct LexingProcess<'a, C> {
    lexer: &'a Lexer<C>,
    index: usize,
    begin_index: usize,
    state: State,
    last_accepted: Option<(usize, State)>,
    input: Vec<char>,
    tokens: Vec<Token<C>>,
}

impl<C: Copy + Ord> Lexer<C> {
    pub fn from_categories(categories: Categories<C>) -> Self {
        let automata: Vec<Dfa<LexerSymbol, C>> = categories
            .into_iter()
            .map(|c| Dfa::from_regex(c.regex).map_results(|_| c.category))
            .collect();
        Self {
            dfa: Dfa::joined(automata).map_results(|r| r.into_iter().min().unwrap()),
        }
    }

    pub fn lex(&self, input: String) -> Vec<Token<C>> {
        let mut process = LexingProcess {
            lexer: self,
            index: 0,
            begin_index: 0,
            state: self.dfa.start,
            last_accepted: None,
            input: input.chars().collect(),
            tokens: vec![],
        };
        process.start();
        process.tokens
    }
}

impl<C: Copy> LexingProcess<'_, C> {
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
            } else if let Some(next_state) = self.get_next_state(self.input[self.index]) {
                self.index += 1;
                self.state = next_state;
                if self.lexer.dfa.is_accepting(self.state) {
                    self.last_accepted = Some((self.index, self.state));
                }
            } else if self.last_accepted.is_some() {
                self.rollback_and_accept();
            } else {
                panic!("Unexpected character: {:?}", self.input[self.index]);
            }
        }
    }

    fn get_next_state(&self, current_char: char) -> Option<State> {
        if let Some(next_state) = self
            .lexer
            .dfa
            .next(self.state, LexerSymbol::Concrete(current_char))
        {
            Some(next_state)
        } else if let Some(category) = Self::get_category(current_char) {
            self.lexer.dfa.next(self.state, category)
        } else {
            None
        }
    }

    fn rollback_and_accept(&mut self) {
        let (index, state) = self.last_accepted.unwrap();
        let content: String = self.input[self.begin_index..index].iter().collect();
        let category = *self.lexer.dfa.get_result(&state);

        self.tokens.push(Token { content, category });
        self.index = index;
        self.begin_index = index;
        self.last_accepted = None;
        self.state = self.lexer.dfa.start;
    }

    fn get_category(c: char) -> Option<LexerSymbol> {
        if c.is_lowercase() {
            Some(LexerSymbol::Lowercase)
        } else if c.is_uppercase() {
            Some(LexerSymbol::Uppercase)
        } else if c.is_ascii_digit() {
            Some(LexerSymbol::Digit)
        } else {
            None
        }
    }
}
