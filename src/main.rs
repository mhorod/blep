mod regex;
mod automata;
mod lexing;
mod graphs;
mod parsing;

mod blep;

use blep::{blep_lexer, blep_parser};

use parsing::grammar::{regex::RegexGrammar, Grammar};
use parsing::parsers::llone::{ParseTree};
use std::fmt::Debug;

use crate::automata::dfa::Dfa;
use crate::automata::nfa::Nfa;
use crate::regex::Regex::*;

fn automatons() {
    let re = Atom("a") + (Atom("b") | Atom("c")).star();
    println!("{:?}", re);
    let nfa = Nfa::from_regex(re);
    println!("{:?}", nfa);
    let dfa = Dfa::from_nfa(nfa);
    println!("{:?}", dfa);
}

fn print_regex_grammar<T: std::fmt::Debug>(grammar: &RegexGrammar<T>) {
    println!("Start: {:?}", grammar.start);
    for (symbol, regex) in &grammar.productions {
        println!("{:?} -> {:?}", symbol, regex);
    }
}

fn print_grammar<T: std::fmt::Debug>(grammar: &Grammar<T>) {
    println!("Start: {:?}", grammar.start);
    for (symbol, dfa) in &grammar.productions {
        println!("{:?} -> {:?}", symbol, dfa);
    }
}

fn print_parse_tree<T: Debug, U: Debug>(tree: &ParseTree<T, U>, indent: usize) {
    print!("{}", "  ".repeat(indent));
    match tree {
        ParseTree::Leaf(token) => {
            println!("{:?}", token);
        }
        ParseTree::Node(symbol, children) => {
            println!("{:?}", symbol);
            for child in children {
                print_parse_tree(child, indent + 1);
            }
        }
    }
}

fn main() {
    let lexer = blep_lexer();
    let lexed = lexer.lex("let x = 1 in x".to_string());
    lexed.iter().for_each(|t| println!("{:?}", t));

    let parser = blep_parser();
    let parsed = parser.parse(lexed);
    
    match parsed {
        Ok(parse_tree) => print_parse_tree(&parse_tree, 0),
        Err(err) => println!("Error: {:?}", err)
    };
}

