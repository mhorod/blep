mod graphs;
mod parsing;

use parsing::grammar::{analyze, regex::RegexGrammar, Grammar};
use parsing::parsers::llone::{LLOneParser, ParseTree};
use parsing::parsers::TokenStream;
use std::fmt::Debug;

use crate::parsing::automata::dfa::Dfa;
use crate::parsing::automata::nfa::Nfa;
use crate::parsing::regex::Regex::*;

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
    let re_grammar = regex_grammar!(
        "E",
        "E" => (Atom("T") + Atom("-") + Atom("E")) | Atom("T"),
        "T" => (Atom("(") + Atom("E") + Atom(")")) | Atom("Var"),
        "Var" => Atom("x") | Atom("y")
    );

    println!("Regex grammar:");
    print_regex_grammar(&re_grammar);

    println!();
    println!("Deregexed:");

    let grammar = Grammar::from(re_grammar);
    print_grammar(&grammar);

    let analyzed_grammar = analyze(grammar);

    let llone_parser = LLOneParser::new(&analyzed_grammar);
    let chars = ["x", "-", "(", "x", "-", "y", ")"];
    let result = llone_parser.parse(TokenStream::from_iter(chars));
    println!("{:?}", result);
    if let Ok(tree) = result {
        print_parse_tree(&tree, 0);
    }
}
