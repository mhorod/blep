mod graphs;
mod parsing;

use parsing::grammar::{analyze, regex::RegexGrammar, Grammar};

use crate::parsing::automata::dfa::Dfa;
use crate::parsing::automata::nfa::Nfa;
use crate::parsing::regex::Regex::*;

fn automatons() {
    // let re = Regex::Atom("a") | (Regex::Atom("b") + Regex::Atom("c")).star();
    let re = Atom("a") + (Atom("b") | Atom("c")).star();
    println!("{:?}", re);
    let nfa = Nfa::from_regex(re);
    println!("{:?}", nfa);
    let dfa = Dfa::from_nfa(nfa);
    println!("{:?}", dfa);
}

fn print_regex_grammar<T: std::fmt::Debug>(grammar: &RegexGrammar<T>) {
    println!("Start: {:?}", grammar.start);
    for prod in &grammar.productions {
        println!("{:?} -> {:?}", prod.symbol, prod.produces);
    }
}

fn print_grammar<T: std::fmt::Debug>(grammar: &Grammar<T>) {
    println!("Start: {:?}", grammar.start);
    for prod in &grammar.productions {
        println!("{:?} -> {:?}", prod.symbol, prod.produces);
    }
}

fn main() {
    let re_grammar = regex_grammar!(
        "S",
        "S" => (Atom("B") | Atom("c")) + Atom("a"),
        "B" =>  Atom("d").star() | Atom("C"),
        "C" => Atom("e")
    );

    println!("Regex grammar:");
    print_regex_grammar(&re_grammar);

    println!();
    println!("Deregexed:");

    let grammar = Grammar::from(re_grammar);
    print_grammar(&grammar);

    analyze(&grammar);
}
