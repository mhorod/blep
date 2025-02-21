mod pipeline;
mod ast;
mod automata;
mod blep;
mod graphs;
mod lexing;
mod parsing;
mod regex;

use blep::{blep_lexer, blep_parser};

use ast::display::print_ast;
use ast::generate::build_ast;
use parsing::grammar::{regex::RegexGrammar, Grammar};
use parsing::parsers::llone::ParseTree;

use std::fmt::Debug;

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

fn print_parse_tree<T: Debug, U: Debug>(tree: &ParseTree<T, U>, indent: String, last: bool) {
    print!("{}", indent);
    match tree {
        ParseTree::Leaf(token) => {
            if last {
                println!("└─{:?}", token);
            } else {
                println!("─{:?}", token);
            }
        }
        ParseTree::Node(symbol, children) => {
            if last {
                println!("└─{:?}", symbol);
            } else {
                println!("─{:?}", symbol);
            }
            let child_indent = indent.clone() + "  │";
            for (i, child) in children.iter().enumerate() {
                if i == children.len() - 1 {
                    print_parse_tree(child, indent.clone() + "  ", true);
                } else {
                    print_parse_tree(child, child_indent.clone(), false);
                }
            }
        }
    }
}

fn load_example_from_file(path: &str) -> String {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    let program = if args.len() > 1 {
        load_example_from_file(&args[1])
    } else {
        return;
    };

    let pipeline = pipeline::BlepPipeline::new();
    pipeline.parse(&program).unwrap();
}
