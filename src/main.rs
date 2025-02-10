mod automata;
mod graphs;
mod lexing;
mod parsing;
mod regex;

mod blep;

use blep::{blep_lexer, blep_parser};

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

fn main() {
    let program = r#"
        struct Foo[X](x: X, y: Int)
        interface I { fun clone() -> Int }
        class Bar(x: Int, y: Int) implements I {
            fun foo() = 0
        }
        
        fun f(a: A) = a
        "#;

    let lexer = blep_lexer();
    let lexed = lexer.lex(program.to_string());
    lexed.iter().for_each(|t| println!("{:?}", t));

    let parser = blep_parser();
    let parsed = parser.parse(lexed);

    match parsed {
        Ok(parse_tree) => print_parse_tree(&parse_tree, "".to_string(), true),
        Err(err) => println!("Error: {:?}", err),
    };
}
