mod automata;
mod graphs;
mod lexing;
mod parsing;
mod regex;
mod ast;
mod blep;

use blep::{blep_lexer, blep_parser};

use parsing::grammar::{regex::RegexGrammar, Grammar};
use parsing::parsers::llone::ParseTree;
use ast::generate::build_ast;

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
        struct Foo(internal mut x: Int, y: Int)
        interface I {
            pure fun f(self: Self, x: Int) -> Int
        }

        class C(a: Int) implements I {
            public fun new(a) = C(a)
            public pure fun f(self, x) = let y = a * x in x + y
        }

        fun ptrs(ref: &Int, ptr: *mut Int) = { *ptr = *ref; }

        fun main() = C::new(10).f(42)
        "#;


    let program2 = r#"
        struct S(x: Int) {}
        "#;

    let lexer = blep_lexer();
    let lexed = lexer.lex(program2.to_string());
    lexed.iter().for_each(|t| println!("{:?}", t));

    let parser = blep_parser();
    let parsed = parser.parse(lexed);

    match parsed {
        Ok(parse_tree) => {
            print_parse_tree(&parse_tree, "".to_string(), true);
            let ast = build_ast(parse_tree);
            println!("AST: {:?}", ast);
        },
        Err(err) => println!("Error: {:?}", err),
    };
}
