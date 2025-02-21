use crate::ast::display::print_ast;
use crate::ast::generate::build_ast;
use crate::ast::Ast;
use crate::blep::lexer::BlepLexer;
use crate::blep::parser::BlepParser;
use crate::blep::{blep_lexer, blep_parser};

pub struct BlepPipeline {
    lexer: BlepLexer,
    parser: BlepParser,
}

impl BlepPipeline {
    pub fn new() -> Self {
        Self {
            lexer: blep_lexer(),
            parser: blep_parser(),
        }
    }

    pub fn run(&self, input: &str) -> Result<(), String> {
        let tokens = self.lexer.lex(input.to_string());
        let ast = self.parser.parse(tokens);
        println!("{:?}", ast);
        Ok(())
    }

    pub fn parse(&self, input: &str) -> Option<Ast> {
        let tokens = self.lexer.lex(input.to_string());
        let parse_tree = self.parser.parse(tokens).ok()?;
        let ast = build_ast(parse_tree);
        print_ast(&ast).unwrap();

        Some(ast)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    use walkdir::WalkDir;

    #[test]
    fn standalone_examples_are_parsed_by_pipeline() {
        let pipeline = super::BlepPipeline::new();
        for entry in WalkDir::new("examples/standalone")
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.is_file() {
                let mut file = File::open(path).unwrap();
                let mut contents = String::new();
                file.read_to_string(&mut contents).unwrap();
                let result = pipeline.parse(&contents);
                assert!(result.is_some());
            }
        }
    }
}
