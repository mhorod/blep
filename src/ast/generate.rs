use super::Ast;
use super::*;
use crate::blep::tokens::{KeywordKind, TokenCategory};
use crate::lexing::Token;
use crate::parsing::parsers::llone;
use crate::{blep::grammar::GrammarSymbol, parsing::parsers::Categorized};
use std::collections::VecDeque;

use llone::ParseTree::{Leaf, Node};

macro_rules! kw {
    ($token:ident) => {
        GrammarSymbol::Token(TokenCategory::Keyword(KeywordKind::$token))
    };
}

macro_rules! open_delim {
    ($token:ident) => {
        GrammarSymbol::Token(TokenCategory::OpenDelim(DelimKind::$token))
    };
}

macro_rules! match_node {
    ($tree:expr, ($symbol:path, $children:ident) => $arm:expr) => {
        match $tree {
            $crate::parsing::parsers::llone::ParseTree::Node($symbol, $children) => $arm,
            _ => panic!("Unexpected leaf"),
        }
    };

    ($tree:expr, ($symbol:path, mut $children:ident) => $arm:expr) => {
        match $tree {
            $crate::parsing::parsers::llone::ParseTree::Node($symbol, mut $children) => $arm,
            _ => panic!("Unexpected leaf"),
        }
    };
}

macro_rules! match_leaf {
    ($tree:expr, ($token:ident) => $arm:expr) => {
        match $tree {
            $crate::parsing::parsers::llone::ParseTree::Leaf($token) => $arm,
            _ => panic!("Unexpected node"),
        }
    };
}

type ParseTree = llone::ParseTree<GrammarSymbol, Token<GrammarSymbol>>;

pub fn build_ast(parse_tree: ParseTree) -> Ast {
    AstBuilder::new().build_ast(parse_tree)
}

struct AstBuilder {
    next_node_id: u32,
}

impl AstBuilder {
    fn new() -> Self {
        Self { next_node_id: 0 }
    }

    fn get_next_node_id(&mut self) -> AstNodeId {
        let result = AstNodeId(self.next_node_id);
        self.next_node_id += 1;
        result
    }

    fn build_ast(&mut self, parse_tree: ParseTree) -> Ast {
        match_node!(parse_tree, (GrammarSymbol::Program, children) => {
                let items: Vec<Item> = children
                    .into_iter()
                    .take_while(|c| c.get_category() != &GrammarSymbol::Token(TokenCategory::Eof))
                    .map(|c| self.build_item(c))
                    .collect();

                Ast { items }
            }
        )
    }

    fn build_item(&mut self, parse_tree: ParseTree) -> Item {
        match_node!(parse_tree, (GrammarSymbol::Item, mut children) => match children.len() {
                1 => {
                    let id = self.get_next_node_id();
                    let visibility = Visibility {
                        span: Span::dummy(),
                        kind: VisibilityKind::Internal,
                    };
                    let kind = self.build_item_kind(children.remove(0));
                    let span = kind.span();

                    Item {
                        id,
                        span,
                        visibility,
                        kind,
                    }
                }
                2 => {
                    let id = self.get_next_node_id();
                    let visibility = self.build_visibility(children.remove(0));
                    let kind = self.build_item_kind(children.remove(0));
                    let span = Span::wrap(vec![visibility.span, kind.span()]);

                    Item {
                        id,
                        span,
                        visibility,
                        kind,
                    }
                }
                _ => panic!("Item should have 1 or 2 children"),
            }
        )
    }

    fn build_visibility(&self, parse_tree: ParseTree) -> Visibility {
        match_node!(parse_tree, (GrammarSymbol::Visibility, mut children) => {
            match_leaf!(children.remove(0), (token) => {
                    let kind = match token.get_category() {
                        kw!(Public) => VisibilityKind::Public,
                        kw!(Private) => VisibilityKind::Private,
                        kw!(Internal) => VisibilityKind::Internal,
                        _ => panic!("Unexpected visibility keyword"),
                    };
                    Visibility {
                        span: token.range.into(),
                        kind,
                    }
                }
            )
        }
        )
    }

    fn build_item_kind(&mut self, parse_tree: ParseTree) -> ItemKind {
        use GrammarSymbol::*;
        match parse_tree {
            Leaf(..) => panic!("ItemKind parse tree cannot be a leaf"),
            Node(symbol, _) => match symbol {
                FunDef => todo!(),
                EnumDef => ItemKind::Enum(self.build_enum(parse_tree)),
                StructDef => ItemKind::Struct(self.build_struct(parse_tree)),
                NewTypeDef => todo!(),
                TypeAliasDef => todo!(),
                InterfaceDef => todo!(),
                _ => panic!("Unexpected ItemKind keyword"),
            },
        }
    }

    fn build_enum(&mut self, parse_tree: ParseTree) -> Enum {
        todo!()
    }

    fn build_struct(&mut self, parse_tree: ParseTree) -> Struct {
        match_node! (parse_tree, (GrammarSymbol::StructDef, mut children) => {
                let (kind, kind_span): (StructKind, Span) = match children.remove(0) {
                    Node(..) => panic!("Unexpected struct kind"),
                    Leaf(token) => match token.get_category() {
                        kw!(Struct) => (StructKind::Struct, token.range.into()),
                        kw!(Class) => (StructKind::Class, token.range.into()),
                        _ => panic!("Unexpected struct kind"),
                    },
                };

                let ident: String = match_leaf!(children.remove(0), (token) => token.content);

                let (generic_params, generic_params_span) =
                    if matches!(children[0].get_category(), GrammarSymbol::GenericParams) {
                        self.build_generic_params(children.remove(0))
                    } else {
                        (Vec::new(), Span::dummy())
                    };
                let default_visibility = match kind {
                    StructKind::Struct => VisibilityKind::Public,
                    StructKind::Class => VisibilityKind::Private
                    };
                let (fields, fields_span) = self.build_struct_fields(children.remove(0), default_visibility);
                let implements: Vec<Ty> = Vec::new();
                let (methods, methods_span) = self.build_struct_methods(children.remove(0));

                let span = Span::wrap(vec![kind_span, generic_params_span, fields_span, methods_span]);
                Struct {
                    span,
                    ident,
                    kind,
                    generic_params,
                    fields,
                    implements,
                    methods,
                }
        }
        )
    }

    fn build_generic_params(&mut self, parse_tree: ParseTree) -> (Vec<TyName>, Span) {
        match_node!(parse_tree, (GrammarSymbol::GenericParams, children) => {
               Self::build_delimited(children, |node| self.build_ty_name(node))
            }
        )
    }

    fn build_ty_name(&mut self, parse_tree: ParseTree) -> TyName {
        match_leaf!(parse_tree, (token) => TyName {
            id: self.get_next_node_id(),
            ident: token.content,
            span: token.range.into()
        })
    }

    fn build_struct_fields(
        &mut self,
        parse_tree: ParseTree,
        default_visibility: VisibilityKind,
    ) -> (Vec<StructField>, Span) {
        match_node!(parse_tree, (GrammarSymbol::StructFields, children) => {
                Self::build_delimited(children, |node| self.build_struct_field(node, default_visibility))
            }
        )
    }

    fn build_struct_field(
        &mut self,
        parse_tree: ParseTree,
        default_visibility: VisibilityKind,
    ) -> StructField {
        match_node! (parse_tree, (GrammarSymbol::StructField, mut children) => {
            let id = self.get_next_node_id();
            let visibility = if children[0].get_category() == &GrammarSymbol::Visibility {
                self.build_visibility(children.remove(0))
            } else {
                Visibility{ kind: default_visibility, span: Span::dummy() }
            };

            let mutability = if children[0].get_category() == &kw!(Mut) {
                Mutability{
                    kind: MutabilityKind::Mutable, span: Self::get_token_span(children.remove(0)) }
            } else {
                Mutability{ kind: MutabilityKind::Immutable, span: Span::dummy() }
            };

            let ident = match_leaf!(children.remove(0), (token) => token.content);
            children.remove(0); // Colon
            let ty = self.build_ty(children.remove(0));

            StructField {
                id,
                visibility,
                mutability,
                ident,
                ty
            }
        })
    }

    fn build_struct_methods(&mut self, parse_tree: ParseTree) -> (Vec<StructMethod>, Span) {
        match_node!(parse_tree, (GrammarSymbol::StructMethods, children) => {
                Self::build_delimited(children, |node| self.build_struct_method(node))
            }
        )
    }

    fn build_struct_method(&mut self, parse_tree: ParseTree) -> StructMethod {
        todo!()
    }

    fn build_ty(&mut self, parse_tree: ParseTree) -> Ty {
        let id = self.get_next_node_id();
        let (kind, span) = match_node!(parse_tree, (GrammarSymbol::Type, mut children) => {
            assert_eq!(children.len(), 1);
            match children.remove(0) {
                Node(GrammarSymbol::PtrType, children) => todo!(),
                Node(GrammarSymbol::RefType, children) => todo!(),
                Node(GrammarSymbol::FunOrTupleType, children) => todo!(),
                Node(GrammarSymbol::QualifiedGenericType, children) => self.build_primitive_ty(children),
                _ => panic!("Unexpected type parse tree")
            }
        });
        Ty { id, kind, span }
    }

    fn build_primitive_ty(&mut self, children: Vec<ParseTree>) -> (TyKind, Span) {
        let mut children: VecDeque<ParseTree> = children.into_iter().collect();
        let mut path: Vec<TyName> = Vec::new();
        path.push(self.build_ty_name(children.pop_front().unwrap()));

        while children.len() >= 2 {
            children.pop_front(); // DoubleColon
            path.push(self.build_ty_name(children.pop_front().unwrap()));
        }

        let (generic_args, generic_args_span) = if let Some(child) = children.pop_front() {
            self.build_generic_args(child)
        } else {
            (Vec::new(), Span::dummy())
        };

        let span = Span::wrap(vec![
            path[0].span,
            path[path.len() - 1].span,
            generic_args_span,
        ]);
        let kind = PrimitiveTy { path, generic_args };
        (TyKind::Primitive(kind), span)
    }

    fn build_generic_args(&mut self, tree: ParseTree) -> (Vec<Ty>, Span) {
        match_node!(tree, (GrammarSymbol::GenericArgs, children) =>
            Self::build_delimited(children, |node| self.build_ty(node))
        )
    }

    fn build_delimited<T, F: FnMut(ParseTree) -> T>(
        mut children: Vec<ParseTree>,
        child_builder: F,
    ) -> (Vec<T>, Span) {
        let open = children.remove(0);
        let close = children.pop().unwrap();
        let span = Span::wrap(vec![
            Self::get_token_span(open),
            Self::get_token_span(close),
        ]);
        let result: Vec<T> = children.into_iter().map(child_builder).collect();

        (result, span)
    }

    fn get_token_span(token: ParseTree) -> Span {
        match_leaf!(token, (token) => { token.range.into() })
    }
}
