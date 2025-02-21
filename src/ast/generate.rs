use super::Ast;
use super::*;
use crate::blep::tokens::{KeywordKind, LiteralKind, SymbolKind, TokenCategory};
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

macro_rules! symb {
    ($token:ident) => {
        GrammarSymbol::Token(TokenCategory::Symbol(SymbolKind::$token))
    };
}

macro_rules! match_node {
    ($tree:expr, ($($symbol:path)|+, $children:ident) => $arm:expr) => {
        match $tree {
            $($crate::parsing::parsers::llone::ParseTree::Node($symbol, $children) => $arm)+,
            _ => panic!("Unexpected leaf: {:?}", $tree),
        }
    };

    ($tree:expr, ($($symbol:path)|+, mut $children:ident) => $arm:expr) => {
        match $tree {
            $($crate::parsing::parsers::llone::ParseTree::Node($symbol, mut $children) => $arm)+,
            _ => panic!("Unexpected leaf: {:?}", $tree),
        }
    };
}

macro_rules! match_leaf {
    ($tree:expr, ($token:ident) => $arm:expr) => {
        match $tree {
            $crate::parsing::parsers::llone::ParseTree::Leaf($token) => $arm,
            _ => panic!("Unexpected node: {:?}", $tree),
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

    fn build_purity(&self, parse_tree: ParseTree) -> Purity {
        match_leaf!(parse_tree, (token) => {
            let kind = match token.get_category() {
                kw!(Pure) => PurityKind::Pure,
                _ => panic!("Unexpected purity keyword")
            };
            Purity { span : token.range.into(), kind }
        })
    }

    fn build_mutability(&self, parse_tree: ParseTree) -> Mutability {
        match_leaf!(parse_tree, (token) => {
            let kind = match token.get_category() {
                kw!(Mut) => MutabilityKind::Mutable,
                _ => panic!("Unexpected mutability keyword")
            };
            Mutability { span : token.range.into(), kind }
        })
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
                FunDef => ItemKind::Fun(self.build_fun(parse_tree)),
                EnumDef => ItemKind::Enum(self.build_enum(parse_tree)),
                StructDef => ItemKind::Struct(self.build_struct(parse_tree)),
                InterfaceDef => ItemKind::Interface(self.build_interface(parse_tree)),
                NewTypeDef => todo!(),
                TypeAliasDef => todo!(),
                _ => panic!("Unexpected ItemKind keyword"),
            },
        }
    }

    fn build_interface(&mut self, parse_tree: ParseTree) -> Interface {
        match_node!(parse_tree, (GrammarSymbol::InterfaceDef, mut children) => {
            children.remove(0); // Interface
            let ident = match_leaf!(children.remove(0), (token) => token.content);
            let (generic_params, generic_params_span) =
                if matches!(children[0].get_category(), GrammarSymbol::GenericParams) {
                    self.build_generic_params(children.remove(0))
                } else {
                    (Vec::new(), Span::dummy())
                };

            let (methods, methods_span) = self.build_interface_methods(children.remove(0));
            let span = Span::wrap(vec![generic_params_span, methods_span]);

            Interface { ident, span, generic_params, methods }
        })
    }

    fn build_interface_methods(&mut self, parse_tree: ParseTree) -> (Vec<InterfaceMethod>, Span) {
        match_node!(parse_tree, (GrammarSymbol::InterfaceMethods, children) => {
            Self::build_delimited_interspersed(children, |node| self.build_interface_method(node))
        })
    }

    fn build_interface_method(&mut self, parse_tree: ParseTree) -> InterfaceMethod {
        match_node!(parse_tree, (GrammarSymbol::InterfaceMethod, mut children) => {
            let id = self.get_next_node_id();
            let staticity = if children[0].get_category() == &kw!(Static) {
                Staticity { span: Self::get_token_span(&children.remove(0)), kind: StaticityKind::Static }
            } else {
                Staticity { span: Span::dummy(), kind: StaticityKind::Instance }
            };

            let mutability = if children[0].get_category() == &kw!(Mut) {
                self.build_mutability(children.remove(0))
            } else {
                Mutability { span: Span::dummy(), kind: MutabilityKind::Immutable }
            };

            let purity = if children[0].get_category() == &kw!(Pure) {
                self.build_purity(children.remove(0))
            } else {
                Purity { span: Span::dummy(), kind: PurityKind::Impure }
            };
            let fun_span = Self::get_token_span(&children.remove(0));
            let ident: String = match_leaf!(children.remove(0), (token) => token.content);
            let (params, params_span) = self.build_fun_params(children.remove(0));
            children.remove(0); // Arrow
            let return_ty = self.build_ty(children.remove(0));

            let span = Span::wrap(vec![staticity.span, purity.span, fun_span, params_span, return_ty.span]);
            InterfaceMethod {
                id,
                ident,
                span,
                purity,
                staticity,
                mutability,
                params,
                return_ty
            }
        })
    }

    fn build_fun(&mut self, parse_tree: ParseTree) -> Fun {
        match_node!(parse_tree, (GrammarSymbol::FunDef, mut children) => {
            let id = self.get_next_node_id();
            let purity = if children[0].get_category() == &kw!(Pure) {
                self.build_purity(children.remove(0))
            } else {
                Purity { span: Span::dummy(), kind: PurityKind::Impure }
            };
            let fun_span = Self::get_token_span(&children.remove(0));
            let ident: String = match_leaf!(children.remove(0), (token) => token.content);
            let (generic_params, generic_params_span) =
                if matches!(children[0].get_category(), GrammarSymbol::GenericParams) {
                    self.build_generic_params(children.remove(0))
                } else {
                    (Vec::new(), Span::dummy())
                };

            let (params, params_span) = self.build_fun_params(children.remove(0));
            let return_ty = if matches!(children[0].get_category(), symb!(Arrow)) {
                children.remove(0);
                self.build_ty(children.remove(0))
            } else {
                Ty::omitted(self.get_next_node_id())
            };
            children.remove(0); // Eq
            let expr = self.build_expr(children.remove(0));

            let span = Span::wrap(vec![purity.span, fun_span, generic_params_span, params_span, expr.span]);

            Fun { id,  ident, span, purity, generic_params, params, return_ty, expr }
        })
    }

    fn build_fun_params(&mut self, parse_tree: ParseTree) -> (Vec<FunParam>, Span) {
        match_node!(parse_tree, (GrammarSymbol::FunParams | GrammarSymbol::TypedFunParams, children) => {
            Self::build_delimited_interspersed(children, |node| self.build_fun_param(node))
        })
    }

    fn build_fun_param(&mut self, parse_tree: ParseTree) -> FunParam {
        match_node!(parse_tree, (GrammarSymbol::FunParam | GrammarSymbol::TypedFunParam, mut children) => {
            let id = self.get_next_node_id();
            let mutability = if children[0].get_category() == &kw!(Mut) {
                self.build_mutability(children.remove(0))
            } else {
                Mutability { span: Span::dummy(), kind: MutabilityKind::Immutable }
            };
            let (ident, ident_span): (String, Span) = match_leaf!(children.remove(0),
                (token) => (token.content, token.range.into()));
            let ty = if children.len() == 2 {
                children.remove(0);
                self.build_ty(children.remove(0))
            } else {
                Ty { id: self.get_next_node_id(), span: Span::dummy(), kind: TyKind::Omitted }
            };

            let span = Span::wrap(vec![mutability.span, ident_span, ty.span]);
            FunParam { id, ident, span, mutability, ty }
        })
    }

    fn build_expr(&mut self, parse_tree: ParseTree) -> Expr {
        use GrammarSymbol::*;
        match parse_tree {
            Leaf(..) => panic!("Unexpected leaf {:?}", parse_tree),
            Node(symbol, mut children) => match symbol {
                Expr => self.build_expr(children.remove(0)),
                AssignmentLevel
                | BooleanLevel
                | BitwiseLevel
                | EqualityComparisonLevel
                | ComparisonLevel
                | ShiftLevel
                | AddLevel
                | MulLevel => self.build_binary(children),
                DerefLevel => self.build_unary(children),
                FunCallLevel => self.build_fun_call_level(children),
                UnaryLevel => self.build_unary(children),
                ExprTerm => self.build_expr_term(children.remove(0)),
                _ => panic!("Unexpected expression: {:?}, {:?}", symbol, children),
            },
        }
    }

    fn build_expr_term(&mut self, parse_tree: ParseTree) -> Expr {
        use GrammarSymbol::Token;
        use TokenCategory::Keyword;
        use TokenCategory::Literal;
        match parse_tree {
            Leaf(token) => {
                let kind = match token.get_category() {
                    Token(Literal(LiteralKind::IntLiteral)) => ExprKind::IntLiteral(IntLiteral {
                        content: token.content,
                    }),
                    Token(Literal(LiteralKind::FloatLiteral)) => {
                        ExprKind::FloatLiteral(FloatLiteral {
                            content: token.content,
                        })
                    }
                    Token(Literal(LiteralKind::BoolLiteral)) => {
                        ExprKind::BoolLiteral(BoolLiteral {
                            content: token.content,
                        })
                    }
                    Token(Literal(LiteralKind::StringLiteral)) => {
                        ExprKind::StringLiteral(StringLiteral {
                            content: token.content,
                        })
                    }

                    Token(Keyword(KeywordKind::Break)) => ExprKind::Break,
                    Token(Keyword(KeywordKind::Continue)) => ExprKind::Continue,
                    _ => panic!("Unexpected token {:?}", token),
                };
                let id = self.get_next_node_id();
                let span: Span = token.range.into();

                Expr { id, span, kind }
            }
            Node(GrammarSymbol::QualifiedIdentifier, children) => {
                self.build_qualified_identifier(children)
            }
            Node(GrammarSymbol::Block, children) => {
                let id = self.get_next_node_id();
                let block = self.build_block_from_children(children);
                Expr {
                    id,
                    span: block.span,
                    kind: ExprKind::Block(block),
                }
            }
            Node(GrammarSymbol::IfExpr, children) => {
                let id = self.get_next_node_id();
                let if_expr = self.build_if_expr(children);
                Expr {
                    id,
                    span: if_expr.span,
                    kind: ExprKind::If(if_expr),
                }
            }
            _ => panic!("Unexpected expr term: {:?}", parse_tree),
        }
    }

    fn build_if_expr(&mut self, children: Vec<ParseTree>) -> If {
        let mut children = VecDeque::from(children);
        let id = self.get_next_node_id();
        let if_span = Self::get_token_span(&children.pop_front().unwrap());
        let condition = self.build_expr(children.pop_front().unwrap());
        let then_block = self.build_block(children.pop_front().unwrap());
        let else_block = if children.front().map(|c| c.get_category()) == Some(&kw!(Else)) {
            children.pop_front(); // Else
            self.build_block(children.pop_front().unwrap())
        } else {
            Block {
                id: self.get_next_node_id(),
                span: Span::dummy(),
                stmts: Vec::new(),
            }
        };

        let span = Span::wrap(vec![if_span, then_block.span]);
        If {
            id,
            span,
            condition: Box::new(condition),
            then_block,
            else_block,
        }
    }

    fn build_let_decl(&mut self, children: Vec<ParseTree>) -> Let {
        let mut children: VecDeque<ParseTree> = children.into_iter().collect();
        let let_span = Self::get_token_span(&children.pop_front().unwrap());
        let mutability = if children[0].get_category() == &kw!(Mut) {
            Mutability {
                kind: MutabilityKind::Mutable,
                span: Self::get_token_span(&children.pop_front().unwrap()),
            }
        } else {
            Mutability {
                kind: MutabilityKind::Immutable,
                span: Span::dummy(),
            }
        };

        let ident = match_leaf!(children.remove(0).unwrap(), (token) => token.content);

        let ty = if children.front().map(|t| t.get_category()) == Some(&symb!(Colon)) {
            children.pop_front();
            self.build_ty(children.pop_front().unwrap())
        } else {
            Ty::omitted(self.get_next_node_id())
        };

        children.pop_front(); // Eq
        let expr = self.build_expr(children.pop_front().unwrap());

        let span = Span::wrap(vec![let_span, expr.span]);
        Let {
            span,
            mutability,
            ident,
            ty,
            expr: Box::new(expr),
        }
    }

    fn build_qualified_identifier(&mut self, children: Vec<ParseTree>) -> Expr {
        let id = self.get_next_node_id();
        let children: Vec<Identifier> =
            Self::build_interspersed(children, |node| self.build_identifier(node));
        let span = Span::wrap(vec![children[0].span, children[children.len() - 1].span]);
        Expr {
            id,
            span,
            kind: ExprKind::QualifiedIdentifier(children),
        }
    }

    fn build_identifier(&mut self, parse_tree: ParseTree) -> Identifier {
        match_leaf!(parse_tree, (token) => Identifier {
            id: self.get_next_node_id(),
            ident: token.content,
            span: token.range.into()
        })
    }

    fn build_stmt(&mut self, parse_tree: ParseTree) -> Stmt {
        match_node!(parse_tree, (GrammarSymbol::Stmt, mut children) => {
            let id = self.get_next_node_id();
            let kind = match children.remove(0) {
                Leaf(token) => panic!("Unexpected leaf {:?}", token),
                Node(symbol, mut children) => match symbol {
                    GrammarSymbol::Expr => StmtKind::Expr(self.build_expr(children.remove(0))),
                    GrammarSymbol::LetDecl => StmtKind::Let(self.build_let_decl(children)),
                    GrammarSymbol::ForLoop => StmtKind::For(self.build_for_loop(children)),
                    GrammarSymbol::WhileLoop => StmtKind::While(self.build_while_loop(children)),
                    GrammarSymbol::LoopLoop => StmtKind::Loop(self.build_loop_loop(children)),
                    _ => panic!("Unexpected statement: {:?}, {:?}", symbol, children),
                },
            };

            let span = kind.span();
            Stmt { id, span, kind }
        })
    }

    fn build_block(&mut self, parse_tree: ParseTree) -> Block {
        match_node!(parse_tree, (GrammarSymbol::Block, children) => self.build_block_from_children(children))
    }

    fn build_block_from_children(&mut self, children: Vec<ParseTree>) -> Block {
        let mut children = VecDeque::from(children);
        let id = self.get_next_node_id();
        let open_span = Self::get_token_span(&children.pop_front().unwrap());
        let close_span = Self::get_token_span(&children.pop_back().unwrap());
        let span = Span::wrap(vec![open_span, close_span]);

        let mut stmts: Vec<Stmt> = Vec::new();
        let last_is_empty = children.back().map(|c| c.get_category()) == Some(&symb!(Semicolon));
        while let Some(child) = children.pop_front() {
            let stmt = self.build_stmt(child);
            stmts.push(stmt);
            if children.front().map(|c| c.get_category()) == Some(&symb!(Semicolon)) {
                children.pop_front();
            }
        }

        if last_is_empty {
            stmts.push(Stmt {
                id: self.get_next_node_id(),
                span: Span::dummy(),
                kind: StmtKind::Expr(Expr {
                    id: self.get_next_node_id(),
                    span: Span::dummy(),
                    kind: ExprKind::Empty,
                }),
            });
        }

        Block { id, span, stmts }
    }

    fn build_for_loop(&mut self, children: Vec<ParseTree>) -> For {
        let mut children = VecDeque::from(children);
        let for_span = Self::get_token_span(&children.pop_front().unwrap());
        let ident = self.build_identifier(children.pop_front().unwrap());
        children.pop_front(); // In
        let range_expr = self.build_range_expr(children.pop_front().unwrap());
        let body = self.build_block(children.pop_front().unwrap());
        let span = Span::wrap(vec![for_span, body.span]);

        For {
            span,
            identifier: ident,
            range: range_expr,
            body,
        }
    }

    fn build_range_expr(&mut self, parse_tree: ParseTree) -> RangeExpr {
        match_node!(parse_tree, (GrammarSymbol::RangeExpr, mut children) => {
            let id = self.get_next_node_id();
            let lhs = self.build_expr(children.remove(0));
            let op = children.remove(0);
            let rhs = self.build_expr(children.remove(0));
            let span = Span::wrap(vec![lhs.span, rhs.span]);

            let kind = match op.get_category() {
                symb!(DotDot) => RangeKind::Open(Box::new(lhs), Box::new(rhs)),
                symb!(DotDotEq) => RangeKind::Closed(Box::new(lhs), Box::new(rhs)),
                _ => panic!("Unexpected range operator"),
            };

            RangeExpr { id, span, kind }
        })
    }

    fn build_while_loop(&mut self, children: Vec<ParseTree>) -> While {
        let mut children = VecDeque::from(children);
        let id = self.get_next_node_id();
        let while_span = Self::get_token_span(&children.pop_front().unwrap());
        let condition = self.build_expr(children.pop_front().unwrap());
        let body = self.build_block(children.pop_front().unwrap());
        let span = Span::wrap(vec![while_span, body.span]);

        While {
            id,
            span,
            condition,
            body,
        }
    }

    fn build_loop_loop(&mut self, children: Vec<ParseTree>) -> Loop {
        let mut children = VecDeque::from(children);
        let id = self.get_next_node_id();
        let loop_span = Self::get_token_span(&children.pop_front().unwrap());
        let body = self.build_block(children.pop_front().unwrap());
        let span = Span::wrap(vec![loop_span, body.span]);

        Loop { id, span, body }
    }

    fn build_fun_call_level(&mut self, children: Vec<ParseTree>) -> Expr {
        let mut children = VecDeque::from(children);
        let mut value = self.build_expr(children.pop_front().unwrap());
        while let Some(operation) = children.pop_front() {
            value = match operation.get_category() {
                GrammarSymbol::FunCall => self.build_fun_call(value, operation),
                GrammarSymbol::MemberAccess => self.build_member_access(value, operation),
                _ => panic!("Unexpected fun call level operation"),
            }
        }
        value
    }

    fn build_fun_call(&mut self, value: Expr, parse_tree: ParseTree) -> Expr {
        match_node!(parse_tree, (GrammarSymbol::FunCall, children) => {
            let (args, span) = Self::build_delimited_interspersed(children, |node| self.build_expr(node));

            Expr {
                id: self.get_next_node_id(),
                span,
                kind: ExprKind::FunCall(Box::new(value), args),
            }
        })
    }

    fn build_member_access(&mut self, value: Expr, parse_tree: ParseTree) -> Expr {
        match_node!(parse_tree, (GrammarSymbol::MemberAccess, mut children) => {
            let operator = children.remove(0);
            let ident_span = Self::get_token_span(&children[0]);
            let ident = match_leaf!(children.remove(0), (token) => token.content);
            let span = Span::wrap(vec![value.span, ident_span]);

            let kind = match operator.get_category() {
                symb!(Dot) => ExprKind::MemberAccess(Box::new(value), ident),
                symb!(Arrow) => ExprKind::PointerMemberAccess(Box::new(value), ident),
                _ => panic!("Unexpected member access operator"),
            };

            Expr {
                id: self.get_next_node_id(),
                span,
                kind
            }
        })
    }

    fn build_binary(&mut self, children: Vec<ParseTree>) -> Expr {
        let mut children: VecDeque<ParseTree> = children.into_iter().collect();
        let mut lhs = self.build_expr(children.pop_front().unwrap());
        while !children.is_empty() {
            let op = self.get_binary_operator(children.pop_front().unwrap());
            let rhs = self.build_expr(children.pop_front().unwrap());
            let span = Span::wrap(vec![lhs.span, rhs.span]);
            let id = self.get_next_node_id();
            let kind = ExprKind::Binary(Box::new(lhs), op, Box::new(rhs));
            lhs = Expr { id, span, kind }
        }

        lhs
    }

    fn build_unary(&mut self, mut children: Vec<ParseTree>) -> Expr {
        let mut value = self.build_expr(children.pop().unwrap());

        while let Some(op) = children.pop() {
            let op_span = Self::get_token_span(&op);
            let op = self.get_unary_operator(op);
            let span = Span::wrap(vec![op_span, value.span]);
            let kind = ExprKind::Unary(op, Box::new(value));
            value = Expr {
                id: self.get_next_node_id(),
                span,
                kind,
            }
        }

        value
    }

    fn get_binary_operator(&self, parse_tree: ParseTree) -> BinaryOperator {
        use GrammarSymbol::Token;
        use SymbolKind::*;
        use TokenCategory::Symbol;
        macro_rules! map (
            ($expr:expr; $($token:pat => $operator:ident),*) => {
                match $expr {
                    $(Token(Symbol($token)) => BinaryOperator::$operator,)*
                    _ => panic!("Unknown binary operator {:?}", $expr)
                }
            };
        );
        match_leaf!(parse_tree, (token) =>
            map!(token.get_category();
                Plus => Add,
                Minus => Sub,
                Asterisk => Mul,
                Slash => Div,
                Percent => Mod,
                Ampersand => BitAnd,
                Pipe => BitOr,
                DoubleAmpersand => And,
                DoublePipe => Or,
                Caret => Xor,
                Eq => Assign,
                EqEq => Eq,
                NotEq => Neq,
                Lt => Lt,
                Leq => Leq,
                Gt => Gt,
                Geq => Geq,
                PlusEq => AddAssign,
                MinusEq => SubAssign,
                AsteriskEq => MulAssign,
                SlashEq => DivAssign,
                PercentEq => ModAssign,
                CaretEq => XorAssign,
                AmpersandEq => BitAndAssign,
                PipeEq => BitOrAssign,
                ShiftLeft => BitShiftLeft,
                ShiftRight => BitShiftRight,
                ShiftLeftEq => BitShiftLeftAssign,
                ShiftRightEq => BitShiftRightAssign
            )
        )
    }

    fn get_unary_operator(&self, parse_tree: ParseTree) -> UnaryOperator {
        use GrammarSymbol::Token;
        use SymbolKind::*;
        use TokenCategory::Symbol;
        match_leaf!(parse_tree, (token) => match token.get_category() {
            Token(Symbol(Asterisk)) => UnaryOperator::Deref,
            Token(Symbol(Ampersand)) => UnaryOperator::Ref,
            Token(Symbol(Minus)) => UnaryOperator::Minus,
            Token(Symbol(Bang)) => UnaryOperator::Not,
            Token(Symbol(Tilde)) => UnaryOperator::Neg,
            _ => panic!("Unknown unary operator {:?}", token)
        })
    }

    fn build_enum(&mut self, parse_tree: ParseTree) -> Enum {
        match_node!(parse_tree, (GrammarSymbol::EnumDef, mut children) => {
            children.remove(0); // Enum
            let ident = match_leaf!(children.remove(0), (token) => token.content);
            let (generic_params, generic_params_span) =
                if matches!(children[0].get_category(), GrammarSymbol::GenericParams) {
                    self.build_generic_params(children.remove(0))
                } else {
                    (Vec::new(), Span::dummy())
                };

            let (variants, variants_span) = self.build_enum_variants(children.remove(0));
            let span = Span::wrap(vec![generic_params_span, variants_span]);

            Enum { ident, span, generic_params, variants }
        })
    }

    fn build_enum_variants(&mut self, parse_tree: ParseTree) -> (Vec<EnumVariant>, Span) {
        match_node!(parse_tree, (GrammarSymbol::EnumVariants, children) => {
            Self::build_delimited_interspersed(children, |node| self.build_enum_variant(node))
        })
    }

    fn build_enum_variant(&mut self, parse_tree: ParseTree) -> EnumVariant {
        match_node!(parse_tree, (GrammarSymbol::EnumVariant, mut children) => {
            let id = self.get_next_node_id();
            let ident_span = Self::get_token_span(&children[0]);
            let ident = match_leaf!(children.remove(0), (token) => token.content);
            let (fields, fields_span) = if children.is_empty() {
                (Vec::new(), Span::dummy())
            } else {
                Self::build_delimited_interspersed(children, |node| self.build_ty(node))
            };

            let span = Span::wrap(vec![ident_span, fields_span]);
            EnumVariant { id, ident, span, fields }
        })
    }

    fn build_struct(&mut self, parse_tree: ParseTree) -> Struct {
        match_node!(parse_tree, (GrammarSymbol::StructDef, mut children) => {
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
                let (implements, implements_span) =
                    if matches!(children[0].get_category(), GrammarSymbol::ImplementsDef) {
                        self.build_implements(children.remove(0))
                    } else {
                        (Vec::new(), Span::dummy())
                    };
                let (methods, methods_span) = self.build_struct_methods(children.remove(0), default_visibility);

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

    fn build_implements(&mut self, parse_tree: ParseTree) -> (Vec<Ty>, Span) {
        match_node!(parse_tree, (GrammarSymbol::ImplementsDef, children) => {
                let mut children: VecDeque<ParseTree> = children.into_iter().collect();
                let mut result: Vec<Ty> = Vec::new();
                let mut span: Span = Self::get_token_span(&children.pop_front().unwrap());

                while let Some(child) = children.pop_front() {
                    let ty = self.build_ty(child);
                    span = Span::wrap(vec![span, ty.span]);
                    result.push(ty);
                    children.pop_front(); // Comma
                }

                (result, span)
            }
        )
    }

    fn build_generic_params(&mut self, parse_tree: ParseTree) -> (Vec<TyName>, Span) {
        match_node!(parse_tree, (GrammarSymbol::GenericParams, children) => {
               Self::build_delimited_interspersed(children, |node| self.build_ty_name(node))
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
                Self::build_delimited_interspersed(children, |node| self.build_struct_field(node, default_visibility))
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
                    kind: MutabilityKind::Mutable, span: Self::get_token_span(&children.remove(0)) }
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

    fn build_struct_methods(
        &mut self,
        parse_tree: ParseTree,
        default_visibility: VisibilityKind,
    ) -> (Vec<StructMethod>, Span) {
        match_node!(parse_tree, (GrammarSymbol::StructMethods, children) => {
                Self::build_delimited_interspersed(children, |node| self.build_struct_method(node, default_visibility))
            }
        )
    }

    fn build_struct_method(
        &mut self,
        parse_tree: ParseTree,
        default_visibility: VisibilityKind,
    ) -> StructMethod {
        match_node!(parse_tree, (GrammarSymbol::StructMethod, mut children) => {
            let id = self.get_next_node_id();
            let visibility = if children[0].get_category() == &GrammarSymbol::Visibility {
                self.build_visibility(children.remove(0))
            } else {
                Visibility { span: Span::dummy(), kind: default_visibility }
            };


            let staticity = if children[0].get_category() == &kw!(Static) {
                Staticity { span: Self::get_token_span(&children.remove(0)), kind: StaticityKind::Static }
            } else {
                Staticity { span: Span::dummy(), kind: StaticityKind::Instance }
            };

            let mutability = if children[0].get_category() == &kw!(Mut) {
                self.build_mutability(children.remove(0))
            } else {
                Mutability { span: Span::dummy(), kind: MutabilityKind::Immutable }
            };

            let fun = self.build_fun(children.remove(0));
            let span = Span::wrap(vec![visibility.span, fun.span]);

            StructMethod { id, visibility, staticity, mutability, fun, span }
        })
    }

    fn build_ty(&mut self, parse_tree: ParseTree) -> Ty {
        let id = self.get_next_node_id();
        let (kind, span) = match_node!(parse_tree, (GrammarSymbol::Type, mut children) => {
            assert_eq!(children.len(), 1);
            match children.remove(0) {
                Node(GrammarSymbol::PtrType, mut children) => {
                    let mutability = if children[0].get_category() == &kw!(MutPtr) {
                        Mutability { span: Span::dummy(), kind: MutabilityKind::Mutable }
                    } else {
                        Mutability { span: Span::dummy(), kind: MutabilityKind::Immutable }
                    };
                    let asterisk_span = Self::get_token_span(&children.remove(0));
                    let ty = self.build_ty(children.remove(0));
                    let span = Span::wrap(vec![asterisk_span, ty.span]);
                    (TyKind::Ptr(mutability, Box::new(ty)), span)
                }
                Node(GrammarSymbol::RefType, mut children) => {
                    let mutability = if children[0].get_category() == &kw!(MutRef) {
                        Mutability { span: Span::dummy(), kind: MutabilityKind::Mutable }
                    } else {
                        Mutability { span: Span::dummy(), kind: MutabilityKind::Immutable }
                    };
                    let ampersand_span = Self::get_token_span(&children.remove(0));
                    let ty = self.build_ty(children.remove(0));
                    let span = Span::wrap(vec![ampersand_span, ty.span]);
                    (TyKind::Ptr(mutability, Box::new(ty)), span)
                }
                Node(GrammarSymbol::FunOrTupleType, _children) => todo!(),
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
            Self::build_delimited_interspersed(children, |node| self.build_ty(node))
        )
    }

    fn build_interspersed<T, F: FnMut(ParseTree) -> T>(
        children: Vec<ParseTree>,
        child_builder: F,
    ) -> Vec<T> {
        children
            .into_iter()
            .enumerate()
            .filter(|(i, _)| i % 2 == 0)
            .map(|(_, child)| child)
            .map(child_builder)
            .collect()
    }

    fn build_delimited<T, F: FnMut(Vec<ParseTree>) -> T>(
        mut children: Vec<ParseTree>,
        mut child_builder: F,
    ) -> (T, Span) {
        let open = children.remove(0);
        let close = children.pop().unwrap();
        let span = Span::wrap(vec![
            Self::get_token_span(&open),
            Self::get_token_span(&close),
        ]);

        (child_builder(children), span)
    }

    fn build_delimited_interspersed<T, F: FnMut(ParseTree) -> T>(
        children: Vec<ParseTree>,
        mut child_builder: F,
    ) -> (Vec<T>, Span) {
        Self::build_delimited(children, |inner| {
            Self::build_interspersed(inner, &mut child_builder)
        })
    }

    fn get_token_span(token: &ParseTree) -> Span {
        match_leaf!(token, (token) => { token.range.clone().into() })
    }
}
