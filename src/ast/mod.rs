pub mod display;
pub mod generate;

use std::ops::Range;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AstNodeId(pub u32);

type Ident = String;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    begin: usize,
    end: usize,
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            begin: range.start,
            end: range.end,
        }
    }
}

impl Span {
    fn wrap(spans: Vec<Span>) -> Self {
        let spans: Vec<Span> = spans.into_iter().filter(|s| !s.is_dummy()).collect();
        let begin = spans.iter().min_by_key(|s| s.begin).unwrap().begin;
        let end = spans.iter().min_by_key(|s| s.end).unwrap().end;
        Self { begin, end }
    }
    fn is_dummy(&self) -> bool {
        return self.begin == 0 && self.end == 0;
    }
    fn dummy() -> Self {
        Self { begin: 0, end: 0 }
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct Item {
    id: AstNodeId,
    span: Span,
    visibility: Visibility,
    kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Enum(Enum),
    Struct(Struct),
    Fun,
    NewType,
    TypeAlias,
}

#[derive(Debug, Clone)]
pub struct Visibility {
    span: Span,
    kind: VisibilityKind,
}

#[derive(Debug, Clone, Copy)]
pub enum VisibilityKind {
    Public,
    Internal,
    Private,
}

#[derive(Debug, Clone)]
pub struct Mutability {
    span: Span,
    kind: MutabilityKind,
}

#[derive(Debug, Clone)]
pub enum MutabilityKind {
    Mutable,
    Immutable,
}

#[derive(Debug, Clone)]
pub struct Enum {
    span: Span,
    generics: Vec<TyName>,
    variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    id: AstNodeId,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Struct {
    span: Span,
    ident: Ident,
    kind: StructKind,
    generic_params: Vec<TyName>,
    fields: Vec<StructField>,
    implements: Vec<Ty>,
    methods: Vec<StructMethod>,
}

#[derive(Debug, Clone)]
pub enum StructKind {
    Struct,
    Class,
}

#[derive(Debug, Clone)]
pub struct StructField {
    id: AstNodeId,
    visibility: Visibility,
    mutability: Mutability,
    ident: Ident,
    ty: Ty,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    id: AstNodeId,
}

#[derive(Debug, Clone)]
pub struct TyName {
    id: AstNodeId,
    ident: Ident,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Ty {
    id: AstNodeId,
    span: Span,
    kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
    Primitive(PrimitiveTy),
    Ref(Box<Ty>),
    Ptr(Box<Ty>),
    Tuple(Vec<Ty>),
    Fun(FunTy),
}

#[derive(Debug, Clone)]
pub struct PrimitiveTy {
    path: Vec<TyName>,
    generic_args: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct FunTy {
    args: Vec<Ty>,
    result: Box<Ty>,
}

impl ItemKind {
    fn span(&self) -> Span {
        match self {
            ItemKind::Enum(e) => e.span,
            ItemKind::Struct(s) => s.span,
            _ => Span::dummy(),
        }
    }
}
