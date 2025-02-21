pub mod display;
pub mod generate;

use std::fmt::Debug;
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
        self.begin == 0 && self.end == 0
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
    Interface(Interface),
    Fun(Fun),
    NewType(NewType),
    TypeAlias(TypeAlias),
}

#[derive(Debug, Clone)]
pub struct Purity {
    span: Span,
    kind: PurityKind,
}

#[derive(Debug, Clone)]
pub enum PurityKind {
    Pure,
    Impure,
}

#[derive(Debug, Clone)]
pub struct Staticity {
    span: Span,
    kind: StaticityKind,
}

#[derive(Debug, Clone, Copy)]
pub enum StaticityKind {
    Static,
    Instance,
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
    ident: Ident,
    generic_params: Vec<TyName>,
    variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    id: AstNodeId,
    span: Span,
    ident: Ident,
    fields: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct Interface {
    span: Span,
    ident: Ident,
    generic_params: Vec<TyName>,
    methods: Vec<InterfaceMethod>
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    id: AstNodeId,
    span: Span,
    staticity: Staticity,
    mutability: Mutability,
    purity: Purity,
    ident: Ident,
    params: Vec<FunParam>,
    return_ty: Ty,
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
    span: Span,
    visibility: Visibility,
    staticity: Staticity,
    mutability: Mutability,
    fun: Fun,
}

#[derive(Debug, Clone)]
pub struct Fun {
    id: AstNodeId,
    ident: Ident,
    span: Span,
    purity: Purity,
    generic_params: Vec<TyName>,
    params: Vec<FunParam>,
    return_ty: Ty,
    expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FunParam {
    id: AstNodeId,
    ident: Ident,
    span: Span,
    mutability: Mutability,
    ty: Ty,
}

#[derive(Debug, Clone)]
pub struct NewType {}

#[derive(Debug, Clone)]
pub struct TypeAlias {}

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
    Omitted,
    Primitive(PrimitiveTy),
    Ref(Mutability, Box<Ty>),
    Ptr(Mutability, Box<Ty>),
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

#[derive(Debug, Clone)]
pub struct Expr {
    id: AstNodeId,
    span: Span,
    kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    BoolLiteral(BoolLiteral),
    StringLiteral(StringLiteral),
    QualifiedIdentifier(Vec<Identifier>),
    MemberAccess(Box<Expr>, Ident),
    PointerMemberAccess(Box<Expr>, Ident),
    FunCall(Box<Expr>, Vec<Expr>),
    Unary(UnaryOperator, Box<Expr>),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Block(Block),
    If(If),
    Empty,
    Break,
    Continue
}

#[derive(Debug, Clone)]
pub struct If {
    id: AstNodeId,
    span: Span,
    condition: Box<Expr>,
    then_block: Block,
    else_block: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    id: AstNodeId,
    span: Span,
    stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Let {
    span: Span,
    mutability: Mutability,
    ident: Ident,
    ty: Ty,
    expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    id: AstNodeId,
    span: Span,
    kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(Expr),
    Let(Let),
    For(For),
    While(While),
    Loop(Loop),
}

#[derive(Debug, Clone)]
pub struct For {
    span: Span,
    identifier: Identifier,
    range: RangeExpr,
    body: Block
}

#[derive(Debug, Clone)]
pub struct While {
    id: AstNodeId,
    span: Span,
    condition: Expr,
    body: Block
}


#[derive(Debug, Clone)]
pub struct Loop {
    id: AstNodeId,
    span: Span,
    body: Block
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    id: AstNodeId,
    span: Span,
    kind: RangeKind,
}

#[derive(Debug, Clone)]
pub enum RangeKind {
    Open(Box<Expr>, Box<Expr>),
    Closed(Box<Expr>, Box<Expr>),
}


#[derive(Debug, Clone)]
pub struct Identifier {
    id: AstNodeId,
    span: Span,
    ident: Ident,
}

#[derive(Debug, Clone)]
pub struct IntLiteral {
    content: String,
}

#[derive(Debug, Clone)]
pub struct FloatLiteral {
    content: String,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    content: String,
}

#[derive(Debug, Clone)]
pub struct BoolLiteral {
    content: String,
}


#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Deref,
    Ref,
    Minus,
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,

    BitShiftLeft,
    BitShiftRight,
    BitAnd,
    BitOr,
    Xor,

    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    XorAssign,
    BitAndAssign,
    BitOrAssign,
    BitShiftLeftAssign,
    BitShiftRightAssign,
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

impl Ty {
    fn omitted(id: AstNodeId) -> Self {
        Ty {
            id,
            span: Span::dummy(),
            kind: TyKind::Omitted,
        }
    }
}

impl StmtKind {
    fn span(&self) -> Span {
        match self {
            StmtKind::Expr(e) => e.span,
            StmtKind::Let(l) => l.span,
            StmtKind::For(f) => f.span,
            StmtKind::While(w) => w.span,
            StmtKind::Loop(l) => l.span,
        }
    }
}
