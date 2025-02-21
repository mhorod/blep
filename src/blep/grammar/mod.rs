mod utils;

use super::tokens::*;
use crate::parsing::grammar::Grammar;
use crate::regex::Regex::Atom;

use crate::{kw, lit, regex_grammar, symb, tok};

use utils::*;

pub fn blep_grammar() -> Grammar<GrammarSymbol> {
    use DelimKind::*;
    use GrammarSymbol::*;
    use KeywordKind::*;
    use LiteralKind::*;
    use SymbolKind::*;
    use TokenCategory::*;

    regex_grammar!(
        Program,
        Program => Atom(Item).star() + tok!(Eof),
        Item => optional(Atom(Visibility)) +
            (Atom(FunDef)
             | Atom(EnumDef)
             | Atom(NewTypeDef)
             | Atom(TypeAliasDef)
             | Atom(StructDef)
             | Atom(InterfaceDef)
             ),

        NewTypeDef => kw!(NewType)
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + symb!(Eq)
            + Atom(Type),

        TypeAliasDef => kw!(TypeAlias)
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + symb!(Eq)
            + Atom(Type),

        FunDef => optional(kw!(Pure))
            + kw!(Fun)
            + tok!(Identifier)
            + optional(Atom(GenericParams))
            + Atom(FunParams)
            + optional(symb!(Arrow) + Atom(Type))
            + symb!(Eq)
            + Atom(Expr),

        EnumDef =>
            kw!(Enum)
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + Atom(EnumVariants),

        EnumVariants => delimited(optional(separated(Atom(EnumVariant), symb!(Comma))), Brace),

        StructDef =>
            (kw!(Struct) | kw!(Class))
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + Atom(StructFields)
            + optional(Atom(ImplementsDef))
            + Atom(StructMethods),

        InterfaceDef =>
            kw!(Interface)
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + optional(Atom(InterfaceMethods)),

        StructFields => delimited(optional(separated(Atom(StructField), symb!(Comma))), Paren),
        StructField =>
            optional(Atom(Visibility))
            + optional(kw!(Mut))
            + tok!(Identifier)
            + symb!(Colon)
            + Atom(Type),

        ImplementsDef =>
            kw!(Implements)
            + interspersed(Atom(Type), symb!(Comma)),

        StructMethods =>
            delimited(Atom(StructMethod).star(), Brace),
        StructMethod => optional(Atom(Visibility)) 
            + optional(kw!(Static)) 
            + optional(kw!(Mut))
            + Atom(FunDef),


        InterfaceMethods => delimited(Atom(InterfaceMethod).star(), Brace),
        InterfaceMethod =>
            optional(kw!(Static)) 
            + optional(kw!(Mut))
            + optional(kw!(Pure))
            + kw!(Fun)
            + tok!(Identifier)
            + Atom(TypedFunParams)
            + symb!(Arrow) + Atom(Type),


        EnumVariant => tok!(TypeName) + optional(delimited(interspersed(Atom(Type), symb!(Comma)), Paren)),

        Visibility => kw!(Public) | kw!(Private) | kw!(Internal),

        FunParams => delimited(optional(interspersed(Atom(FunParam), symb!(Comma))), Paren),
        FunParam => optional(kw!(Mut)) + tok!(Identifier) + optional(symb!(Colon) + Atom(Type)),


        TypedFunParams => delimited(optional(interspersed(Atom(TypedFunParam), symb!(Comma))), Paren),
        FunParam => optional(kw!(Mut)) + tok!(Identifier) + optional(symb!(Colon) + Atom(Type)),
        TypedFunParam => optional(kw!(Mut)) + tok!(Identifier) + symb!(Colon) + Atom(Type),

        GenericParams =>
            empty_delims(Bracket)
            | delimited(interspersed(tok!(TypeName), symb!(Comma)), Bracket),

        LetDecl =>
            kw!(Let)
            + optional(kw!(Mut))
            + tok!(Identifier)
            + optional(symb!(Colon) + Atom(Type))
            + symb!(Eq)
            + Atom(Expr),

        Stmt => Atom(LetDecl)
            | Atom(ForLoop)
            | Atom(WhileLoop)
            | Atom(LoopLoop)
            | Atom(Expr),

        ForLoop => kw!(For)
            + tok!(Identifier)
            + kw!(In)
            + Atom(RangeExpr)
            + Atom(Block),


        RangeExpr => Atom(Expr) + (symb!(DotDot) | symb!(DotDotEq)) + Atom(Expr),

        WhileLoop => kw!(While) + Atom(Expr) +  Atom(Block),
        LoopLoop => kw!(Loop) + Atom(Block),

        Expr => Atom(AssignmentLevel),


        // operator levels
        AssignmentLevel => Atom(BooleanLevel)
            + optional((symb!(Eq) 
                | symb!(PlusEq) 
                | symb!(MinusEq) 
                | symb!(AsteriskEq)
                | symb!(SlashEq)
                | symb!(PercentEq)
                | symb!(AmpersandEq)
                | symb!(PipeEq)
                | symb!(CaretEq)
                | symb!(ShiftLeftEq)
                | symb!(ShiftRightEq)
                )
            + Atom(BooleanLevel)),


        BooleanLevel => Atom(BitwiseLevel) + ((symb!(DoubleAmpersand) | symb!(DoublePipe)) + Atom(BitwiseLevel)).star(),
        BitwiseLevel => Atom(EqualityComparisonLevel) + ((symb!(Ampersand) | symb!(Pipe) | symb!(Caret)) + Atom(EqualityComparisonLevel)).star(),

        EqualityComparisonLevel => Atom(ComparisonLevel) 
            + optional((symb!(EqEq) | symb!(NotEq)) + Atom(ComparisonLevel)),
        ComparisonLevel => Atom(ShiftLevel) 
            + optional((symb!(Lt) | symb!(Leq) | symb!(Gt) | symb!(Geq)) + Atom(ShiftLevel)),

        ShiftLevel => Atom(AddLevel) + ((symb!(ShiftLeft) | symb!(ShiftRight)) + Atom(AddLevel)).star(),

        AddLevel => Atom(MulLevel) + ((symb!(Plus) | symb!(Minus)) + Atom(MulLevel)).star(),
        MulLevel => Atom(DerefLevel) + ((symb!(Asterisk) | symb!(Slash) | symb!(Percent)) + Atom(DerefLevel)).star(),
        DerefLevel => (symb!(Asterisk) | symb!(Ampersand)).star() + Atom(FunCallLevel),
        FunCallLevel => Atom(UnaryLevel)
            + (Atom(FunCall) | Atom(MemberAccess)).star(),
        MemberAccess => ((symb!(Dot) | symb!(Arrow)) + tok!(Identifier)),
        FunCall => delimited(optional(interspersed(Atom(Expr), symb!(Comma))), Paren),

        UnaryLevel => (symb!(Minus) | symb!(Bang) | symb!(Tilde)).star() + Atom(ExprTerm),

        Parens =>
            empty_delims(Paren)
            | delimited(interspersed(Atom(Expr), symb!(Comma)), Paren)
            | delimited(Atom(Expr), Paren)
            ,
        ExprTerm => lit!(IntLiteral)
            | lit!(FloatLiteral)
            | lit!(StringLiteral)
            | lit!(BoolLiteral)
            | kw!(Break)
            | kw!(Continue)
            | Atom(QualifiedIdentifier)
            | Atom(Block)
            | Atom(Parens)
            | Atom(ReturnExpr)
            | Atom(IfExpr),

        ReturnExpr => kw!(Return) + optional(Atom(Expr)),

        QualifiedIdentifier => (tok!(TypeName) + symb!(DoubleColon)).star() +
            (tok!(Identifier) | tok!(TypeName)),

        Block => empty_delims(Brace) | delimited(separated(Atom(Stmt), symb!(Semicolon)), Brace),
        IfExpr => kw!(If) + Atom(Expr) + Atom(Block) + optional(kw!(Else) + Atom(Block)),

        Type => Atom(PtrType) | Atom(RefType) | Atom(FunOrTupleType) | Atom(QualifiedGenericType),
        PtrType => (symb!(Asterisk) | kw!(MutPtr)) + Atom(Type),
        RefType => (symb!(Ampersand) | kw!(MutRef)) + Atom(Type),
        FunOrTupleType =>
            delimited(interspersed(Atom(Type), symb!(Comma)), Paren) + optional(symb!(Arrow) + Atom(Type)),
        QualifiedGenericType =>
            tok!(TypeName)
            + (symb!(DoubleColon) + tok!(TypeName)).star()
            + optional(Atom(GenericArgs)),

        GenericArgs => delimited(interspersed(Atom(Type), symb!(Comma)), Bracket)
    ).into()
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum GrammarSymbol {
    Token(TokenCategory),

    // top level
    Program,
    Item,

    // Items
    FunDef,
    EnumDef,
    NewTypeDef,
    TypeAliasDef,
    StructDef,
    InterfaceDef,

    // Modifiers
    Visibility,

    // enum
    EnumVariants,
    EnumVariant,

    // Struct
    StructFields,
    StructField,
    ImplementsDef,
    StructMethods,
    StructMethod,

    // Interface
    InterfaceMethods,
    InterfaceMethod,

    // Functions
    TypedFunParams,
    TypedFunParam,
    FunParams,
    FunParam,
    ReturnExpr,

    // Statements
    Stmt,
    LetDecl,
    AssignmentLevel,
    ForLoop,
    WhileLoop,
    LoopLoop,
    RangeExpr,

    // Expressions
    Expr,
    ExprTerm,
    FunCall,
    Parens,
    Block,
    IfExpr,
    QualifiedIdentifier,

    // Operator levels
    EqualityComparisonLevel,
    BooleanLevel,
    BitwiseLevel,
    ComparisonLevel,
    ShiftLevel,
    AddLevel,
    MulLevel,
    DerefLevel,
    FunCallLevel,
    MemberAccess,
    UnaryLevel,

    // Types
    Type,
    RefType,
    PtrType,
    FunOrTupleType,
    QualifiedGenericType,
    GenericParams,
    GenericArgs,
}
