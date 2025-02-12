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

    let re_grammar = regex_grammar!(
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

        FunDef => Atom(FunModifiers)
            + kw!(Fun)
            + tok!(Identifier)
            + Atom(FunParams)
            + optional(symb!(Arrow) + Atom(Type))
            + symb!(Eq)
            + Atom(Expr),

        EnumDef =>
            kw!(Enum)
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + delimited(optional(separated(Atom(EnumVariant), symb!(Comma))), Brace),

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
        StructMethod => optional(Atom(Visibility)) + Atom(FunDef),


        InterfaceMethods => delimited(Atom(InterfaceMethod).star(), Brace),
        InterfaceMethod =>
            Atom(FunModifiers)
            + kw!(Fun)
            + tok!(Identifier)
            + Atom(TypedFunParams)
            + symb!(Arrow) + Atom(Type),


        EnumVariant => tok!(TypeName) + optional(delimited(interspersed(Atom(Type), symb!(Comma)),Paren)),

        FunModifiers => optional(kw!(Pure)),
        Visibility => kw!(Public) | kw!(Private) | kw!(Internal),

        FunParams => optional(Atom(GenericParams)) + Atom(FunValueParams),
        FunValueParams => delimited(optional(interspersed(Atom(FunParam), symb!(Comma))), Paren),
        FunParam => optional(kw!(Mut)) + tok!(Identifier) + optional(symb!(Colon) + Atom(Type)),


        TypedFunParams => delimited(optional(interspersed(Atom(TypedFunParam), symb!(Comma))), Paren),
        FunParam => optional(kw!(Mut)) + tok!(Identifier) + optional(symb!(Colon) + Atom(Type)),
        TypedFunParam => optional(kw!(Mut)) + tok!(Identifier) + symb!(Colon) + Atom(Type),

        GenericParams =>
            empty_delims(Bracket)
            | delimited(interspersed(tok!(TypeName), symb!(Comma)), Bracket),

        LetDecl =>
            (kw!(Let) + optional(kw!(Mut)) + tok!(Identifier) + symb!(Eq) + Atom(Expr) + optional(kw!(In) + Atom(Expr)))
        ,
        Expr => Atom(AssignmentLevel),

        // operator levels
        AssignmentLevel => Atom(AddLevel) + (
            (symb!(Eq) | symb!(PlusEq) | symb!(MinusEq)) + Atom(AddLevel)).star(),
        AddLevel => Atom(MulLevel) + ((symb!(Plus) | symb!(Minus)) + Atom(MulLevel)).star(),
        MulLevel => Atom(DerefLevel) + ((symb!(Asterisk) | symb!(Slash)) + Atom(DerefLevel)).star(),
        DerefLevel =>
            Atom(FunCallLevel)
            | ((symb!(Asterisk) | symb!(Ampersand)) + Atom(DerefLevel)) ,
        FunCallLevel => Atom(ExprTerm)
            +(Atom(FunCall) | Atom(MemberAccess)).star(),
        MemberAccess => ((symb!(Dot) | symb!(Arrow)) + tok!(Identifier)),


        FunCall => delimited(optional(interspersed(Atom(Expr), symb!(Comma))), Paren),


        Parens =>
            empty_delims(Paren)
            | delimited(interspersed(Atom(Expr), symb!(Comma)), Paren)
            | delimited(Atom(Expr), Paren)
            ,
        ExprTerm => lit!(IntLiteral)
            | lit!(StringLiteral)
            | Atom(QualifiedIdentifier)
            | Atom(Block)
            | Atom(LetDecl)
            | Atom(Parens),

        QualifiedIdentifier => (tok!(TypeName) + symb!(DoubleColon)).star() + (tok!(Identifier) | tok!(TypeName)),

        Block => empty_delims(Brace) | delimited(separated(Atom(Expr), symb!(Semicolon)), Brace),
        IfExpr => kw!(If) + Atom(Expr) + kw!(Then) + Atom(Expr) + kw!(Else) + Atom(Expr),

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
    );

    re_grammar.into()
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum GrammarSymbol {
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
    FunModifiers,

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
    FunValueParams,
    FunParam,

    Token(TokenCategory),

    // Expressions
    LetDecl,
    Expr,
    ExprTerm,
    FunCall,
    Parens,
    Block,
    IfExpr,
    QualifiedIdentifier,

    // Operator levels
    AssignmentLevel,
    AddLevel,
    MulLevel,
    DerefLevel,
    FunCallLevel,
    MemberAccess,

    // Types
    Type,
    RefType,
    PtrType,
    FunOrTupleType,
    QualifiedGenericType,
    GenericParams,
    GenericArgs,
}
