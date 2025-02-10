mod utils;

use super::tokens::*;
use crate::parsing::grammar::Grammar;
use crate::parsing::grammar::regex::Productions;
use crate::regex::Regex::Atom;

use crate::{tok, kw, symb, lit, regex_grammar, grammar_productions};

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
            + delimited(separated(Atom(EnumVariant), symb!(Comma)), Brace),

        StructDef =>
            (kw!(Struct) | kw!(Class))
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + delimited(separated(Atom(StructField), symb!(Comma)), Paren)
            + optional(Atom(ImplementsDef))
            + optional(Atom(StructMethods)),

        InterfaceDef =>
            kw!(Interface)
            + tok!(TypeName)
            + optional(Atom(GenericParams))
            + optional(Atom(InterfaceMethods)),

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
            delimited(Atom(FunDef).star(), Brace),

        InterfaceMethods => delimited(Atom(InterfaceMethod).star(), Brace),
        InterfaceMethod => 
            kw!(Fun)
            + tok!(Identifier)
            + Atom(TypedFunParams)
            + symb!(Arrow) + Atom(Type),


        EnumVariant => tok!(TypeName) + optional(delimited(interspersed(Atom(Type), symb!(Comma)),Paren)),

        FunModifiers => optional(kw!(Pure)),
        Visibility => kw!(Public) | kw!(Private),

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
        Expr => Atom(AddLevel),

        // operator levels
        AddLevel => Atom(MulLevel) + ((symb!(Plus) | symb!(Minus)) + Atom(MulLevel)).star(),
        MulLevel => Atom(DerefLevel) + ((symb!(Asterisk) | symb!(Slash)) + Atom(FunCallLevel)).star(),
        DerefLevel =>
            Atom(FunCallLevel)
            | ((symb!(Asterisk) | symb!(Ampersand)) + Atom(DerefLevel)) ,
        FunCallLevel => Atom(MemberAccessLevel) + Atom(FunCall).star(),
        MemberAccessLevel => Atom(ExprTerm) + ((symb!(Dot) | symb!(Arrow)) + tok!(Identifier)).star(),


        FunCall => delimited(optional(interspersed(Atom(Expr), symb!(Comma))), Paren),


        Parens =>
            empty_delims(Paren)
            | delimited(interspersed(Atom(Expr), symb!(Comma)), Paren)
            | delimited(Atom(Expr), Paren)
            ,
        ExprTerm => lit!(IntLiteral) | lit!(StringLiteral) | tok!(Identifier) | Atom(Block) | Atom(LetDecl) | Atom(Parens),
        Block => empty_delims(Brace) | delimited(interspersed(Atom(Expr), symb!(Semicolon)), Brace),
        IfExpr => kw!(If) + Atom(Expr) + kw!(Then) + Atom(Expr) + kw!(Else) + Atom(Expr),

        Type => tok!(TypeName) | Atom(PtrType) | Atom(RefType) | Atom(GenericType) | Atom(FunOrTupleType),
        PtrType => (symb!(Asterisk) | kw!(MutPtr)) + Atom(Type),
        RefType => (symb!(Ampersand) | kw!(MutRef)) + Atom(Type),
        FunOrTupleType =>
            delimited(interspersed(Atom(Type), symb!(Comma)), Paren)
            | (delimited(interspersed(Atom(Type), symb!(Comma)), Paren) + symb!(Arrow) + Atom(Type))
    );

    re_grammar.into()
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum GrammarSymbol {
    // top level
    Program,
    Item,
    FunDef,
    EnumDef,
    NewTypeDef,
    TypeAliasDef,
    StructDef,
    InterfaceDef,

    Visibility,
    FunModifiers,

    EnumVariant,

    StructField,
    ImplementsDef,
    StructMethods,
    InterfaceMethods,
    InterfaceMethod,


    TypedFunParams,
    TypedFunParam,
    FunParams,
    GenericParams,
    FunValueParams,
    FunParam,

    Token(TokenCategory),
    LetDecl,
    Expr,
    ExprTerm,
    FunCall,
    Parens,
    Block,
    IfExpr,

    // Operator levels
    AddLevel,
    MulLevel,
    DerefLevel,
    FunCallLevel,
    MemberAccessLevel,

    Type,
    RefType,
    PtrType,
    GenericType,
    FunOrTupleType,
}
