use super::*;

use std::fmt::{Display, Formatter, Result, Write};

struct AstPrinter<'a, W> {
    indent: Vec<&'static str>,
    child_marker: Vec<&'static str>,
    writer: &'a mut W,
}

pub fn print_ast(ast: &Ast) -> Result {
    let mut buffer = String::new();
    write_ast(ast, &mut buffer)?;
    println!("{}", buffer);
    Ok(())
}

pub fn write_ast<W: Write>(ast: &Ast, fmt: &mut W) -> Result {
    AstPrinter::new(fmt).print_ast(ast)
}

macro_rules! print_vec {
    ($self:ident, $vec:expr, $method:ident) => {
        for i in 0..$vec.len() {
            if i + 1 == $vec.len() {
                $self.begin_last_child();
            } else {
                $self.begin_inner_child();
            }
            $self.$method(&$vec[i])?;
            $self.end_child();
        }
    };
}

macro_rules! inner {
    ($self:ident, $child:expr) => {
        $self.begin_inner_child();
        $child;
        $self.end_child();
    };
}

macro_rules! last {
    ($self:ident, $child:expr) => {
        $self.begin_last_child();
        $child;
        $self.end_child();
    };
}

impl<'a, W: Write> AstPrinter<'a, W> {
    fn new(writer: &'a mut W) -> Self {
        Self {
            indent: Vec::new(),
            child_marker: Vec::from([""]),
            writer,
        }
    }

    fn begin_inner_child(&mut self) {
        self.indent.push("  │");
        self.child_marker.push("  ├─")
    }
    fn begin_last_child(&mut self) {
        self.indent.push("  ");
        self.child_marker.push("  └─");
    }
    fn end_child(&mut self) {
        self.indent.pop();
        self.child_marker.pop();
    }

    fn print_indent(&mut self) -> Result {
        for (i, s) in self.indent.iter().enumerate() {
            if i + 1 < self.indent.len() {
                write!(self.writer, "{}", s)?
            }
        }
        write!(
            self.writer,
            "{}",
            self.child_marker[self.child_marker.len() - 1]
        )?;
        Ok(())
    }

    fn print<T: Display>(&mut self, value: &T) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "{}", value)
    }

    fn print_ast(&mut self, ast: &Ast) -> Result {
        writeln!(self.writer, "Ast")?;

        for i in 0..ast.items.len() {
            if i + 1 == ast.items.len() {
                self.begin_last_child();
            } else {
                self.begin_inner_child();
            }
            self.print_item(&ast.items[i])?;
            self.end_child();
        }

        Ok(())
    }

    fn print_item(&mut self, item: &Item) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Item [node_id={}]", item.id.0)?;
        inner!(self, self.print_visibility(&item.visibility)?);
        last!(self, self.print_item_kind(&item.kind)?);
        Ok(())
    }

    fn print_purity(&mut self, purity: &Purity) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Purity: {}", purity.kind)
    }

    fn print_visibility(&mut self, visibility: &Visibility) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Visibility: {}", visibility.kind)
    }

    fn print_mutability(&mut self, mutability: &Mutability) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Mutability: {}", mutability.kind)
    }

    fn print_staticity(&mut self, staticity: &Staticity) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Staticity: {}", staticity.kind)
    }

    fn print_item_kind(&mut self, item_kind: &ItemKind) -> Result {
        match item_kind {
            ItemKind::Enum(e) => self.print_enum(e),
            ItemKind::Struct(s) => self.print_struct(s),
            ItemKind::Fun(f) => self.print_fun(f),
            _ => Ok(()),
        }
    }

    fn print_enum(&mut self, e: &Enum) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "enum {}", e.ident)?;
        inner!(self, self.print_generic_params(&e.generic_params)?);
        last!(self, self.print_enum_variants(&e.variants)?);
        Ok(())
    }

    fn print_enum_variants(&mut self, variants: &[EnumVariant]) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Variants")?;
        print_vec!(self, variants, print_enum_variant);
        Ok(())
    }

    fn print_enum_variant(&mut self, variant: &EnumVariant) -> Result {
        self.print_indent()?;
        writeln!(
            self.writer,
            "Variant {} [node_id={}]",
            variant.ident, variant.id.0
        )?;
        last!(self, {
            self.print(&"Fields")?;
            print_vec!(self, &variant.fields, print_ty)
        });
        Ok(())
    }

    fn print_struct(&mut self, s: &Struct) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "struct {}", s.ident)?;
        inner!(self, self.print_generic_params(&s.generic_params)?);
        inner!(self, self.print_struct_fields(&s.fields)?);
        inner!(self, self.print_implements(&s.implements)?);
        last!(self, self.print_struct_methods(&s.methods)?);
        Ok(())
    }

    fn print_fun(&mut self, f: &Fun) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "fun {} [node_id={}]", f.ident, f.id.0)?;
        inner!(self, self.print_purity(&f.purity)?);
        inner!(self, self.print_generic_params(&f.generic_params)?);
        inner!(self, self.print_fun_params(&f.params)?);
        inner!(self, self.print_return_ty(&f.return_ty)?);
        last!(self, self.print_expr(&f.expr)?);
        Ok(())
    }

    fn print_return_ty(&mut self, ty: &Ty) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Return Type")?;
        last!(self, self.print_ty(ty)?);
        Ok(())
    }

    fn print_fun_params(&mut self, params: &[FunParam]) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Fun Params")?;
        print_vec!(self, params, print_fun_param);
        Ok(())
    }

    fn print_fun_param(&mut self, param: &FunParam) -> Result {
        self.print_indent()?;
        writeln!(
            self.writer,
            "Param {} [node_id={}]",
            param.ident, param.id.0
        )?;
        inner!(self, self.print_mutability(&param.mutability)?);
        last!(self, self.print_ty(&param.ty)?);
        Ok(())
    }

    fn print_generic_params(&mut self, params: &[TyName]) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Generic Params")?;
        print_vec!(self, params, print_ty_name);
        Ok(())
    }

    fn print_ty_name(&mut self, ty_name: &TyName) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "{} [node_id={}]", ty_name.ident, ty_name.id.0)
    }

    fn print_struct_fields(&mut self, fields: &[StructField]) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Fields")?;
        print_vec!(self, fields, print_struct_field);
        Ok(())
    }

    fn print_struct_field(&mut self, field: &StructField) -> Result {
        self.print_indent()?;
        writeln!(
            self.writer,
            "Field {} [node_id={}]",
            field.ident, field.id.0
        )?;
        inner!(self, self.print_visibility(&field.visibility)?);
        inner!(self, self.print_mutability(&field.mutability)?);
        last!(self, self.print_ty(&field.ty)?);
        Ok(())
    }

    fn print_ty(&mut self, ty: &Ty) -> Result {
        self.print_indent()?;
        match &ty.kind {
            TyKind::Omitted => {
                writeln!(self.writer, "Omitted Type [node_id={}]", ty.id.0)
            }
            TyKind::Primitive(inner) => {
                writeln!(self.writer, "Primitive Type [node_id={}]", ty.id.0)?;
                inner!(self, self.print_path(&inner.path)?);
                last!(self, self.print_generic_args(&inner.generic_args)?);
                Ok(())
            }
            TyKind::Ref(mutability, inner) => {
                writeln!(self.writer, "Reference Type [node_id={}]", ty.id.0)?;
                inner!(self, self.print_mutability(mutability)?);
                last!(self, self.print_ty(inner)?);
                Ok(())
            }

            TyKind::Ptr(mutability, inner) => {
                writeln!(self.writer, "Pointer Type [node_id={}]", ty.id.0)?;
                inner!(self, self.print_mutability(mutability)?);
                last!(self, self.print_ty(inner)?);
                Ok(())
            }

            TyKind::Tuple(inner) => {
                writeln!(self.writer, "Pointer Type [node_id={}]", ty.id.0)?;
                self.begin_last_child();
                print_vec!(self, inner, print_ty);
                self.end_child();
                Ok(())
            }

            TyKind::Fun(inner) => {
                writeln!(self.writer, "Function Type [node_id={}]", ty.id.0)?;
                Ok(())
            }
        }
    }

    fn print_path(&mut self, path: &[TyName]) -> Result {
        self.print(&"Path")?;
        print_vec!(self, path, print_ty_name);
        Ok(())
    }

    fn print_generic_args(&mut self, args: &[Ty]) -> Result {
        self.print(&"Generic Args")?;
        print_vec!(self, args, print_ty);
        Ok(())
    }

    fn print_implements(&mut self, implements: &[Ty]) -> Result {
        self.print(&"Implements")?;
        print_vec!(self, implements, print_ty);
        Ok(())
    }

    fn print_struct_methods(&mut self, methods: &[StructMethod]) -> Result {
        self.print(&"Methods")?;
        print_vec!(self, methods, print_struct_method);
        Ok(())
    }

    fn print_struct_method(&mut self, method: &StructMethod) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Struct Method [node_id={}]", method.id.0)?;
        inner!(self, self.print_visibility(&method.visibility)?);
        inner!(self, self.print_staticity(&method.staticity)?);
        inner!(self, self.print_mutability(&method.mutability)?);
        last!(self, self.print_fun(&method.fun)?);
        Ok(())
    }

    fn print_expr(&mut self, expr: &Expr) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Expr [node_id={}]", expr.id.0)?;
        self.begin_last_child();
        match &expr.kind {
            ExprKind::Empty => {
                self.print(&"Empty")?;
            }
            ExprKind::IntLiteral(literal) => {
                self.print_indent()?;
                writeln!(self.writer, "Int Literal {}", literal.content)?;
            }
            ExprKind::FloatLiteral(literal) => {
                self.print_indent()?;
                writeln!(self.writer, "Float Literal {}", literal.content)?;
            }
            ExprKind::BoolLiteral(literal) => {
                self.print_indent()?;
                writeln!(self.writer, "Bool Literal {}", literal.content)?;
            }
            ExprKind::StringLiteral(literal) => {
                self.print_indent()?;
                writeln!(self.writer, "String Literal {}", literal.content)?;
            }
            ExprKind::QualifiedIdentifier(path) => {
                self.print(&"Qualified Identifier")?;
                last!(self, print_vec!(self, path, print_identifier));
            }
            ExprKind::Unary(op, expr) => {
                self.print(&"Unary")?;
                inner!(self, self.print_unary_operator(op)?);
                last!(self, self.print_expr(expr)?);
            }
            ExprKind::Binary(lhs, op, rhs) => {
                self.print(&"Binary")?;
                inner!(self, self.print_expr(lhs)?);
                inner!(self, self.print_binary_operator(op)?);
                last!(self, self.print_expr(rhs)?);
            }
            ExprKind::Block(block) => {
                self.print(&"Block")?;
                last!(self, print_vec!(self, block.stmts, print_stmt));
            }
            ExprKind::FunCall(fun, args) => {
                self.print(&"Fun Call")?;
                inner!(self, self.print_expr(fun)?);
                last!(self, {
                    self.print(&"Args")?;
                    print_vec!(self, args, print_expr)
                });
            }
            ExprKind::MemberAccess(expr, ident) => {
                self.print(&"Member Access")?;
                inner!(self, self.print_expr(expr)?);
                last!(self, self.print_ident(ident)?);
            }
            ExprKind::PointerMemberAccess(expr, ident) => {
                self.print(&"Pointer Member Access")?;
                inner!(self, self.print_expr(expr)?);
                last!(self, self.print_ident(ident)?);
            }
            ExprKind::If(if_expr) => {
                self.print(&"If")?;
                inner!(self, self.print_expr(&if_expr.condition)?);
                inner!(self, self.print_block(&if_expr.then_block)?);
                last!(self, {
                    self.print(&"Else")?;
                    last!(self, self.print_block(&if_expr.else_block)?);
                });
            }
            ExprKind::Break => {
                self.print(&"Break")?;
            }
            ExprKind::Continue => {
                self.print(&"Continue")?;
            },
        }
        self.end_child();

        Ok(())
    }

    fn print_block(&mut self, block: &Block) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Block [node_id={}]", block.id.0)?;
        last!(self, print_vec!(self, &block.stmts, print_stmt));
        Ok(())
    }

    fn print_ident(&mut self, ident: &Ident) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Identifier {}", ident)
    }

    fn print_stmt(&mut self, stmt: &Stmt) -> Result {
        self.print_indent()?;
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                writeln!(self.writer, "Expr Stmt [node_id={}]", stmt.id.0)?;
                last!(self, self.print_expr(expr)?);
            }
            StmtKind::Let(decl) => {
                writeln!(self.writer, "Let Stmt [node_id={}]", stmt.id.0)?;
                inner!(self, self.print_ident(&decl.ident)?);
                inner!(self, self.print_ty(&decl.ty)?);
                last!(self, self.print_expr(&decl.expr)?);
            }
            StmtKind::For(for_loop) => {
                writeln!(self.writer, "For Stmt [node_id={}]", stmt.id.0)?;
                inner!(self, self.print_identifier(&for_loop.identifier)?);
                inner!(self, self.print_range_expr(&for_loop.range)?);
                last!(self, self.print_block(&for_loop.body)?);
            }
            StmtKind::While(while_loop) => {
                writeln!(self.writer, "While Stmt [node_id={}]", stmt.id.0)?;
                inner!(self, self.print_expr(&while_loop.condition)?);
                last!(self, self.print_block(&while_loop.body)?);
            }
            StmtKind::Loop(loop_loop) => {
                writeln!(self.writer, "Loop Stmt [node_id={}]", stmt.id.0)?;
                last!(self, self.print_block(&loop_loop.body)?);
            }
        }
        Ok(())
    }

    fn print_range_expr(&mut self, range: &RangeExpr) -> Result {
        self.print_indent()?;
        match &range.kind {
            RangeKind::Open(begin, end) => {
                writeln!(self.writer, "Open Range [node_id={}]", range.id.0)?;
                inner!(self, self.print_expr(begin)?);
                last!(self, self.print_expr(end)?);
            }
            RangeKind::Closed(begin, end) => {
                writeln!(self.writer, "Closed Range [node_id={}]", range.id.0)?;
                inner!(self, self.print_expr(begin)?);
                last!(self, self.print_expr(end)?);
            }
        }
        Ok(())
    }

    fn print_binary_operator(&mut self, op: &BinaryOperator) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Binary Operator: {}", op)
    }

    fn print_unary_operator(&mut self, op: &UnaryOperator) -> Result {
        self.print_indent()?;
        writeln!(self.writer, "Unary Operator: {}", op)
    }

    fn print_identifier(&mut self, ident: &Identifier) -> Result {
        self.print_indent()?;
        writeln!(
            self.writer,
            "Identifier {} [node_id={}]",
            ident.ident, ident.id.0
        )
    }
}

impl Display for VisibilityKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        match self {
            Self::Public => write!(fmt, "public"),
            Self::Internal => write!(fmt, "internal"),
            Self::Private => write!(fmt, "private"),
        }
    }
}

impl Display for MutabilityKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        match self {
            Self::Mutable => write!(fmt, "mutable"),
            Self::Immutable => write!(fmt, "immutable"),
        }
    }
}

impl Display for PurityKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        match self {
            Self::Pure => write!(fmt, "pure"),
            Self::Impure => write!(fmt, "impure"),
        }
    }
}

impl Display for StaticityKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        match self {
            Self::Static => write!(fmt, "static"),
            Self::Instance => write!(fmt, "instance"),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        match self {
            Self::Add => write!(fmt, "+ (addition)"),
            Self::Sub => write!(fmt, "- (subtraction)"),
            Self::Mul => write!(fmt, "* (multiplication)"),
            Self::Div => write!(fmt, "/ (division)"),
            Self::Mod => write!(fmt, "% (remainder)"),
            Self::And => write!(fmt, "&& (boolean and)"),
            Self::BitAnd => write!(fmt, "& (bitwise and)"),
            Self::BitOr => write!(fmt, "| (bitwise or)"),
            Self::Or => write!(fmt, "|| (boolean or)"),
            Self::Xor => write!(fmt, "^ (xor)"),
            Self::BitShiftLeft => write!(fmt, "<< (bitwise shift left)"),
            Self::BitShiftRight => write!(fmt, ">> (bitwise shift right)"),
            Self::Eq => write!(fmt, "== (equal)"),
            Self::Neq => write!(fmt, "!= (not equal)"),
            Self::Lt => write!(fmt, "< (less)"),
            Self::Leq => write!(fmt, "<= (less or equal)"),
            Self::Gt => write!(fmt, "> (greater)"),
            Self::Geq => write!(fmt, ">= (greater or equal)"),
            Self::Assign => write!(fmt, "= (assignment)"),
            Self::AddAssign => write!(fmt, "+= (addition assignment)"),
            Self::SubAssign => write!(fmt, "-= (subtraction assignment)"),
            Self::MulAssign => write!(fmt, "*= (multiplication assignment)"),
            Self::DivAssign => write!(fmt, "/= (division assignment)"),
            Self::ModAssign => write!(fmt, "%= (remainder assignment)"),
            Self::XorAssign => write!(fmt, "^= (xor assignment)"),
            Self::BitAndAssign => write!(fmt, "&= (bitwise and assignment)"),
            Self::BitOrAssign => write!(fmt, "|= (bitwise or assignment)"),
            Self::BitShiftLeftAssign => write!(fmt, "<<= (bitwise shift left assignment)"),
            Self::BitShiftRightAssign => write!(fmt, ">>= (bitwise shift right assignment)"),
        }
    }
}
impl Display for UnaryOperator {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        match self {
            Self::Deref => write!(fmt, "* (dereference)"),
            Self::Ref => write!(fmt, "& (reference)"),
            Self::Minus => write!(fmt, "- (minus)"),
            Self::Neg => write!(fmt, "! (negation)"),
            Self::Not => write!(fmt, "~ (bitwise not)"),
        }
    }
}
