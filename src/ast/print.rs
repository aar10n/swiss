use crate::ast::*;
use crate::print::ansi::{
    chars::{ARROW, COLON, COMMA, EQUALS, LBRAC, LPARN, RBRAC, RPARN},
    ATTR, BOLD, DIMENSION, DIRECTIVE, IDENT, KEYWORD, KIND, NUMBER, OPERATOR, PUNCT, RESET, STRING,
    UNIT,
};
use crate::print::{PrettyPrint, PrettyString, TABWIDTH};

use either::{Either, Left, Right};
use std::io;

type Context = ();

impl<T: PrettyPrint<Context>> PrettyPrint<Context> for Spanned<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        self.value().pretty_print(out, ctx, level)
    }
}

impl<T: PrettyPrint<Context>> PrettyPrint<Context> for KindNode<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;

        self.id().pretty_print(out, ctx, level)?;
        write!(out, " ")?;
        self.kind.pretty_print(out, ctx, 0)
    }
}

impl<T: PrettyPrint<Context>> PrettyPrint<Context> for ListNode<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;

        if let Some((ldelim, _)) = self.ldelim {
            write!(out, "{PUNCT}{}{RESET}", ldelim)?;
        }
        for (i, item) in self.items.iter().enumerate() {
            item.pretty_print(out, ctx, level)?;
            if i < self.items.len() - 1 {
                write!(out, "{COMMA} ")?;
            }
        }
        if let Some((rdelim, _)) = self.rdelim {
            write!(out, "{PUNCT}{}{RESET}", rdelim)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Module {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        for item in &self.items {
            item.pretty_print(out, ctx, level)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Item {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        match &self.kind {
            ItemKind::Import(path) => {
                write!(out, "{KIND}Import{RESET} ")?;
                path.pretty_print(out, ctx, level)?;
                writeln!(out)
            }
            ItemKind::Directive(directive) => {
                directive.pretty_print(out, ctx, level)?;
                writeln!(out)
            }
            ItemKind::DimDecl(decl) => decl.pretty_print(out, ctx, level),
            ItemKind::UnitDecl(decl) => decl.pretty_print(out, ctx, level),
            ItemKind::OpDecl(decl) => decl.pretty_print(out, ctx, level),
            ItemKind::FnDecl(decl) => decl.pretty_print(out, ctx, level),
            ItemKind::Expr(expr) => {
                write!(out, "{KIND}Expr{RESET} ")?;
                expr.pretty_print(out, ctx, level)?;
                writeln!(out)
            }
        }
    }
}

impl PrettyPrint<Context> for Directive {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write!(out, "{KIND}Directive{RESET} ")?;
        match &self.kind {
            DirectiveKind::Associativity(assoc) => write!(
                out,
                "{DIRECTIVE}associativity{RESET}{EQUALS}{IDENT}{:?}{RESET}",
                assoc
            ),
            DirectiveKind::BinaryCoercion(behavior) => write!(
                out,
                "{DIRECTIVE}binary_coercion{RESET}{EQUALS}{IDENT}{:?}{RESET}",
                behavior
            ),
            DirectiveKind::Coercion(behavior) => write!(
                out,
                "{DIRECTIVE}coercion{RESET}{EQUALS}{IDENT}{:?}{RESET}",
                behavior
            ),
            DirectiveKind::FloatConversion(behavior) => write!(
                out,
                "{DIRECTIVE}float_conversion{RESET}{EQUALS}{IDENT}{:?}{RESET}",
                behavior
            ),
            DirectiveKind::Precedence(prec) => write!(
                out,
                "{DIRECTIVE}precedence{RESET}{EQUALS}{NUMBER}{}{RESET}",
                prec,
            ),
            DirectiveKind::Precision(prec) => write!(
                out,
                "{DIRECTIVE}precision{RESET}{EQUALS}{NUMBER}{}{RESET}",
                prec,
            ),
        }
    }
}

impl PrettyPrint<Context> for DimDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;

        write!(out, "{KIND}DimDecl{RESET} ")?;
        self.name.pretty_print(out, ctx, 0)?;
        write!(out, " ")?;
        self.dimension.pretty_print(out, ctx, 0)?;
        writeln!(out)
    }
}

impl PrettyPrint<Context> for UnitDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;

        match &self.kind {
            UnitKind::BaseUnit => write!(out, "{KIND}BaseUnitDecl{RESET} ")?,
            UnitKind::SubUnit => write!(out, "{KIND}SubUnitDecl{RESET} ")?,
        };

        self.name.pretty_print(out, ctx, 0)?;
        write!(
            out,
            " {DIMENSION}{}{RESET}",
            self.dimension.plain_string(ctx)
        )?;
        writeln!(out)
    }
}

impl PrettyPrint<Context> for OpDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;

        write!(out, "{KIND}OpDecl{RESET} {LPARN}")?;
        self.name.pretty_print(out, ctx, 0)?;
        write!(out, "{RPARN}{LPARN}")?;
        for (i, param) in self.params.iter().enumerate() {
            match &param.anno {
                Some(Left(dim)) => write!(out, "{LBRAC}{}{RBRAC}", dim.pretty_string(ctx))?,
                Some(Right(ty)) => ty.pretty_print(out, ctx, 0)?,
                None => write!(out, "{ATTR}any{RESET}")?,
            }
            if i < self.params.len() - 1 {
                write!(out, "{COMMA} ")?;
            }
        }
        write!(out, "{RPARN} ")?;
        self.body.pretty_print(out, ctx, 0)?;
        write!(out, " {ATTR}prec{EQUALS}{NUMBER}{}{RESET}", self.prec)?;
        write!(out, " {ATTR}assoc{EQUALS}{IDENT}{:?}{RESET}", self.assoc)?;
        writeln!(out)
    }
}

impl PrettyPrint<Context> for FnDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;

        write!(out, "{KIND}FnDecl{RESET} ")?;
        self.name.pretty_print(out, ctx, 0)?;
        write!(out, " ")?;
        for param in self.params.iter() {
            param.pretty_print(out, ctx, 0)?;
            write!(out, " ")?;
        }

        if let Some(ret) = &self.ret {
            write!(out, "{ARROW} {ATTR}{}{RESET}", ret.plain_string(ctx))?;
        }
        writeln!(out)?;

        self.body.pretty_print(out, ctx, level + 1)?;
        writeln!(out)
    }
}

impl PrettyPrint<Context> for DimExpr {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;
        match &self.kind {
            DimExprKind::Mul(lhs, rhs) => {
                lhs.pretty_print(out, ctx, 0)?;
                write!(out, " * ")?;
                rhs.pretty_print(out, ctx, 0)
            }
            DimExprKind::Div(lhs, rhs) => {
                lhs.pretty_print(out, ctx, 0)?;
                write!(out, " / ")?;
                rhs.pretty_print(out, ctx, 0)
            }
            DimExprKind::Pow(lhs, rhs) => {
                lhs.pretty_print(out, ctx, 0)?;
                write!(out, " ^ ")?;
                rhs.pretty_print(out, ctx, 0)
            }
            DimExprKind::Neg(expr) => {
                write!(out, "-")?;
                expr.pretty_print(out, ctx, 0)
            }
            DimExprKind::Ident(ident) => ident.pretty_print(out, ctx, 0),
            DimExprKind::Number(number) => number.pretty_print(out, ctx, 0),
        }
    }
}

impl PrettyPrint<Context> for Expr {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;
        match &self.kind {
            ExprKind::InfixOp(op, lhs, rhs) => {
                write!(out, "{LPARN}")?;
                lhs.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                op.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                rhs.pretty_print(out, ctx, 0)?;
                write!(out, "{RPARN}")
            }
            ExprKind::PrefixOp(op, expr) => {
                op.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                expr.pretty_print(out, ctx, 0)
            }
            ExprKind::PostfixOp(expr, op) => {
                expr.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                op.pretty_print(out, ctx, 0)
            }
            ExprKind::Unit(expr, unit) => {
                expr.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                unit.pretty_print(out, ctx, 0)
            }
            ExprKind::IfElse(cond, then, else_) => {
                write!(out, "{KEYWORD}If{RESET} ")?;
                cond.pretty_print(out, ctx, 0)?;
                writeln!(out)?;

                if then.len() == 1 {
                    writeln!(
                        out,
                        "{tab}{KEYWORD}Then{RESET} {}",
                        then[0].pretty_string(ctx)
                    )?;
                } else {
                    writeln!(out, "{tab}{KEYWORD}Then{RESET}")?;
                    for item in then.iter() {
                        item.pretty_print(out, ctx, level + 1)?;
                        writeln!(out)?;
                    }
                }
                if else_.len() == 1 {
                    write!(
                        out,
                        "{tab}{KEYWORD}Else{RESET} {}",
                        else_[0].pretty_string(ctx)
                    )?;
                } else {
                    writeln!(out, "{tab}{KEYWORD}Else{RESET}")?;
                    for (i, item) in else_.iter().enumerate() {
                        item.pretty_print(out, ctx, level + 1)?;
                        if i < else_.len() - 1 {
                            writeln!(out)?;
                        }
                    }
                }
                Ok(())
            }
            ExprKind::FnCall(func, args) => {
                func.pretty_print(out, ctx, 0)?;
                write!(out, "(")?;
                for (i, arg) in args.items.iter().enumerate() {
                    arg.pretty_print(out, ctx, 0)?;
                    if i < args.items.len() - 1 {
                        write!(out, ", ")?;
                    }
                }
                write!(out, ")")
            }
            ExprKind::List(list) => {
                write!(out, "{LBRAC}")?;
                for (i, item) in list.items.iter().enumerate() {
                    item.pretty_print(out, ctx, 0)?;
                    if i < list.items.len() - 1 {
                        write!(out, ", ")?;
                    }
                }
                write!(out, "{RBRAC}")
            }
            ExprKind::Tuple(tuple) => {
                write!(out, "{LPARN}")?;
                for (i, item) in tuple.items.iter().enumerate() {
                    item.pretty_print(out, ctx, 0)?;
                    if i < tuple.items.len() - 1 {
                        write!(out, ", ")?;
                    }
                }
                write!(out, "{RPARN}")
            }
            ExprKind::Path(path) => path.pretty_print(out, ctx, level),
            ExprKind::Ident(ident) => ident.pretty_print(out, ctx, level),
            ExprKind::Number(number) => number.pretty_print(out, ctx, level),
            ExprKind::String(string) => write!(out, "{STRING}\"{}\"{RESET}", string),
            ExprKind::Boolean(boolean) => write!(out, "{NUMBER}{}{RESET}", boolean),
        }
    }
}

impl PrettyPrint<Context> for Ty {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        out.write_all(tab.as_bytes())?;
        match &self.kind {
            TyKind::Any => write!(out, "{ATTR}any{RESET}"),
            TyKind::Bool => write!(out, "{ATTR}bool{RESET}"),
            TyKind::Int => write!(out, "{ATTR}int{RESET}"),
            TyKind::Float => write!(out, "{ATTR}float{RESET}"),
            TyKind::Str => write!(out, "{ATTR}string{RESET}"),
            TyKind::Num => write!(out, "{ATTR}num{RESET}"),
            TyKind::List => write!(out, "{ATTR}list{RESET}"),
            TyKind::Tuple(tys) => {
                write!(out, "{LPARN}")?;
                for (i, ty) in tys.iter().enumerate() {
                    ty.pretty_print(out, ctx, 0)?;
                    if i < tys.len() - 1 {
                        write!(out, ", ")?;
                    }
                }
                write!(out, "{RPARN}")
            }
        }
    }
}

//

impl PrettyPrint<Context> for Param {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        self.name.pretty_print(out, ctx, level)?;
        match &self.anno {
            Some(Either::Left(dim)) => {
                write!(out, "{LBRAC}")?;
                dim.pretty_print(out, ctx, 0)?;
                write!(out, "{RBRAC}")?;
            }
            Some(Either::Right(ty)) => {
                write!(out, "{COLON}")?;
                ty.pretty_print(out, ctx, 0)?;
            }
            _ => (),
        };
        Ok(())
    }
}

impl PrettyPrint<Context> for Path {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        for (i, ident) in self.parts.iter().enumerate() {
            if i > 0 {
                write!(out, "{IDENT}::{RESET}")?;
            }
            ident.pretty_print(out, ctx, level)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Ident {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write!(out, "{IDENT}{}{RESET}", self.raw)
    }
}

impl PrettyPrint<Context> for Operator {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write!(out, "{OPERATOR}{}{RESET}", self.raw)
    }
}

impl PrettyPrint<Context> for Unit {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write!(out, "{UNIT}{}{RESET}", self.name)
    }
}

impl PrettyPrint<Context> for Number {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        match &self.kind {
            NumberKind::Integer(value) => write!(out, "{NUMBER}{}{RESET}", value),
            NumberKind::Float(value) => write!(out, "{NUMBER}{}{RESET}", value),
        }
    }
}

impl PrettyPrint<Context> for StringLit {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write!(out, "{STRING}\"{}\"{RESET}", self.value)
    }
}

impl PrettyPrint<Context> for NodeId {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        write!(out, "{IDENT}{BOLD}[{}]{RESET}", self.raw())
    }
}
