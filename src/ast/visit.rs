use super::ast::*;
use crate::runtime::Context;

use std::ops::{Deref, DerefMut};

// MARK: Visitor Trait

/// An AST Node visitor.
pub trait Visitor<'a, S: Default, E>: Sized {
    fn context(&mut self) -> &mut Context;
    fn push_scope(&mut self) {}
    fn pop_scope(&mut self) {}

    fn visit_module(&mut self, module: &mut Module) -> Result<S, E> {
        module.walk(self)
    }
    fn visit_item(&mut self, item: &mut Item) -> Result<S, E> {
        item.walk(self)
    }
    fn visit_directive(&mut self, directive: &mut Directive) -> Result<S, E> {
        directive.walk(self)
    }
    fn visit_dim_decl(&mut self, decl: &mut DimDecl) -> Result<S, E> {
        decl.walk(self)
    }
    fn visit_unit_decl(&mut self, decl: &mut UnitDecl) -> Result<S, E> {
        decl.walk(self)
    }
    fn visit_op_decl(&mut self, decl: &mut OpDecl) -> Result<S, E> {
        decl.walk(self)
    }
    fn visit_fn_decl(&mut self, decl: &mut FnDecl) -> Result<S, E> {
        decl.walk(self)
    }
    fn visit_param(&mut self, param: &mut Param) -> Result<S, E> {
        param.walk(self)
    }
    fn visit_dim_expr(&mut self, dim_expr: &mut DimExpr) -> Result<S, E> {
        dim_expr.walk(self)
    }
    fn visit_top_level_expr(&mut self, expr: &mut Expr) -> Result<S, E> {
        expr.walk(self)
    }
    fn visit_expr(&mut self, expr: &mut Expr) -> Result<S, E> {
        expr.walk(self)
    }
    fn visit_list(&mut self, list: &mut ListNode<Expr>) -> Result<S, E> {
        list.walk(self)
    }
    fn visit_tuple(&mut self, tuple: &mut ListNode<Expr>) -> Result<S, E> {
        tuple.walk(self)
    }
    fn visit_path(&mut self, path: &mut Path) -> Result<S, E> {
        path.walk(self)
    }
    fn visit_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        ident.walk(self)
    }
    fn visit_operator(&mut self, operator: &mut Operator) -> Result<S, E> {
        operator.walk(self)
    }
    fn visit_unit(&mut self, unit: &mut Unit) -> Result<S, E> {
        unit.walk(self)
    }
    fn visit_number(&mut self, number: &mut Number) -> Result<S, E> {
        number.walk(self)
    }
    fn visit_string(&mut self, string: &mut String) -> Result<S, E> {
        Ok(S::default())
    }
    fn visit_boolean(&mut self, boolean: &mut bool) -> Result<S, E> {
        Ok(S::default())
    }
}

/// A trait for visitable AST nodes.
pub trait Visit {
    /// Invokes the visitor method on this node.
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E>;
    /// Visits all of the children of this node.
    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E>;
}

impl<T: Visit> Visit for Box<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.deref_mut().visit(visitor)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.deref_mut().walk(visitor)
    }
}

impl<T: Visit> Visit for Option<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        if let Some(item) = self {
            item.visit(visitor);
        }
        Ok(S::default())
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        if let Some(item) = self {
            item.walk(visitor);
        }
        Ok(S::default())
    }
}

impl<T: Visit> Visit for Vec<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for item in self {
            item.visit(visitor)?;
        }
        Ok(S::default())
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for item in self {
            item.walk(visitor)?;
        }
        Ok(S::default())
    }
}

impl<T: Visit> Visit for ListNode<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for item in &mut self.items {
            item.visit(visitor)?;
        }
        Ok(S::default())
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for item in &mut self.items {
            item.walk(visitor)?;
        }
        Ok(S::default())
    }
}

//
// MARK: Visitor Impls
//

impl Visit for Module {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_module(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.items.visit(visitor)
    }
}

impl Visit for Item {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_item(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            ItemKind::Import(_) => Ok(S::default()),
            ItemKind::Directive(directive) => directive.visit(visitor),
            ItemKind::DimDecl(decl) => decl.visit(visitor),
            ItemKind::UnitDecl(decl) => decl.visit(visitor),
            ItemKind::OpDecl(decl) => decl.visit(visitor),
            ItemKind::FnDecl(decl) => decl.visit(visitor),
            ItemKind::Expr(expr) => visitor.visit_top_level_expr(expr),
        }
    }
}

impl Visit for Directive {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_directive(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for DimDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_dim_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for UnitDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_unit_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for OpDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_op_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for FnDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_fn_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for Param {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_param(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for DimExpr {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_dim_expr(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            DimExprKind::Mul(lhs, rhs) => {
                lhs.visit(visitor)?;
                rhs.visit(visitor)?;
            }
            DimExprKind::Div(lhs, rhs) => {
                lhs.visit(visitor)?;
                rhs.visit(visitor)?;
            }
            DimExprKind::Pow(lhs, rhs) => {
                lhs.visit(visitor)?;
                rhs.visit(visitor)?;
            }
            DimExprKind::Neg(expr) => {
                expr.visit(visitor)?;
            }
            DimExprKind::Ident(name) => {
                name.visit(visitor)?;
            }
            DimExprKind::Number(num) => {
                num.visit(visitor)?;
            }
        };
        Ok(S::default())
    }
}

impl Visit for Expr {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_expr(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            ExprKind::InfixOp(op, lhs, rhs) => {
                op.visit(visitor)?;
                lhs.visit(visitor)?;
                rhs.visit(visitor)?;
            }
            ExprKind::PrefixOp(op, expr) => {
                op.visit(visitor)?;
                expr.visit(visitor)?;
            }
            ExprKind::PostfixOp(op, expr) => {
                op.visit(visitor)?;
                expr.visit(visitor)?;
            }
            ExprKind::Unit(expr, unit) => {
                expr.visit(visitor)?;
                unit.visit(visitor)?;
            }
            ExprKind::IfElse(cond, then, else_) => {
                cond.visit(visitor)?;
                then.visit(visitor)?;
                else_.visit(visitor)?;
            }
            ExprKind::FnCall(func, args) => {
                func.visit(visitor)?;
                args.visit(visitor)?;
            }
            ExprKind::List(list) => {
                visitor.visit_list(list)?;
            }
            ExprKind::Tuple(tuple) => {
                visitor.visit_tuple(tuple)?;
            }
            ExprKind::Path(path) => {
                path.visit(visitor)?;
            }
            ExprKind::Ident(ident) => {
                ident.visit(visitor)?;
            }
            ExprKind::Number(num) => {
                num.visit(visitor)?;
            }
            ExprKind::String(string) => {
                visitor.visit_string(string)?;
            }
            ExprKind::Boolean(boolean) => {
                visitor.visit_boolean(boolean)?;
            }
        };
        Ok(S::default())
    }
}

impl Visit for Path {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_path(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.parts.visit(visitor)
    }
}

impl Visit for Ident {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_ident(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for Operator {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_operator(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for Unit {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_unit(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}

impl Visit for Number {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_number(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        Ok(S::default())
    }
}
