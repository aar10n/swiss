pub mod ansi;

use ansi::StripAnsi;

use const_format::concatcp;
use either::{Either, Left, Right};
use std::io;

pub const TABWIDTH: &str = "  ";

/// A trait for values that can be pretty-printed.
pub trait PrettyPrint<Ctx, Info: Clone = usize> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()>;
}

/// A trait for values that can be pretty-printed to a string.
pub trait PrettyString<Ctx, Info: Clone>: PrettyPrint<Ctx, Info> {
    fn pretty_string(&self, ctx: &Ctx) -> String;
    fn plain_string(&self, ctx: &Ctx) -> String;
    fn print_stdout(&self, ctx: &Ctx) -> io::Result<()>;
    fn pretty_string_info(&self, ctx: &Ctx, info: Info) -> String {
        let mut buf = Vec::new();
        self.pretty_print(&mut buf, ctx, info).unwrap();
        String::from_utf8(buf).unwrap()
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Default + Clone> PrettyString<Ctx, Info> for T {
    fn pretty_string(&self, ctx: &Ctx) -> String {
        let mut buf = Vec::new();
        self.pretty_print(&mut buf, ctx, Info::default()).unwrap();
        String::from_utf8(buf).unwrap()
    }

    fn plain_string(&self, ctx: &Ctx) -> String {
        let mut buf = Vec::new();
        self.pretty_print(&mut StripAnsi::new(&mut buf), ctx, Info::default())
            .unwrap();
        String::from_utf8(buf).unwrap()
    }

    fn print_stdout(&self, ctx: &Ctx) -> io::Result<()> {
        self.pretty_print(&mut io::stdout(), ctx, Info::default())
    }
}

//
// MARK: PrettyPrint default impls
//

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Vec<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        for item in self {
            item.pretty_print(out, ctx, info.clone())?;
        }
        Ok(())
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Option<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        if let Some(item) = self {
            item.pretty_print(out, ctx, info)?;
        }
        Ok(())
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, E: std::fmt::Debug, Info: Clone> PrettyPrint<Ctx, Info>
    for Result<T, E>
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        match self {
            Ok(item) => item.pretty_print(out, ctx, info),
            Err(err) => write!(out, "{:?}", err),
        }
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Box<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        (**self).pretty_print(out, ctx, info)
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for &T {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        (**self).pretty_print(out, ctx, info)
    }
}

impl<L: PrettyPrint<Ctx, Info>, R: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info>
    for Either<L, R>
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        match self {
            Left(l) => l.pretty_print(out, ctx, info),
            Right(r) => r.pretty_print(out, ctx, info),
        }
    }
}
