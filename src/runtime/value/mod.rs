mod dim;
mod number;
mod quantity;
mod ty;
mod value;

pub use dim::*;
pub use number::*;
pub use quantity::*;
pub use ty::*;
pub use value::*;

pub use super::{Context, Exception};
pub use rug::{Float, Integer};

// MARK: Numeric

pub trait Numeric {
    fn number(&self) -> &Number;
}

impl Numeric for Quantity {
    fn number(&self) -> &Number {
        &self.number
    }
}

impl Numeric for Number {
    fn number(&self) -> &Number {
        self
    }
}

// MARK: Dimensinal

pub trait Dimensional {
    fn dim(&self) -> &Dim;
}

impl Dimensional for Quantity {
    fn dim(&self) -> &Dim {
        &self.dim
    }
}

impl Dimensional for Number {
    fn dim(&self) -> &Dim {
        Dim::none_ref()
    }
}
