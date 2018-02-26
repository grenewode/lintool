use lang::{Expr, Ref, Sym};
use types::Type;
use std::result;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "type clash in ({})({}): cannot compose ({}) with ({})", _0, _1, _2, _3)]
    CannotCompose(Expr, Expr, Type, Type),
    #[fail(display = "unbound symbol {}", _0)]
    UnboundSymbol(Sym),
    #[fail(display = "Unknown Model Ref {}", _0)]
    UnknownModelRef(Ref),
}

pub type Result<T> = result::Result<T, Error>;
