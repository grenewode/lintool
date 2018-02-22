use lang::{Expr, Ref, Sym};
use types::Type;
use std::result;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "type clash in {}: no rule of composition exists for ({})({})", _0, _1, _2)]
    CannotCompose(Expr, Type, Type),
    #[fail(display = "type clash in {}: expected {} but got {}", _0, _1, _2)]
    TypeMismatch(Expr, Type, Type),
    #[fail(display = "unbound symbol {}", _0)]
    UnboundSymbol(Sym),
    #[fail(display = "partial expression {}", _0)]
    PartialExpression(Expr),
    #[fail(display = "Unknown Model Ref {}", _0)]
    UnknownModelRef(Ref),
}

pub type Result<T> = result::Result<T, Error>;
