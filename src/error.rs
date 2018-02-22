use ast::{Ast, Sym};
use types::Type;
use std::result;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "type clash: no rule of composition exists for ({})({})", _0, _1)]
    CannotCompose(Type, Type),
    #[fail(display = "type clash: expected {} but got {}", _0, _1)]
    TypeMismatch(Type, Type),
    #[fail(display = "unbound symbol {}", _0)]
    UnboundSymbol(Sym),
}

pub type Result<T> = result::Result<T, Error>;
