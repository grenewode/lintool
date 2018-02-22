use std::fmt::{self, Display, Formatter};
use error::{Error, Result};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    StrLit,
    Named(String),
    Func(Box<Type>, Box<Type>),
}

impl From<String> for Type {
    fn from(name: String) -> Self {
        Type::Named(name)
    }
}

impl<'a> From<&'a str> for Type {
    fn from(name: &'a str) -> Self {
        Type::Named(name.into())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::StrLit => write!(f, "str"),
            Type::Named(ref name) => write!(f, "{}", name),
            Type::Func(ref arg, ref out) => write!(f, "<{}, {}>", arg, out),
        }
    }
}

impl Type {
    pub fn compose(&self, other: &Type) -> Result<Type> {
        if let Type::Func(ref input, ref output) = *self {
            if **input == *other {
                return Ok(**output);
            }
        }
        Err(Error::CannotCompose(self.clone(), other.clone()))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Typed<E> {
    pub expr: E,
    pub tp: Type,
}

impl<E> Typed<E> {
    pub fn new<E1: Into<E>, T: Into<Type>>(expr: E1, tp: T) -> Self {
        Self {
            expr: expr.into(),
            tp: tp.into(),
        }
    }
}

impl<E: Display> Display for Typed<E> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Typed { ref expr, ref tp } = *self;
        write!(f, "{}:{}", expr, tp)
    }
}
