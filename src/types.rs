use std::fmt::{self, Display, Formatter};
use error::{Error, Result};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Named(String),
    Func(Box<Type>, Box<Type>),
}

impl Type {
    pub fn named<N: Into<String>>(name: N) -> Self {
        Type::Named(name.into())
    }
    pub fn func<A: Into<Type>, B: Into<Type>>(arg: A, body: B) -> Self {
        Type::Func(Box::new(arg.into()), Box::new(body.into()))
    }
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
            Type::Named(ref name) => write!(f, "{}", name),
            Type::Func(ref arg, ref out) => write!(f, "<{}, {}>", arg, out),
        }
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
