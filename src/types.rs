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
impl<T1: Into<Type>, T2: Into<Type>> From<(T1, T2)> for Type {
    fn from(name: (T1, T2)) -> Self {
        Type::func(name.0, name.1)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Named(ref name) => write!(f, "{}", name),
            Type::Func(ref arg, ref out) => write!(f, "\u{27e8}{}, {}\u{27e9}", arg, out),
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
        if let Type::Named(ref name) = *tp {
            if name == "e" {
                return write!(f, "{}", expr);
            }
        };
        write!(f, "{expr}:{tp}", expr = expr, tp = tp)
    }
}

impl<E, E1, T> From<(E1, T)> for Typed<E>
where
    E1: Into<E>,
    T: Into<Type>,
{
    fn from((expr, tp): (E1, T)) -> Self {
        Self::new(expr, tp)
    }
}
