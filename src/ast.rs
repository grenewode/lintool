use std::collections::HashMap;
use std::iter::FromIterator;
use std::fmt::{self, Display, Formatter};
use error::{Error, Result};
use types::Typed;

use types::Type;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Sym(String);

impl From<String> for Sym {
    fn from(value: String) -> Self {
        Sym(value)
    }
}

impl<'a> From<&'a str> for Sym {
    fn from(value: &'a str) -> Self {
        Sym(value.into())
    }
}

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Seg<T> {
    Expr(T),
    Str(String),
}

impl<T> Seg<T> {
    pub fn expr<T1: Into<T>>(expr: T1) -> Self {
        Seg::Expr(expr.into())
    }

    pub fn str<S: Into<String>>(string: S) -> Self {
        Seg::Str(string.into())
    }
}

impl Seg<Ast<Typed<Sym>, Sym>> {
    pub fn do_bind(self, binding: &Vec<Typed<Sym>>) -> Result<Seg<Ast<Typed<Sym>, Typed<Sym>>>> {
        match self {
            Seg::Str(string) => Ok(Seg::Str(string)),
            Seg::Expr(expr) => Ok(Seg::expr(expr.do_bind(binding)?)),
        }
    }
}

impl Seg<Ast<Typed<Sym>, Typed<Sym>>> {
    pub fn do_sub(
        self,
        binding: &Typed<Sym>,
        value: Ast<Typed<Sym>, Typed<Sym>>,
    ) -> Result<Seg<Ast<Typed<Sym>, Typed<Sym>>>> {
        match self {
            Seg::Str(string) => Ok(Seg::Str(string)),
            Seg::Expr(expr) => Ok(Seg::expr(expr.do_sub(binding, value)?)),
        }
    }
}

impl<T: Display> Display for Seg<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Seg::Expr(ref expr) => expr.fmt(f),
            Seg::Str(ref string) => string.fmt(f),
        }
    }
}

pub type Str<T> = Vec<Seg<T>>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Ast<B, S> {
    Sym(S),
    Str(Str<Ast<B, S>>),
    Func(B, Box<Ast<B, S>>),
    Compose(Box<Ast<B, S>>, Box<Ast<B, S>>),
}

impl<B, S> Ast<B, S> {
    pub fn sym<S1: Into<S>>(sym: S1) -> Self {
        Ast::Sym(sym.into())
    }

    pub fn str<I: IntoIterator<Item = Seg<Self>>>(segements: I) -> Self {
        Ast::Str(Vec::from_iter(segements))
    }

    pub fn func<B1: Into<B>, A: Into<Self>>(binding: B1, body: A) -> Self {
        Ast::Func(binding.into(), Box::new(body.into()))
    }

    pub fn compose<A: Into<Self>>(self, other: A) -> Self {
        Ast::Compose(Box::new(self), Box::new(other.into()))
    }
}

impl<B, S> From<Vec<Seg<Ast<B, S>>>> for Ast<B, S> {
    fn from(segs: Vec<Seg<Self>>) -> Self {
        Ast::Str(segs)
    }
}

impl Ast<Typed<Sym>, Sym> {
    pub fn do_bind(self, bindings: &Vec<Typed<Sym>>) -> Result<Ast<Typed<Sym>, Typed<Sym>>> {
        match self {
            Ast::Sym(sym) => {
                if let Some(binding) = bindings.iter().find(|binding| binding.expr == sym) {
                    Ok(Ast::sym(binding.clone()))
                } else {
                    Err(Error::UnboundSymbol(sym))
                }
            }
            Ast::Str(segements) => Ok(Ast::str(segements
                .into_iter()
                .map(|segment| segment.do_bind(bindings))
                .collect::<Result<Vec<_>>>()?)),
            Ast::Func(arg_binding, body) => {
                if arg_binding.expr != binding.expr {
                    let body = body.do_bind(&arg_binding)?;
                    // If the function does not shadow this symbol, we can continue to substitute
                    Ok(Ast::func(arg_binding, body.do_bind(binding)?))
                } else {
                    let body = body.do_bind(&arg_binding)?;
                    // The function shadows this symbol, so we don't substitute into it
                    Ok(Ast::func(arg_binding, body))
                }
            }
        }
    }
}

impl Ast<Typed<Sym>, Typed<Sym>> {
    pub fn get_type(&self) -> Result<Type> {
        match *self {
            Ast::Sym(ref sym) => Ok(sym.tp),
            Ast::Str(..) => Ok(Type::StrLit),
            Ast::Func(ref binding, ref body) => {
                Ok(Type::Func(Box::new(binding.tp), Box::new(body.get_type()?)))
            }
            Ast::Compose(ref a, ref b) => a.get_type()?.compose(&b.get_type()?),
        }
    }

    pub fn do_compose(self, arg: Self) -> Result<Self> {
        if let Ast::Func(arg_binding, body) = self {
            body.do_sub(&arg_binding, arg)
        } else {
            Err(Error::CannotCompose(self.get_type()?, arg.get_type()?))
        }
    }

    pub fn eval(self) -> Result<Self> {
        if let Ast::Compose(a, b) = self {
            a.do_compose(*b)
        } else {
            Ok(self)
        }
    }

    pub fn do_sub(self, binding: &Typed<Sym>, value: Self) -> Result<Self> {
        match self {
            Ast::Sym(sym) => if sym.expr == binding.expr {
                if sym.tp == binding.tp {
                    Ok(value)
                } else {
                    Err(Error::TypeMismatch(sym.tp, binding.tp))
                }
            } else {
                Ok(Ast::Sym(sym))
            },
            Ast::Str(segements) => Ok(Ast::str(segements
                .into_iter()
                .map(|segment| segment.do_sub(binding, value.clone()))
                .collect::<Result<Vec<_>>>()?)),
            Ast::Func(arg_binding, body) => if arg_binding.expr != binding.expr {
                // If the function does not shadow this symbol, we can continue to substitute
                Ok(Ast::func(arg_binding, body.do_sub(binding, value)?))
            } else {
                // The function shadows this symbol, so we don't substitute into it
                Ok(Ast::func(arg_binding, *body))
            },
            Ast::Compose(a, b) => Ok(Ast::compose(
                a.do_sub(binding, value.clone())?,
                b.do_sub(binding, value.clone())?,
            )),
        }
    }

    pub fn complete(&self) -> bool {
        match *self {
            Ast::Sym(_) | Ast::Func(..) | Ast::Compose(..) => false,
            Ast::Str(ref segments) => segments.iter().all(|segement| match *segement {
                Seg::Expr(ref expr) => expr.complete(),
                Seg::Str(_) => true,
            }),
        }
    }
}

impl<B: Display, S: Display> Display for Ast<B, S> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Ast::Sym(ref sym) => sym.fmt(f),
            Ast::Str(ref string) => {
                for seg in string {
                    seg.fmt(f)?;
                }
                Ok(())
            }
            Ast::Func(ref arg, ref body) => write!(f, "λ{} [{}]", arg, body),
            Ast::Compose(ref a, ref b) => write!(f, "({})({})", a, b),
        }
    }
}
