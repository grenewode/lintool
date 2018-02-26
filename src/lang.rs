use std::collections::HashMap;
use std::iter::FromIterator;
use std::fmt::{self, Display, Formatter};
use error::{Error, Result};
use types::Typed;

use types::Type;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ref(String);

impl From<String> for Ref {
    fn from(value: String) -> Self {
        Ref(value)
    }
}

impl<'a> From<&'a str> for Ref {
    fn from(value: &'a str) -> Self {
        Ref(value.into())
    }
}

impl Display for Ref {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "\u{27e6}{}\u{27e7}", self.0)
    }
}

pub struct Model(HashMap<Ref, Expr>);

impl Model {
    pub fn new() -> Self {
        Model(HashMap::new())
    }

    pub fn lookup(&self, mref: &Ref) -> Result<&Expr> {
        self.0
            .get(mref)
            .ok_or_else(|| Error::UnknownModelRef(mref.clone()))
    }

    pub fn define<R: Into<Ref>, E: Into<Expr>>(&mut self, mref: R, expr: E) {
        self.0.insert(mref.into(), expr.into());
    }
}

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

impl From<Expr> for Seg<Expr> {
    fn from(expr: Expr) -> Seg<Expr> {
        Seg::Expr(expr)
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

impl<T> From<String> for Seg<T> {
    fn from(string: String) -> Self {
        Seg::str(string)
    }
}
impl<'a, T> From<&'a str> for Seg<T> {
    fn from(string: &'a str) -> Self {
        Seg::str(string)
    }
}

impl Seg<Expr> {
    pub fn subs(self, model: &Model, binding: &Typed<Sym>, value: Expr) -> Result<Seg<Expr>> {
        match self {
            Seg::Str(string) => Ok(Seg::Str(string)),
            Seg::Expr(expr) => expr.subs(model, binding, value).map(Seg::expr),
        }
    }

    pub fn do_compose(self, model: &Model) -> Result<(Seg<Expr>, bool)> {
        match self {
            Seg::Str(string) => Ok((Seg::Str(string), false)),
            Seg::Expr(expr) => {
                let (new_expr, changed) = expr.do_compose(model)?;
                Ok((Seg::expr(new_expr), changed))
            }
        }
    }

    pub fn do_expand(self, model: &Model) -> Result<(Seg<Expr>, bool)> {
        match self {
            Seg::Str(string) => Ok((Seg::Str(string), false)),
            Seg::Expr(expr) => {
                let (new_expr, changed) = expr.do_expand(model)?;
                Ok((Seg::expr(new_expr), changed))
            }
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
pub enum Expr {
    Sym(Sym),
    TruthLit(Str<Expr>),
    EntityLit(String),
    RefLit(Ref),
    Func(Typed<Sym>, Box<Expr>),
    Compose(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn sym<S: Into<Sym>>(sym: S) -> Self {
        Expr::Sym(sym.into())
    }

    pub fn truth<I: IntoIterator<Item = Seg<Self>>>(segements: I) -> Self {
        Expr::TruthLit(Vec::from_iter(segements))
    }

    pub fn entity<S: Into<String>>(name: S) -> Self {
        Expr::EntityLit(name.into())
    }

    pub fn func<B: Into<Typed<Sym>>, A: Into<Self>>(binding: B, body: A) -> Self {
        Expr::Func(binding.into(), Box::new(body.into()))
    }

    pub fn compose<A: Into<Self>>(self, other: A) -> Self {
        Expr::Compose(Box::new(self), Box::new(other.into()))
    }

    pub fn mref<R: Into<Ref>>(mref: R) -> Self {
        Expr::RefLit(mref.into())
    }

    pub fn append_truth<T: Into<Seg<Self>>>(self, truth: T) -> Self {
        match self {
            Expr::TruthLit(mut all) => {
                all.push(truth.into());
                Expr::truth(all)
            }
            _ => Expr::truth(vec![self.into(), truth.into()]),
        }
    }

    pub fn get_type(&self, model: &Model) -> Result<Type> {
        match *self {
            Expr::Sym(ref sym) => Err(Error::UnboundSymbol(sym.clone())),
            Expr::TruthLit(..) => Ok(Type::named("t")),
            Expr::EntityLit(..) => Ok(Type::named("e")),
            Expr::RefLit(ref mref) => model.lookup(mref).and_then(|expr| expr.get_type(model)),
            Expr::Func(ref binding, ref body) => Ok(Type::Func(
                Box::new(binding.tp.clone()),
                Box::new(body.get_type(model)?),
            )),
            Expr::Compose(ref a, ref b) => {
                let a_type = a.get_type(model)?;
                let b_type = b.get_type(model)?;
                if let &Type::Func(ref input, ref output) = &a_type {
                    if **input == b_type {
                        return Ok(*output.clone());
                    }
                }
                Err(Error::CannotCompose(*a.clone(), *b.clone(), a_type, b_type))
            }
        }
    }

    pub fn subs(self, model: &Model, binding: &Typed<Sym>, value: Self) -> Result<Self> {
        match self {
            Expr::EntityLit(..) | Expr::RefLit(..) => Ok(self),
            Expr::Sym(sym) => if sym == binding.expr {
                Ok(value)
            } else {
                Ok(Expr::sym(sym))
            },
            Expr::TruthLit(segements) => Ok(Expr::truth(segements
                .into_iter()
                .map(|segment| segment.subs(model, binding, value.clone()))
                .collect::<Result<Vec<_>>>()?)),
            Expr::Func(arg_binding, body) => if arg_binding.expr != binding.expr {
                // If the function does not shadow this symbol, we can continue to substitute
                Ok(Expr::func(arg_binding, body.subs(model, binding, value)?))
            } else {
                // The function shadows this symbol, so we don't substitute into it
                Ok(Expr::func(arg_binding, *body))
            },
            Expr::Compose(a, b) => Ok(Expr::compose(
                a.subs(model, binding, value.clone())?,
                b.subs(model, binding, value.clone())?,
            )),
        }
    }

    pub fn do_expand(self, model: &Model) -> Result<(Self, bool)> {
        match self {
            Expr::EntityLit(..) | Expr::Sym(..) => Ok((self, false)),
            Expr::RefLit(mref) => model.lookup(&mref).map(|expr| (expr.clone(), true)),
            Expr::Func(arg, body) => {
                let (body, changed) = body.do_expand(model)?;
                Ok((Expr::func(arg, body), changed))
            }
            Expr::TruthLit(segements) => {
                let mut any_changed = false;
                Ok((
                    Expr::truth(segements
                        .into_iter()
                        .map(|segment| {
                            if any_changed {
                                return Ok(segment);
                            }
                            let (seg, changed) = segment.do_expand(model)?;
                            any_changed = changed;
                            Ok(seg)
                        })
                        .collect::<Result<Vec<_>>>()?),
                    any_changed,
                ))
            }
            Expr::Compose(a, b) => {
                let (new_b, changed) = b.do_expand(model)?;
                if changed {
                    return Ok((Expr::compose(*a, new_b), true));
                }
                let a = *a;
                match a {
                    Expr::Func(arg, body) => Ok((body.subs(model, &arg, new_b)?, true)),
                    a @ _ => {
                        let (new_a, changed) = a.do_expand(model)?;
                        Ok((Expr::compose(new_a, new_b), changed))
                    }
                }
            }
        }
    }

    pub fn do_compose(self, model: &Model) -> Result<(Self, bool)> {
        match self {
            Expr::EntityLit(..) | Expr::Sym(..) | Expr::Func(..) | Expr::RefLit(..) => {
                Ok((self, false))
            }
            Expr::TruthLit(segements) => {
                let mut any_changed = false;
                Ok((
                    Expr::truth(segements
                        .into_iter()
                        .map(|segment| {
                            if any_changed {
                                return Ok(segment);
                            }
                            let (seg, changed) = segment.do_compose(model)?;
                            any_changed = changed;
                            Ok(seg)
                        })
                        .collect::<Result<Vec<_>>>()?),
                    any_changed,
                ))
            }
            Expr::Compose(a, b) => {
                let (new_b, changed) = b.do_compose(model)?;
                if changed {
                    return Ok((Expr::compose(*a, new_b), true));
                }
                let a = *a;
                match a {
                    Expr::Func(arg, body) => {
                        let arg_tp = &arg.tp;
                        let b_tp = new_b.get_type(model)?;
                        if *arg_tp == b_tp {
                            Ok((body.subs(model, &arg, new_b)?, true))
                        } else {
                            Err(Error::CannotCompose(
                                Expr::Func(arg.clone(), body),
                                new_b,
                                arg_tp.clone(),
                                b_tp,
                            ))
                        }
                    }
                    a @ _ => {
                        let (new_a, changed) = a.do_compose(model)?;
                        Ok((Expr::compose(new_a, new_b), changed))
                    }
                }
            }
        }
    }

    pub fn eval(self, model: &Model) -> Result<(Self, bool)> {
        let (composed, changed) = self.do_compose(model)?;
        if changed {
            return Ok((composed, true));
        }
        composed.do_expand(model)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Expr::Sym(ref sym) => sym.fmt(f),
            Expr::TruthLit(ref string) => {
                for seg in string {
                    seg.fmt(f)?;
                }
                Ok(())
            }
            Expr::RefLit(ref mref) => mref.fmt(f),
            Expr::EntityLit(ref name) => name.fmt(f),
            Expr::Func(ref arg, ref body) => write!(f, "\u{03bb}{} [{}]", arg, body),
            Expr::Compose(ref a, ref b) => write!(f, "{}({})", a, b),
        }
    }
}
