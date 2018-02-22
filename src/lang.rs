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
        write!(f, "⟦{}⟧", self.0)
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

impl Seg<Expr> {
    pub fn do_sub(self, model: &Model, binding: &Typed<Sym>, value: Expr) -> Result<Seg<Expr>> {
        match self {
            Seg::Str(string) => Ok(Seg::Str(string)),
            Seg::Expr(expr) => Ok(Seg::expr(expr.do_sub(model, binding, value)?)),
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
}

impl Expr {
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
                Err(Error::CannotCompose(
                    Expr::Compose(a.clone(), b.clone()),
                    a_type,
                    b_type,
                ))
            }
        }
    }

    pub fn do_compose(self, model: &Model, arg: Self) -> Result<Self> {
        let use_function_compose = if let &Expr::Func(ref arg_binding, ..) = &self {
            arg_binding.tp == arg.get_type(model)?
        } else {
            false
        };

        if use_function_compose {
            match self {
                Expr::Func(arg_binding, body) => body.do_sub(model, &arg_binding, arg),
                _ => unreachable!(),
            }
        } else {
            let arg_tp = arg.get_type(model)?;
            let self_tp = self.get_type(model)?;
            let expr = Expr::compose(self, arg);
            Err(Error::CannotCompose(expr, self_tp, arg_tp))
        }
    }

    pub fn eval(self, model: &Model) -> Result<Self> {
        match self {
            Expr::Compose(a, b) => a.do_compose(model, *b),
            t @ Expr::TruthLit(..) => Ok(t),
            _ => Err(Error::PartialExpression(self)),
        }
    }

    pub fn do_sub(self, model: &Model, binding: &Typed<Sym>, value: Self) -> Result<Self> {
        match self {
            Expr::Sym(sym) => if sym == binding.expr {
                Ok(value)
            } else {
                Ok(Expr::Sym(sym))
            },
            Expr::TruthLit(segements) => Ok(Expr::truth(segements
                .into_iter()
                .map(|segment| segment.do_sub(model, binding, value.clone()))
                .collect::<Result<Vec<_>>>()?)),
            a @ Expr::EntityLit(..) => Ok(a),
            Expr::RefLit(ref mref) => model.lookup(mref).map(|expr| expr.clone()),
            Expr::Func(arg_binding, body) => if arg_binding.expr != binding.expr {
                // If the function does not shadow this symbol, we can continue to substitute
                Ok(Expr::func(arg_binding, body.do_sub(model, binding, value)?))
            } else {
                // The function shadows this symbol, so we don't substitute into it
                Ok(Expr::func(arg_binding, *body))
            },
            Expr::Compose(a, b) => Ok(Expr::compose(
                a.do_sub(model, binding, value.clone())?,
                b.do_sub(model, binding, value.clone())?,
            )),
        }
    }

    pub fn is_complete_with_scope(&self, syms: &mut Vec<Sym>) -> bool {
        match *self {
            Expr::Sym(ref sym) => syms.contains(sym),
            Expr::Func(ref arg_binding, ref body) => {
                syms.push(arg_binding.expr.clone());
                let body_complete = body.is_complete_with_scope(syms);
                syms.pop();
                body_complete
            }
            Expr::Compose(..) | Expr::RefLit(..) => false,
            Expr::EntityLit(_) => true,
            Expr::TruthLit(ref segments) => segments.iter().all(|segement| match *segement {
                Seg::Expr(ref expr) => expr.is_complete_with_scope(syms),
                Seg::Str(_) => true,
            }),
        }
    }

    pub fn is_complete(&self) -> bool {
        self.is_complete_with_scope(&mut Vec::new())
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
            Expr::EntityLit(ref name) => write!(f, "@{}", name),
            Expr::Func(ref arg, ref body) => write!(f, "λ{} [{}]", arg, body),
            Expr::Compose(ref a, ref b) => write!(f, "({})({})", a, b),
        }
    }
}
