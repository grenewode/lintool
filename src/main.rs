extern crate failure;
#[macro_use]
extern crate failure_derive;

mod error;
mod lang;
mod types;

use lang::{Expr, Model, Seg, Sym};
use types::Typed;
use types::Type;

fn eval(mut program: Expr, model: &Model) {
    // Run typechecking
    if let Err(error) = program.get_type(model) {
        println!("{}", error);
        return;
    }
    println!("{}", program);
    loop {
        program = match program.eval(model) {
            Ok((new_program, partial)) => {
                if !partial {
                    break;
                }
                println!("{}", new_program);
                new_program
            }
            Err(error) => {
                println!("{}", error);
                break;
            }
        };
    }
}

fn main() {
    let mut model = Model::new();
    model.define(
        "whimpered",
        Expr::func(
            ("x", "e"),
            Expr::truth(vec![Seg::expr(Expr::sym("x")), Seg::str(" whimpered")]),
        ),
    );
    model.define(
        "loathes",
        Expr::func(
            ("x", "e"),
            Expr::func(
                ("y", "e"),
                Expr::truth(vec![
                    Seg::expr(Expr::sym("x")),
                    Seg::str(" loathes "),
                    Seg::expr(Expr::sym("y")),
                ]),
            ),
        ),
    );
    model.define(
        "not",
        Expr::func(
            ("f", Type::func("e", "t")),
            Expr::func(
                ("x", "e"),
                Expr::truth(vec![
                    Seg::expr(Expr::compose(Expr::sym("f"), Expr::sym("x"))),
                    Seg::str(" = 0"),
                ]),
            ),
        ),
    );

    eval(Expr::mref("whimpered").compose(Expr::mref("not")), &model);
}
