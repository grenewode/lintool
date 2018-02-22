extern crate failure;
#[macro_use]
extern crate failure_derive;

mod error;
mod lang;
mod types;

use lang::{Expr, Model, Seg, Sym};
use types::Typed;
use types::Type;

fn main() {
    let mut model = Model::new();
    model.define("Bob", Expr::entity("Bob"));

    let func = Expr::func(
        Typed::new("y", "e"),
        Expr::truth(vec![Seg::expr(Expr::sym("y")), Seg::str(" is a Bob")]),
    );

    let mut program = Expr::func(
        Typed::new("f", Type::func("e", "t")),
        Expr::sym("f").compose(Expr::mref("Bob")),
    ).compose(func);

    println!("{}", program);
    loop {
        program = match program.eval(&model) {
            Ok(new_program) => new_program,
            Err(error) => {
                println!("{}", error);
                break;
            }
        };
        println!("{}", program);

        if program.is_complete() {
            break;
        }
    }
}
