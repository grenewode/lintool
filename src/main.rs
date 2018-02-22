extern crate failure;
#[macro_use]
extern crate failure_derive;

mod error;
mod ast;
mod eval;
mod types;

use ast::{Ast, Seg, Sym};
use types::Typed;
use types::Type;

fn main() {
    let func = Ast::func(
        Typed::new("y", "e"),
        vec![Seg::expr(Ast::sym("y")), Seg::str(" is a Bob")],
    );

    // let mut program =
    //     Ast::func(Typed::new("x", "e"), func.compose("x")).compose(vec![Seg::str("Joe")]);

    // println!("{}", program);
    // loop {
    //     program = match program.eval() {
    //         Ok(new_program) => new_program,
    //         Err(error) => {
    //             println!("{}", error);
    //             break;
    //         }
    //     };
    //     println!("{}", program);

    //     if program.complete() {
    //         break;
    //     }
    // }
}
