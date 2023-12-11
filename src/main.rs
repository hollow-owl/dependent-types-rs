use CoC::{Term, FnClone, Term::*, print, assert_infer};
use curry_macro::curry;

use crate::CoC::{equate, eval, infer_ty};

pub mod CoC;

fn appl(f: Term, args: Vec<Term>) -> Term {
    args.into_iter().fold(f, |m, n| Term::Appl(Box::new(m), Box::new(n)))
}

fn assert_beta_eq(m: Term, n: Term) {
    assert!(equate(0, (eval(m), eval(n))));
}

fn main() {
    let n_ty = Term::Pi(
        Box::new(Term::Star),
        Box::new(move |a:Term| {
            let a = a.clone();
            let a2 = a.clone();
            Term::Pi(
                Box::new(Term::Pi(
                    Box::new(a.clone()),
                    Box::new(move |_x| a.clone()),
                )),
                Box::new(move |_f| {
                    let a = a2.clone();
                    Term::Pi(Box::new(a.clone()), Box::new(move |_x| a.clone()))
                }),
            )
        }),
    );

    let zero = Term::Ann(
        Box::new(
            Lam(Box::new(move |_a| 
                Lam(Box::new(move |_f| 
                    Lam(Box::new(move |x| x))))))),
        Box::new(n_ty.clone()),
    );

    let n_ty2 = n_ty.clone();
    let succ = Term::Ann(
        Box::new(
            Lam(Box::new(move |n: Term| {
                Lam(Box::new(move |a: Term| {
                    let n = n.clone();
                    Lam(Box::new(move |f: Term| { 
                        let a = a.clone();
                        let n = n.clone();
                        Lam(Box::new(move |x: Term| {
                            let f = f.clone();
                            let a = a.clone();
                            let n = n.clone();
                            appl(f.clone(), vec![n.clone(), a.clone(), appl(n, vec![a, f, x])])
                        }))
                    }))
                }))
            }))),
        Box::new(Term::Pi(Box::new(n_ty.clone()), Box::new(move |_n| {
            let n_ty = n_ty2.clone();
            n_ty
        }))),
    );

    let one = appl(succ.clone(), vec![zero.clone()]);
    let two = appl(succ.clone(), vec![one.clone()]);
    let three = appl(succ.clone(), vec![two.clone()]);
    let four = appl(succ.clone(), vec![three.clone()]);

    // assert_infer(Vec::new(), zero.clone(), n_ty.clone());
    assert_infer(Vec::new(), one.clone(), n_ty.clone());
    // assert_infer(Vec::new(), two.clone(), n_ty.clone());
    // assert_infer(Vec::new(), three.clone(), n_ty.clone());
    // assert_infer(Vec::new(), four.clone(), n_ty.clone());

}