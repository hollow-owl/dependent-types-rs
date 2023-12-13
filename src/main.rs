use CoC::{Term, Term::*, print};
use pest::Parser;

use crate::CoC::{TermParser, Rule};


pub mod CoC;

fn appl(f: Term, args: Vec<Term>) -> Term {
    args.into_iter().fold(f, |m, n| Term::Appl(Box::new(m), Box::new(n)))
}


fn main() {
    let n_ty = Term::pi(Star, |a: Term| {
        let a2 = a.clone();
        Term::pi(Term::pi(a.clone(), |_x| a), |_f| 
            Term::pi(a2.clone(), |_x| a2))
    });
    dbg!(n_ty);

    // let input = "(Π *.(Π (Π 0.0).(Π 0.0)))";
    // dbg!(TermParser::parse(Rule::term, input));
    // dbg!(print(0, 
    println!("{}", print(0,&Pi(Box::new(FreeVar(0)), Box::new(|_| Star))));

    // let curry2 = |f| Term::lam(|x| Term::lam(|y| f(x)(y)));
    // // let curry3 = |f: impl Fn(Term) -> impl Fn(Term) -> impl Fn(Term) -> Term| Term::lam(|x| curry2(f(x)));
    // // let curry4 = |f: impl Fn(Term) -> impl Fn(Term) -> impl Fn(Term) -> impl Fn(Term) -> Term| Term::lam(|x| curry3(f(x)));
    // // let curry5 = |f: impl Fn(Term) -> impl Fn(Term) -> impl Fn(Term) -> impl Fn(Term) -> impl Fn(Term) -> Term| Term::lam(|x| curry4(f(x)));
    // let a =|x: Term| |y: Term| y;
    // curry2(a);
    // dbg!(a)

    // let zero = Term::Ann(curry3(|_a| |_f| |x| x), n_ty.clone());
    // dbg!(zero)

    // let zero = Term::Ann(
    //     Box::new(
    //         Lam(Box::new(move |_a| 
    //             Lam(Box::new(move |_f| 
    //                 Lam(Box::new(move |x| x))))))),
    //     Box::new(n_ty.clone()),
    // );

    // let n_ty2 = n_ty.clone();
    // let succ = Term::Ann(
    //     Box::new(
    //         Lam(Box::new(move |n: Term| {
    //             Lam(Box::new(move |a: Term| {
    //                 let n = n.clone();
    //                 Lam(Box::new(move |f: Term| { 
    //                     let a = a.clone();
    //                     let n = n.clone();
    //                     Lam(Box::new(move |x: Term| {
    //                         let f = f.clone();
    //                         let a = a.clone();
    //                         let n = n.clone();
    //                         appl(f.clone(), vec![n.clone(), a.clone(), appl(n, vec![a, f, x])])
    //                     }))
    //                 }))
    //             }))
    //         }))),
    //     Box::new(Term::Pi(Box::new(n_ty.clone()), Box::new(move |_n| {
    //         let n_ty = n_ty2.clone();
    //         n_ty
    //     }))),
    // );

    // let one = appl(succ.clone(), vec![zero.clone()]);
    // let two = appl(succ.clone(), vec![one.clone()]);
    // let three = appl(succ.clone(), vec![two.clone()]);
    // let four = appl(succ.clone(), vec![three.clone()]);

    // // assert_infer(Vec::new(), zero.clone(), n_ty.clone());
    // assert_infer(Vec::new(), one.clone(), n_ty.clone());
    // // assert_infer(Vec::new(), two.clone(), n_ty.clone());
    // // assert_infer(Vec::new(), three.clone(), n_ty.clone());
    // // assert_infer(Vec::new(), four.clone(), n_ty.clone());

}