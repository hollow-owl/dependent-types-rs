use CoC::{Term, FnClone, Term::*, print};
use curry_macro::curry;

use crate::CoC::{equate, eval, infer_ty};

pub mod CoC;

fn appl(f: Term, args: Vec<Term>) -> Term {
    args.into_iter().fold(f, |m, n| Term::Appl(Box::new(m), Box::new(n)))
}

fn assert_beta_eq(m: Term, n: Term) {
    assert!(equate(0, (eval(m), eval(n))));
}

fn assert_infer(ctx: Vec<Term>, t: Term, expected_ty: Term) {
    let lvl = ctx.len();
    let infered_ty = infer_ty(lvl, &ctx, &t);
    dbg!("{infered_ty}, {expected_ty}");
    assert!(equate(lvl, (infered_ty, expected_ty)))
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

    // println!("{}", print(0,&n_ty));
    // println!("{}", print(0,&zero));
    assert_infer(Vec::new(), zero.clone(), n_ty.clone());
    assert_infer(Vec::new(), one.clone(), n_ty.clone());
    assert_infer(Vec::new(), two.clone(), n_ty.clone());
    assert_infer(Vec::new(), three.clone(), n_ty.clone());
    assert_infer(Vec::new(), four.clone(), n_ty.clone());

    // let n_ty2 = n_ty.clone();
    // let add_ty = Term::Pi(
    //     Box::new(n_ty.clone()),
    //     Box::new(move |_n| {
    //         let n_ty = n_ty2.clone();
    //         Term::Pi(
    //             Box::new(n_ty.clone()),
    //             Box::new(move |_m| n_ty.clone()),
    //         )
    //     }),
    // );

    // let add = Term::Ann(
    //     Box::new(
    //         // curry5(|n, m, a, f, x| 
    //         Lam(Box::new(move |n: Term| {
    //             Lam(Box::new(move |m: Term| {
    //                 let n = n.clone();
    //                 Lam(Box::new(move |a: Term| {
    //                     let n = n.clone();
    //                     let m = m.clone();
    //                     Lam(Box::new(move |f: Term| {
    //                         let n = n.clone();
    //                         let m = m.clone();
    //                         let a = a.clone();
    //                         Lam(Box::new(move |x: Term| {
    //                             let n = n.clone();
    //                             let m = m.clone();
    //                             appl(n, vec![a.clone(), f.clone(), appl(m, vec![a.clone(), f.clone(), x.clone()])])
    //                         }))
    //                     }))
    //                 }))
    //             }))
    //         }))),
    //     Box::new(add_ty.clone()),
    // );

    // assert_infer(Vec::new(), add.clone(), add_ty.clone());

    // assert_beta_eq(appl(add.clone(), vec![zero.clone(), zero.clone()]), zero.clone());
    // assert_beta_eq(appl(add.clone(), vec![zero.clone(), one.clone()]), one.clone());
    // assert_beta_eq(appl(add.clone(), vec![one.clone(), zero.clone()]), one.clone());
    // assert_beta_eq(appl(add.clone(), vec![three.clone(), one.clone()]), four.clone());

    // let pair_ty = Term::Pi(
    //     Box::new(Term::Star),
    //     Box::new(|_a| {
    //         Term::Pi(
    //             Box::new(Term::Star),
    //             Box::new(|_b| Term::Star),
    //         )
    //     }),
    // );

    // let pair = Term::Ann(
    //     Box::new(Lam(Box::new(|a| Lam(Box::new(|b| Term::Pi(
    //         Box::new(Term::Star),
    //         Box::new(|c| Term::Pi(
    //             Box::new(Term::Pi(
    //                 Box::new(a.clone()),
    //                 Box::new(|_x| Term::Pi(
    //                     Box::new(b.clone()),
    //                     Box::new(|_y| c.clone()),
    //                 )),
    //             )),
    //             Box::new(|_f| c.clone()),
    //         )),
    //     )))))),
    //     Box::new(pair_ty.clone()),
    // );
}
// // fn curry2<F>(f: F) -> Term
// // where F: FnClone {
// //     Lam(Box::new(move |x: Term| Lam(Box::new(move |y: Term| f.call(x).call(y)))))
// // }

// // left fold association
// fn appl(f: Term,args: Vec<Term>) -> Term{
//     args.into_iter().fold(f,|m,n| Appl(Box::new(m),Box::new(n)))
// }

// fn assert_beta_eq(m: Term,n: Term) {
//     assert!(equate(0, (eval(m), eval(n))));
// }


// fn main() {
//     // church numerals
//     let n_ty = Pi(Box::new(Star), Box::new(|a| {
//         Pi(
//             Box::new(Pi(Box::new(a), Box::new(|_x| a))), 
//             Box::new(move |_f| Pi(Box::new(a), Box::new(move |_x| a))))
//     }));

    // zero and succ
    // |_a, _f, x| x

    // let zero = Ann(Box::new(Lam(Box::new(move |_a| Lam(Box::new(move |_f| Lam(Box::new(move |x| x))))))), Box::new(n_ty));
    // let succ = Ann(
    //     // Box::new(curry4(|n, a, f, x| 
    //     Box::new(Lam(Box::new(move |n| Lam(Box::new(move |a| Lam(Box::new(move |f| Lam(Box::new(move |x| 
    //         Appl(Box::new(f), Box::new(appl(n, vec![a,f,x])))))))))))),
    //     Box::new(Pi(Box::new(n_ty.clone()), Box::new(move |_n| n_ty.clone()))));

    // let one = Appl(Box::new(succ), Box::new(zero));
    // let two= Appl(Box::new(succ), Box::new(one));
    // let three = Appl(Box::new(succ), Box::new(two));
    // let four = Appl(Box::new(succ), Box::new(three));
    // let five = Appl(Box::new(succ), Box::new(four));

    // assert_infer(vec![], zero, n_ty);
    // assert_infer(vec![], one, n_ty);
    // assert_infer(vec![], two, n_ty);
    // assert_infer(vec![], three, n_ty);
    // assert_infer(vec![], four, n_ty);
// }

