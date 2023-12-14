use std::fmt;

use pest::Parser;
use pest_derive::Parser;

use Term::*;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TermParser;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    Lam(Box<Term>), // (λ 0)
    Pi(Box<Term>, Box<Term>), // (Π 0.0)
    Appl(Box<Term>, Box<Term>), // Application ((λ 0) 1)
    Ann(Box<Term>, Box<Term>), // Annotation ((λ 0) : (Π 0.0))
    FreeVar(usize), // 0
    Star, // *
    Bx // ☐
}

impl Term {
    pub fn lam(f: Term) -> Term { Term::Lam(Box::new(f)) }
    pub fn pi(a: Term, f: Term) -> Term { Term::Pi(Box::new(a), Box::new(f)) }
    pub fn appl(m: Term, n: Term) -> Term { Term::Appl(Box::new(m), Box::new(n)) }
    pub fn ann(m: Term, a: Term) -> Term { Term::Ann(Box::new(m), Box::new(a)) }

    pub fn parse(input: &str) -> Result<Term, pest::error::Error<Rule>> {
        let mut parsed = TermParser::parse(Rule::term, input)?;
        dbg!(&parsed);
        Term::parse_term(parsed.next().unwrap())
    }

    pub fn parse_term(pair: pest::iterators::Pair<Rule>) -> Result<Term, pest::error::Error<Rule>> {
        match pair.as_rule() {
            Rule::term => Self::parse_term(pair.into_inner().next().unwrap()),
            Rule::lambda => {
                let body = Self::parse_term(pair.into_inner().nth(1).unwrap())?;
                Ok(Self::lam(body))
            },
            Rule::pi => {
                let mut pair = pair.into_inner();
                let a = Self::parse_term(pair.nth(1).unwrap())?;
                let f = Self::parse_term(pair.nth(1).unwrap())?;
                Ok(Self::pi(a,f))
            },
            Rule::application => {
                let mut pair = pair.into_inner();
                let a = Self::parse_term(pair.nth(0).unwrap())?;
                let f = Self::parse_term(pair.nth(1).unwrap())?;
                Ok(Self::appl(a,f))
            },
            Rule::ann => {
                let mut pair = pair.into_inner();
                let a = Self::parse_term(pair.nth(0).unwrap())?;
                let f = Self::parse_term(pair.nth(2).unwrap())?;
                Ok(Self::ann(a,f))
            },
            Rule::variable => Ok(FreeVar(pair.as_str().parse().unwrap())),
            Rule::star => Ok(Star),
            Rule::bx => Ok(Bx),
            _ => unreachable!(),
        }
    }

    fn eval(self, ctx: &Vec<Term>) -> Term { 
        println!("{}",&self);
        match self {
            Lam(f) => Self::lam(f.eval(ctx)),
            Pi(a, f) => Self::pi(a.eval(ctx), f.eval(ctx)),
            Appl(m, n) => match (m.eval(ctx), n.eval(ctx)) {
                (Lam(f), n) => f.eval(ctx),
                (m, n) => Self::appl(m, n),
            },
            Ann(m, _a) => m.eval(ctx),
            x => x,
        }
    }

    // fn subsitute(self, index: usize, replacement: &Term) -> Term {
    //     match self {
    //         Lam(f) => Self::lam(f.subsitute(index+1, replacement)),
    //         Pi(a, f) => Self::pi(a.subsitute(index, replacement), f.subsitute(index, replacement)), // ????
    //         Appl(m, n) => Self::appl(m.subsitute(index, replacement),n.subsitute(index, replacement)),
    //         Ann(m, a) => Self::ann(m.subsitute(index, replacement),a.subsitute(index, replacement)),
    //         FreeVar(x) => if x == index {
    //             replacement.clone()
    //         } else if x > index {
    //             FreeVar(x-1)
    //         } else {
    //             FreeVar(x)
    //         },
    //         x => x
    //     }
    // }

    fn infer_ty(self, ctx: &mut Vec<Term>) { unimplemented!() }
    fn infer_sort(self, ctx: &mut Vec<Term>) { unimplemented!() }
    fn check_ty(self, ctx: &mut Vec<Term>) -> bool { unimplemented!() }
}

impl fmt::Display for Term {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lam(f) => write!(fmt,"(λ {})", f),
            Pi(a, f) => write!(fmt, "(Π {}.{})", a,f),
            Appl(m, n) => write!(fmt,"({} {})", m, n),
            Ann(m, a) => write!(fmt, "({} : {})", m, a),
            FreeVar(x) => write!(fmt, "{}", x),
            Star => write!(fmt, "*"),
            Bx => write!(fmt, "☐"),
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;

    #[test]
    fn eval_lambda() {
        // (λ λ 4 2 (λ 1 3)) (λ 5 1)
        // let a = "(λ ((λ ((4 2) (λ (1 3)))) (λ (5 1))))";
        let a = "((λ (λ ((4 2) (λ (1 3))))) (λ (5 1)))";
        // let a ="((λ (0 1)) (λ (0 1)))";
        // ((λ (0 1)) 0)

        // λ 3 (λ 6 1) (λ 1 (λ 7 1))
        let b = "(λ ((3 (λ (6 1))) (λ (1 (λ (7 1))))))";
        println!("{}", Term::parse(a).unwrap().eval(&vec![]));
        // println!("{}", Term::parse(b).unwrap());
    }

    #[test]
    fn parsing() {
        // λ Π ☐
        let a = "(((λ ☐) 0) : (Π 0.*))";
        let terms = Term::parse(a).unwrap();
        assert_eq!(a,format!("{}", &terms));
    }
}
// use core::fmt;
// use std::collections::VecDeque;

// use pest_derive::Parser;
// use pest::{Parser, iterators::Pair};

// use crate::CoC::Term::*;


// pub trait FnClone {
//     fn call(&self, t: Term) -> Term;
//     fn clone_box(&self) -> Box<dyn FnClone>;
// }

// impl<F> FnClone for F
// where
//     F: 'static + Clone + FnOnce(Term) -> Term,
// {
//     fn call(&self, t: Term) -> Term {
//         (self.clone())(t)
//     }

//     fn clone_box(&self) -> Box<dyn FnClone> {
//         Box::new(self.clone())
//     }
// }

// impl Clone for Box<dyn FnClone> {
//     fn clone(&self) -> Self {
//         self.clone_box()
//     }
// }

// #[derive(Parser)]
// #[grammar = "grammar.pest"]
// pub struct TermParser;

// #[derive(Clone)]
// pub enum Term {
//     Lam(Box<dyn FnClone>),
//     Pi(Box<Term>, Box<dyn FnClone>),
//     Appl(Box<Term>, Box<Term>),
//     Ann(Box<Term>, Box<Term>),
//     FreeVar(usize),
//     Star,
//     Bx,
// }

// impl Term {
//     pub fn lam<F>(f: F) -> Self
//     where F: 'static + FnClone {
//         Lam(Box::new(f))
//     }

//     pub fn pi<F>(a: Term, f: F) -> Self
//     where F: 'static + FnClone {
//         Pi(Box::new(a), Box::new(f))
//     }

//     pub fn appl(m: Term, n: Term) -> Self {
//         Appl(Box::new(m), Box::new(n))
//     }

//     pub fn ann(m: Term, a: Term) -> Self {
//         Ann(Box::new(m), Box::new(a))
//     }

//     pub fn parse_term(pair: Pair<Rule>) -> Term {
//         match pair.as_rule() {
//             Rule::lambda => todo!(),
//             Rule::pi => todo!(),
//             Rule::application => {
//                 let mut inner_rules = pair.into_inner();
//                 let m = Self::parse_term(inner_rules.next().unwrap());
//                 let n = Self::parse_term(inner_rules.next().unwrap());
//                 Self::appl(m,n)
//             },
//             Rule::ann => {
//                 let mut pair = pair.into_inner();
//                 let m = Self::parse_term(pair.next().unwrap());
//                 let a = Self::parse_term(pair.next().unwrap());
//                 Self::ann(m,a)
//             },
//             Rule::variable => {
//                 let mut pair = pair.into_inner();
//                 let x = pair.next().unwrap().as_str().parse().unwrap();
//                 FreeVar(x)
//             },
//             Rule::star => Star,
//             Rule::bx => Bx,
//             Rule::term => Self::parse_term(pair.into_inner().next().unwrap()),
//             Rule::space | Rule::digit => unreachable!(),
//         }
//     }
// }

// impl fmt::Debug for Term {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", print(0,self))
//     }
// }
// pub fn unfurl(lvl: usize, f: &Box<dyn FnClone>) -> Term {
//     let out = f.call(FreeVar(lvl));
//     out
// }

// pub fn unfurl2<'a,'b>(lvl: usize, (f,g): (&'a Box<dyn FnClone>, &'b Box<dyn FnClone>)) -> (Term,Term) {
//     (unfurl(lvl, f), unfurl(lvl, g))
// }

// pub fn print(lvl: usize, f: &Term) -> String {
//     let plunge = |f| print(lvl+1,&unfurl(lvl, f));
//     match f {
//         Lam(f) => format!("(λ {})", plunge(f)),
//         Pi(a, f) => format!("(Π {}.{})", print(lvl, a), plunge(f)),
//         Appl(m, n) => format!("({} {})", print(lvl,m), print(lvl,n)),
//         Ann(m, a) => format!("({} : {})", print(lvl,m), print(lvl,a)),
//         FreeVar(x) => format!("{}", x),
//         Star => format!("*"),
//         Bx => format!("☐"),
//     }
// }

// pub fn eval(term: Term) -> Term {
//     match term {
//         Lam(f) => Lam(Box::new(move |n| {eval(f.call(n))})),
//         Pi(a, f) => Pi(Box::new(eval(*a)), Box::new(move |n| eval(f.call(n)))),
//         Appl(m, n) => match (eval(*m), eval(*n)) {
//             (Lam(f), n) => f.call(n),
//             (m,n) => Appl(Box::new(m),Box::new(n)),
//         },
//         Ann(m, _a) => eval(*m),
//         t => t,
//     }
// }

// pub fn equate(lvl: usize, (f,g): (Term,Term)) -> bool {
//     let plunge = |f: Box<dyn FnClone>,g: Box<dyn FnClone>| equate(lvl+1, (f.call(FreeVar(lvl)), g.call(FreeVar(lvl))));
//     match (f,g) {
//         (Lam(f), Lam(g)) => plunge(f,g),
//         (Pi(a,f), Pi(b,g)) => equate(lvl, (*a,*b)) && plunge(f,g),
//         (Appl(m,n),Appl(m_,n_)) => equate(lvl, (*m,*m_)) && equate(lvl, (*n,*n_)),
//         (Ann(m,a), Ann(m_,b)) => equate(lvl, (*m,*m_)) && equate(lvl, (*a,*b)),
//         (FreeVar(x), FreeVar(y)) => x == y,
//         (Star,Star) | (Bx, Bx)=> true,
//         (_,_) => false,
//     }
// }

// pub fn panic(lvl: usize, t: &Term, fmt: String) -> ! {
//     panic!("{}: {}", fmt, print(lvl, t));
// }

// pub fn infer_ty(lvl: usize, ctx: &VecDeque<Term>, term: &Term) -> Term {
//     match term {
//         Pi(a,f) => {
//             let _s1 = infer_sort(lvl, ctx, a);
//             let mut new_ctx = ctx.to_owned();
//             new_ctx.push_front(eval(*a.clone()));
//             let s2 = infer_sort(lvl+1, &new_ctx, &unfurl(lvl, f));
//             s2
//         },
//         Appl(m,n) => match infer_ty(lvl, ctx, m) {
//             Pi(a,f) => {
//                 let _ = check_ty(lvl, ctx, n,&a);
//                 f.call(*n.clone())
//             },
//             x => panic(lvl, &m, format!("Want a Pi type, got {}", print(lvl, &x)))
//         },
//         Ann(m,a) => {
//             let _s = infer_sort(lvl, ctx, a);
//             check_ty(lvl, ctx, &m, &eval(*a.clone()))
//         },
//         FreeVar(x) => ctx[lvl-x-1].clone(),
//         Star => Bx,
//         Bx => panic(lvl,&Bx,"Has no type".to_string()),
//         t => panic(lvl, t, "Not inferrable".to_string()),
//     }
// }

// pub fn infer_sort(lvl: usize, ctx: &VecDeque<Term>, a: &Term) -> Term {
//     let b = infer_ty(lvl, ctx, a);
//     match b {
//         Bx => Bx,
//         Star => Star,
//         ty => panic(lvl,&a,format!("Want a sort got {}", print(lvl, &ty))),
//     }
// }

// pub fn check_ty(lvl: usize, ctx: &VecDeque<Term>, t: &Term, ty: &Term) -> Term {
//     match (t,ty) {
//         (Lam(f), Pi(a,g)) => {
//             let mut new_ctx = ctx.to_owned();
//             new_ctx.push_front(*a.clone());
//             let (x,y) = unfurl2(lvl, (f,g));
//             let _ = check_ty(lvl+1, &new_ctx, &x,&y);
//             Pi(a.clone(),g.clone())
//         },
//         (lamda @ Lam(_), ty) => panic(lvl, lamda, format!("Want a Pi type, got {}", print(lvl,ty))),
//         (t,ty) => {
//             let got_ty = infer_ty(lvl, ctx, t);
//             if equate(lvl, (ty.clone(),got_ty.clone())) {
//                 ty.clone()
//             } else {
//                 panic(lvl, t, format!("Watn type {}, got {}", print(lvl,ty), print(lvl,&got_ty)))
//             }
//         }
//     }
// }

// pub fn assert_infer(ctx: Vec<Term>, t: Term, expected_ty: Term) {
//     let lvl = ctx.len();
//     let infered_ty = infer_ty(lvl, &ctx.into(), &t);
//     assert!(equate(lvl, (infered_ty, expected_ty)))
// }

// pub fn assert_beta_eq(m: Term, n: Term) {
//     assert!(equate(0, (eval(m), eval(n))));
// }
// #[cfg(test)]
// mod tests {
//     use super::*;
//     #[test]
//     fn test_print() {
//         assert_eq!("42", format!("{:?}", FreeVar(42)));
//         assert_eq!("*", format!("{:?}", Star));
//         assert_eq!("☐", format!("{:?}", Bx));

//         assert_eq!("(λ 3)", print(3, &Lam(Box::new(move |x| x))));
//         assert_eq!("(λ (3 42))", print(3, &Lam(Box::new(move |x| Appl(Box::new(x), Box::new(FreeVar(42)))))));


//     }

//     #[test]
//     fn test_infer_var() {
//         assert_infer(vec![Star], FreeVar(0), Star);
//         assert_infer(vec![Bx, Star], FreeVar(0), Star);
//         assert_infer(vec![Bx,Bx, Star], FreeVar(0), Star);
//         assert_infer(vec![Star, Bx], FreeVar(1), Star);
//         assert_infer(vec![Bx, Star, Bx], FreeVar(1), Star);
//         assert_infer(vec![Bx, Star, Bx, Bx], FreeVar(2), Star);
//         assert_infer(vec![Star, Bx, Bx], FreeVar(2), Star);
//     }

//     #[test]
//     fn test_infer_star_box() {
//         assert_infer(vec![], Star, Bx);
//         assert_infer(vec![Bx, Bx, Bx], Star, Bx);
//     }

//     #[test]
//     #[should_panic]
//     fn test_infer_box() {
//         assert_infer(vec![], Bx, Bx);
//     }

//     #[test]
//     fn test_infer_pi() {
//         // term dep on term
//         assert_infer(vec![
//             Pi(Box::new(FreeVar(0)), Box::new(|_| Star)),
//             Star], 
//             Pi(Box::new(FreeVar(0)), Box::new(move |x| Appl(Box::new(FreeVar(1)),Box::new(x)))),
//             Star);
//         // term dep on type
//         assert_infer(vec![Pi(Box::new(Star), Box::new(|_| Star))], 
//             Pi(Box::new(Star), Box::new(move |x| Appl(Box::new(FreeVar(0)),Box::new(x)))),
//             Star);
//         // type dep on term
//         assert_infer(vec![
//             Pi(Box::new(FreeVar(0)), Box::new(|_| Bx)),
//             Star], 
//             Pi(Box::new(FreeVar(0)), Box::new(move |x| Appl(Box::new(FreeVar(1)),Box::new(x)))),
//             Bx);
//         // type dep on type
//         assert_infer(vec![ Pi(Box::new(Star), Box::new(|_| Bx)) ], 
//             Pi(Box::new(Star), Box::new(move |x| Appl(Box::new(FreeVar(0)),Box::new(x)))),
//             Bx);
//     }

//     #[test]
//     #[should_panic]
//     fn test_infer_pi_fail() {
//         assert_infer(vec![ FreeVar(0), Star],
//             Pi(Box::new(FreeVar(1)), Box::new(move |_| Star)),
//             Bx);
//     }

//     #[test]
//     #[should_panic]
//     fn test_infer_pi_fail2() {
//         assert_infer(vec![ FreeVar(0), Star],
//             Pi(Box::new(Star), Box::new(move |_| FreeVar(1))),
//             Bx);
//     }
// }