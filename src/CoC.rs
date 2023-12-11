use core::fmt;
use std::collections::VecDeque;

use dyn_clone::DynClone;

use crate::CoC::Term::*;

pub trait FnClone: DynClone {
    fn call(&self, t: Term) -> Term;
}

impl<F> FnClone for F
    where F: Fn(Term) -> Term + Clone 
{
    fn call(&self, t: Term) -> Term {
        self(t)
    }
}

dyn_clone::clone_trait_object!(FnClone);

#[derive(Clone)]
pub enum Term {
    Lam(Box<dyn FnClone>),
    Pi(Box<Term>, Box<dyn FnClone>),
    Appl(Box<Term>, Box<Term>),
    Ann(Box<Term>, Box<Term>),
    FreeVar(usize),
    Star,
    Bx,
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", print(0,self))
    }
}
pub fn unfurl(lvl: usize, f: &Box<dyn FnClone>) -> Term {
    let out = f.call(FreeVar(lvl));
    out
}

pub fn unfurl2<'a,'b>(lvl: usize, (f,g): (&'a Box<dyn FnClone>, &'b Box<dyn FnClone>)) -> (Term,Term) {
    (unfurl(lvl, f), unfurl(lvl, g))
}

pub fn print(lvl: usize, f: &Term) -> String {
    let plunge = |f| print(lvl+1,&unfurl(lvl, f));
    match f {
        Lam(f) => format!("(λ {})", plunge(f)),
        Pi(a, f) => format!("(Π {}.{})", print(lvl, a), plunge(f)),
        Appl(m, n) => format!("({} {})", print(lvl,m), print(lvl,n)),
        Ann(m, a) => format!("({} : {})", print(lvl,m), print(lvl,a)),
        FreeVar(x) => format!("{}", x),
        Star => format!("*"),
        Bx => format!("☐"),
    }
}

pub fn eval(term: Term) -> Term {
    match term {
        Lam(f) => Lam(Box::new(move |n| {eval(f.call(n))})),
        Pi(a, f) => Pi(Box::new(eval(*a)), Box::new(move |n| eval(f.call(n)))),
        Appl(m, n) => match (eval(*m), eval(*n)) {
            (Lam(f), n) => f.call(n),
            (m,n) => Appl(Box::new(m),Box::new(n)),
        },
        Ann(m, _a) => eval(*m),
        t => t,
    }
}

pub fn equate(lvl: usize, (f,g): (Term,Term)) -> bool {
    let plunge = |f: Box<dyn FnClone>,g: Box<dyn FnClone>| equate(lvl+1, (f.call(FreeVar(lvl)), g.call(FreeVar(lvl))));
    match (f,g) {
        (Lam(f), Lam(g)) => plunge(f,g),
        (Pi(a,f), Pi(b,g)) => equate(lvl, (*a,*b)) && plunge(f,g),
        (Appl(m,n),Appl(m_,n_)) => equate(lvl, (*m,*m_)) && equate(lvl, (*n,*n_)),
        (Ann(m,a), Ann(m_,b)) => equate(lvl, (*m,*m_)) && equate(lvl, (*a,*b)),
        (FreeVar(x), FreeVar(y)) => x == y,
        (Star,Star) | (Bx, Bx)=> true,
        (_,_) => false,
    }
}

pub fn panic(lvl: usize, t: &Term, fmt: String) -> ! {
    panic!("{}: {}", fmt, print(lvl, t));
}

pub fn infer_ty(lvl: usize, ctx: &VecDeque<Term>, term: &Term) -> Term {
    match term {
        Pi(a,f) => {
            let _s1 = infer_sort(lvl, ctx, a);
            let mut new_ctx = ctx.to_owned();
            new_ctx.push_front(eval(*a.clone()));
            let s2 = infer_sort(lvl+1, &new_ctx, &unfurl(lvl, f));
            s2
        },
        Appl(m,n) => match infer_ty(lvl, ctx, m) {
            Pi(a,f) => {
                let _ = check_ty(lvl, ctx, n,&a);
                f.call(*n.clone())
            },
            x => panic(lvl, &m, format!("Want a Pi type, got {}", print(lvl, &x)))
        },
        Ann(m,a) => {
            let _s = infer_sort(lvl, ctx, a);
            check_ty(lvl, ctx, &m, &eval(*a.clone()))
        },
        FreeVar(x) => ctx[lvl-x-1].clone(),
        Star => Bx,
        Bx => panic(lvl,&Bx,"Has no type".to_string()),
        t => panic(lvl, t, "Not inferrable".to_string()),
    }
}

pub fn infer_sort(lvl: usize, ctx: &VecDeque<Term>, a: &Term) -> Term {
    let b = infer_ty(lvl, ctx, a);
    match b {
        Bx => Bx,
        Star => Star,
        ty => panic(lvl,&a,format!("Want a sort got {}", print(lvl, &ty))),
    }
}

pub fn check_ty(lvl: usize, ctx: &VecDeque<Term>, t: &Term, ty: &Term) -> Term {
    match (t,ty) {
        (Lam(f), Pi(a,g)) => {
            let mut new_ctx = ctx.to_owned();
            new_ctx.push_front(*a.clone());
            let (x,y) = unfurl2(lvl, (f,g));
            let _ = check_ty(lvl+1, &new_ctx, &x,&y);
            Pi(a.clone(),g.clone())
        },
        (lamda @ Lam(_), ty) => panic(lvl, lamda, format!("Want a Pi type, got {}", print(lvl,ty))),
        (t,ty) => {
            let got_ty = infer_ty(lvl, ctx, t);
            if equate(lvl, (ty.clone(),got_ty.clone())) {
                ty.clone()
            } else {
                panic(lvl, t, format!("Watn type {}, got {}", print(lvl,ty), print(lvl,&got_ty)))
            }
        }
    }
}

pub fn assert_infer(ctx: Vec<Term>, t: Term, expected_ty: Term) {
    let lvl = ctx.len();
    let infered_ty = infer_ty(lvl, &ctx.into(), &t);
    assert!(equate(lvl, (infered_ty, expected_ty)))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_print() {
        assert_eq!("42", format!("{:?}", FreeVar(42)));
        assert_eq!("*", format!("{:?}", Star));
        assert_eq!("☐", format!("{:?}", Bx));

        assert_eq!("(λ 3)", print(3, &Lam(Box::new(move |x| x))));
        assert_eq!("(λ (3 42))", print(3, &Lam(Box::new(move |x| Appl(Box::new(x), Box::new(FreeVar(42)))))));


    }

    #[test]
    fn test_infer_var() {
        assert_infer(vec![Star], FreeVar(0), Star);
        assert_infer(vec![Bx, Star], FreeVar(0), Star);
        assert_infer(vec![Bx,Bx, Star], FreeVar(0), Star);
        assert_infer(vec![Star, Bx], FreeVar(1), Star);
        assert_infer(vec![Bx, Star, Bx], FreeVar(1), Star);
        assert_infer(vec![Bx, Star, Bx, Bx], FreeVar(2), Star);
        assert_infer(vec![Star, Bx, Bx], FreeVar(2), Star);
    }

    #[test]
    fn test_infer_star_box() {
        assert_infer(vec![], Star, Bx);
        assert_infer(vec![Bx, Bx, Bx], Star, Bx);
    }

    #[test]
    #[should_panic]
    fn test_infer_box() {
        assert_infer(vec![], Bx, Bx);
    }

    #[test]
    fn test_infer_pi() {
        // term dep on term
        assert_infer(vec![
            Pi(Box::new(FreeVar(0)), Box::new(|_| Star)),
            Star], 
            Pi(Box::new(FreeVar(0)), Box::new(move |x| Appl(Box::new(FreeVar(1)),Box::new(x)))),
            Star);
        // term dep on type
        assert_infer(vec![Pi(Box::new(Star), Box::new(|_| Star))], 
            Pi(Box::new(Star), Box::new(move |x| Appl(Box::new(FreeVar(0)),Box::new(x)))),
            Star);
        // type dep on term
        assert_infer(vec![
            Pi(Box::new(FreeVar(0)), Box::new(|_| Bx)),
            Star], 
            Pi(Box::new(FreeVar(0)), Box::new(move |x| Appl(Box::new(FreeVar(1)),Box::new(x)))),
            Bx);
        // type dep on type
        assert_infer(vec![ Pi(Box::new(Star), Box::new(|_| Bx)) ], 
            Pi(Box::new(Star), Box::new(move |x| Appl(Box::new(FreeVar(0)),Box::new(x)))),
            Bx);
    }

    #[test]
    #[should_panic]
    fn test_infer_pi_fail() {
        assert_infer(vec![ FreeVar(0), Star],
            Pi(Box::new(FreeVar(1)), Box::new(move |_| Star)),
            Bx);
    }

    #[test]
    #[should_panic]
    fn test_infer_pi_fail2() {
        assert_infer(vec![ FreeVar(0), Star],
            Pi(Box::new(Star), Box::new(move |_| FreeVar(1))),
            Bx);
    }
}