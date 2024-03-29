use std::collections::HashMap;

use pest::Parser;
use pest_derive::Parser;

use Term::*;


#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TermParser;

#[derive(Debug, Clone, Eq)]
pub enum Term { 
    Star,
    Bx,
    V(String), // constants and variables of all varieties: term variables, type variables, kinds variables, and beyond.
    Lam(String, Box<Term>, Box<Term>), // abstractions, again for all sorts: terms, types, and so on, which can map to terms, types, and so on
    Pi(String, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>), // applications of all sorts
    // Ann(Box<Term>, Box<Term>), // type annotation
}

impl Term {
    pub fn v(s: &str) -> Term { Self::V(s.to_string()) }
    pub fn lam(s: &str, a: Term, m: Term) -> Term { Self::Lam(s.to_string(), Box::new(a), Box::new(m)) }
    pub fn pi(s: &str, a: Term, m: Term) -> Term { Self::Pi(s.to_string(), Box::new(a), Box::new(m)) }
    pub fn app(a: Term, b: Term) -> Term { Self::App(Box::new(a), Box::new(b)) }
    // pub fn ann(a: Term, b: Term) -> Term { Self::Ann(Box::new(a), Box::new(b)) }

    pub fn parse(term: &str) -> Result<Term, pest::error::Error<Rule>> {
        let pair = TermParser::parse(Rule::term, term)?.next().unwrap();
        Self::parse_term(pair)
    }

    pub fn parse_term(pair: pest::iterators::Pair<Rule>) -> Result<Term, pest::error::Error<Rule>> {
        match pair.as_rule() {
            Rule::term => Self::parse_term(pair.into_inner().next().unwrap()),
            Rule::lambda => {
                let mut pair = pair.into_inner();
                let v = pair.next().unwrap().as_str();
                let a = Self::parse_term(pair.next().unwrap())?;
                let m = Self::parse_term(pair.next().unwrap())?;
                Ok(Self::lam(v,a,m))
            },
            Rule::pi => {
                let mut pair = pair.into_inner();
                let v = pair.next().unwrap().as_str();
                let a = Self::parse_term(pair.next().unwrap())?;
                let m = Self::parse_term(pair.next().unwrap())?;
                Ok(Self::pi(v,a,m))
            },
            Rule::application => {
                let mut pair = pair.into_inner();
                let a = Self::parse_term(pair.next().unwrap())?;
                let b = Self::parse_term(pair.nth(1).unwrap())?;
                Ok(Self::app(a,b))
            },
            Rule::variable => {
                Ok(Self::v(pair.as_str()))
            },
            Rule::star => Ok(Self::Star),
            Rule::bx => Ok(Self::Bx),
            Rule::space | Rule::digit => unreachable!(),
        }
    }

    pub fn free_vars(&self) -> Vec<String> {
        match self {
            Star | Bx => vec![],
            V(s) => vec![ s.clone() ],
            Lam(s, a,m) => [a.free_vars(),m.free_vars().into_iter().filter(|x| x != s).collect()].concat(),
            Pi(s, a,m) => [a.free_vars(),m.free_vars().into_iter().filter(|x| x != s).collect()].concat(),
            App(m, n) => [m.free_vars(), n.free_vars()].concat(),
        }
    }

    pub fn binders(&self) -> Vec<String> {
        match self {
            V(_) | Star | Bx => vec![],
            Lam(s, a, m) => [vec![s.clone()],a.binders(),m.binders()].concat(),
            Pi(s, a, m) => [vec![s.clone()],a.binders(),m.binders()].concat(),
            App(m, n) => [m.binders(),n.binders()].concat(),
        }
    }

    pub fn all_vars(&self) -> Vec<String> {
        [self.free_vars(), self.binders()].concat()
    }

    pub fn subst(self, x: &str, m: &Term) -> Term{
        match self {
            Star => Star,
            Bx => Bx,
            V(y) if x == y => m.clone(),
            V(y) => V(y),
            Lam(y, a,n) if x == y => Self::lam(&y,*a,*n),
            Lam(y, a,n) if m.free_vars().contains(&y) => {
                let banlist = m.free_vars();
                Self::lam(&freshen(&banlist,&y),a.subst(x, &Self::v(&y)), n.subst(x, &Self::v(&y)))
            },
            Lam(y,a,n) => Self::lam(&y, a.subst(x, m), n.subst(x, m)),
            Pi(y, a,n) if x == y => Self::lam(&y,*a,*n),
            Pi(y, a,n) if m.free_vars().contains(&y) => {
                let banlist = m.free_vars();
                Self::lam(&freshen(&banlist,&y),a.subst(x, &Self::v(&y)), n.subst(x, &Self::v(&y)))
            },
            Pi(y,a,n) => Self::lam(&y, a.subst(x, m), n.subst(x, m)),
            App(m_, n) => Self::app(m_.subst(x, m),n.subst(x, m)),
        }
    }

    pub fn eval(&self) -> Term {
        match self {
           App(m,n) => match (m.eval(), n.eval()) {
                (Lam(x,a,m), n) => m.subst(&x, &n).eval(),
                (m,n) => Self::app(m,n),
           },
           Lam(x,a,m) => Self::lam(x,a.eval(),m.eval()),
           Pi(x,a,m) => Self::pi(x,a.eval(),m.eval()),
           V(s) => Self::v(s),
           Star => Star,
           Bx => Bx,
        }
    }

    pub fn infer_type(&self, ctx: &HashMap<&str, Term>) -> Term {
        match self {
            Star => Bx,
            Bx => panic!("☐ has no type!"),
            V(x) => ctx.get(x.as_str()).and_then(|x| Some(x.eval())).unwrap(),
            Lam(x, a, m) => {
                let mut new_ctx = ctx.clone();
                new_ctx.insert(&x, *a.clone());
                let m_ty = m.infer_type(&new_ctx);
                let lam_ty = Self::pi(&x,*a.clone(),m_ty);
                let _ = &lam_ty.infer_type(ctx);
                lam_ty.eval()
            },
            Pi(x, a, m) => {
                let _s1 = a.infer_sort(ctx);
                let mut new_ctx = ctx.clone();
                new_ctx.insert(&x, *a.clone());
                let s2 = m.infer_type(&new_ctx);
                s2
            },
            App(m,n) => Self::infer_app(ctx, &m,&n),
        }
    }

    pub fn infer_sort(&self, ctx: &HashMap<&str, Term>) -> Term {
        match self.infer_type(ctx) {
            Star => Star,
            Bx => Bx,
            a => panic!("Expected sort, got {a:?}: {self:?}")
        }
    }

    pub fn infer_app(ctx: &HashMap<&str, Term>, m: &Term, n: &Term) -> Term {
        match m.infer_type(ctx) {
            Pi(x,a,m) => {
                let n_ty = n.infer_type(ctx);
                if *a == n_ty {
                    m.subst(&x, n).eval()
                } else {
                    panic!("Expected type {a:?}, got {n_ty:?}: {n:?}")
                }
            },
            m_ty => {
                panic!("Application of argument {n:?} to a non-lambda {m:?} of type {m_ty:?}")
            }
        }
    }
}

impl PartialEq for Term {
    // alpha equality
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (V(x), V(x_)) => x == x_,
            (Lam(x,a,m),Lam(x_,a_,m_)) if x == x_ => a == a_ && m == m_,
            (Lam(x,a,m),Lam(x_,a_,m_)) => {
                let mut banlist = m.all_vars();
                banlist.append(&mut m_.all_vars());
                let banlist = vec![];
                let fresh_x = freshen(&banlist, x);
                Self::lam(&fresh_x, *a.clone(), m.clone().subst(x, &Self::v(&fresh_x))) == Self::lam(&fresh_x,*a_.clone(), m_.clone().subst(x_, &Self::v(&fresh_x)))
            },
            (Pi(x,a,m),Pi(x_,a_,m_)) if x == x_ => a == a_ && m == m_,
            (Pi(x,a,m),Pi(x_,a_,m_)) => {
                let mut banlist = m.all_vars();
                banlist.append(&mut m_.all_vars());
                let banlist = vec![];
                let fresh_x = freshen(&banlist, x);
                Self::lam(&fresh_x, *a.clone(), m.clone().subst(x, &Self::v(&fresh_x))) == Self::lam(&fresh_x,*a_.clone(), m_.clone().subst(x_, &Self::v(&fresh_x)))
            },
            (App(m,n),App(m_,n_)) => m == m_ && n == n_,
            (Star,Star) | (Bx,Bx) => true,
            (_,_) => false
        }
    }
}

fn freshen(banlist: &Vec<String>, base: &String) -> String{
    if banlist.contains(base) {
        freshen(banlist, &format!("{base}'"))
    } else {
        base.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> Term {
        Term::parse(s).unwrap()
    }

    #[test]
    fn parsing() {
        assert_eq!(Term::parse("^x:*.x").unwrap(), Term::parse("λx:*.x").unwrap());
        assert_eq!(Term::lam("x", Star, Term::lam("y", Star, Term::v("y"))), Term::parse("^x:*.^y:*.y").unwrap());
    }

    #[test] 
    fn free_vars() {
        assert_eq!(Term::v("x").subst("x", &Term::v("y")), Term::v("y"));
    }

    #[test]
    fn subst() {
        let x = vec![
            ("x","y","x","y"),
            ("y","z","x","x"),
            ("x","z", "(x y)", "(z y)"),
            ("y","z", "(x y)", "(x z)"),
            ("z","l", "(x y)", "(x y)"),
            ("x", "z", "^x:*.x", "^x:*.x"),
            ("y", "z", "^x:*.y", "^x:*.z"),
            ("y", "x", "^x:*.y", "^x':*.x"),
        ];
        for (x,m,term,out) in x {
            assert_eq!(p(term).subst(x, &p(m)), p(out));
        }
    }

    #[test]
    fn alpha_eq() {
        assert!(p("^x:*.x") == p("^y:*.y"));
        assert!(p("^x:*.^y:*.x") == p("^y:*.^x:*.y"));
        assert!(p("^x:*.^y:*.x") == p("^y:*.^x:*.x"));
    }

    #[test]
    fn eval() {
        assert_eq!(p("x").eval(), p("x"));
        assert_eq!(p("(x y)").eval(), p("(x y)"));
        assert_eq!(p("(^x:*.x y)").eval(), p("y"));
        assert_eq!(p("((^x:*.^x:*.x z) y)").eval(), p("y"));

        assert_eq!(p("(x (^x:*.x y))").eval(), p("(x y)"));
        assert_eq!(p("(^x:*.x (^x:*.x z))").eval(), p("z"));
        assert_eq!(p("(^x:*.(^y:*.y x) z)").eval(), p("z"));
        assert_eq!(p("^x:*.x").eval(), p("^x:*.x"));
        assert_eq!(p("^x:*.(^x:*.x z)").eval(), p("^x:*.z"));

        assert_eq!(p("(^x:a.x y)").eval(),p("y"));
    }

    #[test]
    fn type_inference() {
        let x = vec![
            // super type
            (vec![], Star, Bx),
            (vec![("x", Star), ("y", p("x"))], Star, Bx),
            // vars
            (vec![("x", Star)], p("x"), Star),
            (vec![("x", Star), ("y", p("x")), ("z", p("x"))], p("y"), p("x")),
            (vec![("x", Star), ("y", p("x"))], p("y"), p("x")),
            // application
            (vec![("a", Star), ("y", p("a"))], p("(^x:a.x y)"), p("a")),
            (vec![("y", Star)], p("(^x:*.x y)"), Star),
            // term dep term
            (vec![("a", Star)], p("^x:a.x"), p("Πx:a.a")),
            // term dep type
            (vec![("a", Star), ("y", p("a"))], p("^x:*.y"), p("Πx:*.a")),
            // type dep type
            (vec![], p("^x:*.x"), p("Πx:*.*")),
            // type dep term
            (vec![("a", Star)], p("^x:a.a"), p("Πx:a.*")),
            (vec![("a", Star)], p("^x:a.Πy:a.a"), p("Πx:a.*")),
            // pi types
            (vec![("a", Star)], p("Πx:a.a"), p("*")),
            (vec![("a", Star)], p("Πx:a.Πx:a.a"), Star),
            (vec![("a", Bx)], p("Πx:a.a"), Bx),
            (vec![("a", Bx)], p("Πx:a.Πx:a.a"), Bx),
            // norm subexpr
            (vec![("a", Star), ("b", Star), ("y", p("a")), ("z", p("b"))], p("^x:(^x:*.x a).(^x:b.x z)"), p("Πx:a.b"))

        ];
        for (ctx,term, ty) in x {
            assert_eq!(term.infer_type(&ctx.into_iter().collect()), ty);
        }
    }

    #[test]
    fn useage() {
        let t = p("^a:*.^x:a.^y:a.x");
        let f = p("^a:*.^x:a.^y:a.y");
        let bool_ty = p("Πa:*.Πx:a.Πy:a.a");
        assert!(t.infer_type(&HashMap::new()) == bool_ty);
    }
}