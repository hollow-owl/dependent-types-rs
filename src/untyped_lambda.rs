use pest::Parser;
use pest_derive::Parser;

use Term::*;


#[derive(Parser)]
#[grammar = "grammar.pest"]
struct TermParser;

#[derive(Debug, Clone, Eq)]
pub enum Term { 
    // S(String),
    V(String), // constants and variables of all varieties: term variables, type variables, kinds variables, and beyond.
    Lam(String, Box<Term>), // abstractions, again for all sorts: terms, types, and so on, which can map to terms, types, and so on
    // Pi(String, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>), // applications of all sorts
    // Ann(Box<Term>, Box<Term>), // type annotation
}

impl Term {
    pub fn v(s: &str) -> Term { Self::V(s.to_string()) }
    pub fn lam(s: &str, ks: Term) -> Term { Self::Lam(s.to_string(), Box::new(ks)) }
    // pub fn pi(s: String, ks: Term, kt: Term) -> Term { Self::Pi(s, Box::new(ks), Box::new(kt)) }
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
                let ks = Self::parse_term(pair.next().unwrap())?;
                Ok(Self::lam(v,ks))
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
            Rule::space | Rule::digit => unreachable!(),
        }
    }

    pub fn free_vars(&self) -> Vec<String> {
        match self {
            V(s) => vec![ s.clone() ],
            Lam(s, f) => f.free_vars().into_iter().filter(|x| x != s).collect(),
            App(m, n) => [m.free_vars(), n.free_vars()].concat(),
        }
    }

    pub fn binders(&self) -> Vec<String> {
        match self {
            V(_) => vec![],
            Lam(s, f) => [vec![s.clone()],f.binders()].concat(),
            App(m, n) => [m.binders(),n.binders()].concat(),
        }
    }

    pub fn all_vars(&self) -> Vec<String> {
        [self.free_vars(), self.binders()].concat()
    }

    pub fn subst(self, x: &str, m: &Term) -> Term{
        match self {
            V(y) if x == y => m.clone(),
            V(y) => V(y),
            Lam(y, a) if x == y => Self::lam(&y,*a),
            Lam(y, a) if m.free_vars().contains(&y) => {
                let banlist = m.free_vars();
                Self::lam(&freshen(&banlist,&y),a.subst(x, &V(y)))
            },
            Lam(y,a) => Self::lam(&y, a.subst(x, m)),
            App(m_, n) => Self::app(m_.subst(x, m),n.subst(x, m)),
        }
    }

    pub fn eval(&self) -> Term {
        match self {
           App(m,n) => match (m.eval(), n.eval()) {
                (Lam(x,m), n) => m.subst(&x, &n).eval(),
                (m,n) => Self::app(m,n),
           },
           Lam(x,f) => Self::lam(x,f.eval()),
           V(s) => Self::v(s),
        }
    }
}

impl PartialEq for Term {
    // alpha equality
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (V(x), V(x_)) => x == x_,
            (Lam(x,a),Lam(x_,a_)) if x == x_ => a == a_,
            (Lam(x,a),Lam(x_,a_)) => {
                let mut banlist = a.all_vars();
                banlist.append(&mut a_.all_vars());
                let banlist = vec![];
                let fresh_x = freshen(&banlist, x);
                Self::lam(&fresh_x, a.clone().subst(x, &Self::v(&fresh_x))) == Self::lam(&fresh_x,a_.clone().subst(x_, &Self::v(&fresh_x)))
            },
            (App(m,n),App(m_,n_)) => m == m_ && n == n_,
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
        assert_eq!(Term::parse("^x.x").unwrap(), Term::parse("λx.x").unwrap());
        assert_eq!(Term::lam("x", Term::lam("y", Term::v("x"))), Term::parse("^x.^y.y").unwrap());
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
            ("x", "z", "^x.x", "^x.x"),
            ("y", "z", "^x.y", "^x.z"),
            // ("y", "x", "^x.y", "^x'.x"),
        ];
        for (x,m,term,out) in x {
            assert_eq!(p(term).subst(x, &p(m)), p(out));
        }
    }

    #[test]
    fn alpha_eq() {
        assert!(p("^x.x").alpha_eq(&p("^y.y")));
        assert!(p("^x.^y.x").alpha_eq(&p("^y.^x.y")));
        assert!(p("^x.^y.x").alpha_eq(&p("^y.^x.x")));
    }

    #[test]
    fn eval() {
        assert_eq!(p("x").eval(), p("x"));
        assert_eq!(p("(x y)").eval(), p("(x y)"));
        assert_eq!(p("(^x.x y)").eval(), p("y"));
        assert_eq!(p("((^x.^x.x z) y)").eval(), p("y"));

        assert_eq!(p("(x (^x.x y))").eval(), p("(x y)"));
        assert_eq!(p("(^x.x (^x.x z))").eval(), p("z"));
        assert_eq!(p("(^x.(^y.y x) z)").eval(), p("z"));
        assert_eq!(p("^x.x").eval(), p("^x.x"));
        assert_eq!(p("^x.(^x.x z)").eval(), p("^x.z"));
    }
}