use std::{cell::RefCell, collections::HashMap, env, error::Error, fs, rc::Rc};
type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)?;
    let v = Parser::new(&code).list();
    eval_list(&default_env(), to_vec(Rc::new(v)))?;
    Ok(())
}
struct Parser {
    tokens: Vec<String>,
}
impl Parser {
    fn new(code: &str) -> Self {
        Parser {
            tokens: code
                .replace("(", " ( ")
                .replace(")", " ) ")
                .replace("'", "' ")
                .split_ascii_whitespace()
                .rev()
                .filter(|x| !x.is_empty())
                .map(|x| x.to_owned())
                .collect(),
        }
    }
    fn list(&mut self) -> Val {
        if self.tokens.is_empty() || self.tokens.last().unwrap() == ")" {
            self.tokens.pop();
            return Nil();
        }
        new_pair(&self.value().into(), &self.list().into())
    }
    fn value(&mut self) -> Val {
        match self.tokens.pop().expect("EOF").as_str() {
            "(" => self.list(),
            "#t" => Bool(true),
            "#f" => Bool(false),
            "'" => Quote(self.value().into()),
            x => match x.parse::<i64>() {
                Ok(i) => Int(i),
                _ => Symbol(x.to_string()),
            },
        }
    }
}
enum Val {
    Nil(),
    Bool(bool),
    Int(i64),
    Pair(RefCell<Rc<Val>>, RefCell<Rc<Val>>),
    Symbol(String),
    Quote(Rc<Val>),
    Func(Box<dyn Fn(&Rc<Env>, Rc<Val>) -> Result<Rc<Val>>>),
}

use Val::*;
impl Val {
    fn pair(&self) -> Result<(Rc<Val>, Rc<Val>)> {
        match self {
            Pair(x, y) => Ok((x.borrow().clone(), y.borrow().clone())),
            _ => Err("pair: not pair".into()),
        }
    }
    fn int(&self) -> Result<i64> {
        match self {
            Int(x) => Ok(*x),
            _ => Err("not int".into()),
        }
    }
    fn bool(&self) -> bool {
        !self.eq(&Bool(false))
    }
    fn symbol(&self) -> Result<&str> {
        match self {
            Symbol(x) => Ok(x),
            _ => Err("not symbol".into()),
        }
    }
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Nil(), Nil()) => true,
            (Bool(x), Bool(y)) => x == y,
            (Int(x), Int(y)) => x == y,
            (Quote(x), Quote(y)) => x.eq(y),
            (Symbol(x), Symbol(y)) => x == y,
            _ => false,
        }
    }
}
fn new_pair(x: &Rc<Val>, y: &Rc<Val>) -> Val {
    Pair(RefCell::new(x.clone()), RefCell::new(y.clone()))
}
fn to_vec(v: Rc<Val>) -> Vec<Rc<Val>> {
    match v.pair() {
        Ok((x, y)) => [vec![x], to_vec(y)].concat(),
        _ => vec![],
    }
}
impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil() => write!(f, "()"),
            Bool(true) => write!(f, "#t"),
            Bool(false) => write!(f, "#f"),
            Int(i) => write!(f, "{}", i),
            Pair(x, y) => write!(f, "( {} {} )", x.borrow(), y.borrow()),
            Symbol(x) => write!(f, "{}", x),
            Quote(x) => write!(f, "'{}", x),
            Func(_) => write!(f, "<func>"),
        }
    }
}
fn eval(e: &Rc<Env>, v: Rc<Val>) -> Result<Rc<Val>> {
    Ok(match v.as_ref() {
        Nil() => Nil().into(),
        Bool(x) => Bool(*x).into(),
        Int(x) => Int(*x).into(),
        Pair(x, y) => match eval(e, x.borrow().clone())?.as_ref() {
            Func(f) => f(e, y.borrow().clone())?,
            _ => return Err("not func".into()),
        },
        Symbol(x) => e.lookup(x)?,
        Quote(x) => x.clone(),
        _ => return Err(format!("{}", v).into()),
    })
}
fn eval_list(e: &Rc<Env>, v: Vec<Rc<Val>>) -> Result<Rc<Val>> {
    v.into_iter()
        .map(|x| eval(e, x))
        .last()
        .unwrap_or(Err("empty".into()))
}
struct Env {
    m: RefCell<HashMap<String, Rc<Val>>>,
    next: Option<Rc<Env>>,
}
impl Env {
    fn lookup(&self, s: &str) -> Result<Rc<Val>> {
        match (self.m.borrow().get(s), self.next.as_ref()) {
            (Some(v), _) => Ok(Rc::clone(v)),
            (_, Some(e)) => e.lookup(s),
            _ => Err(format!("not found: {}", s).into()),
        }
    }
    fn ensure(self: &Rc<Self>, s: &str, v: Rc<Val>) -> Rc<Self> {
        self.m.borrow_mut().insert(s.to_string(), v);
        self.clone()
    }
    fn with_fold(self: Rc<Self>, s: &str, f: fn(Rc<Val>, Rc<Val>) -> Result<Rc<Val>>) -> Rc<Self> {
        let g = move |e: &Rc<Env>, v| {
            let l = to_vec(v).into_iter().map(|v| eval(e, v)).into_iter();
            l.reduce(|x, y| f(x?, y?)).unwrap_or(Err("empty".into()))
        };
        self.ensure(s, Func(Box::new(g)).into())
    }
    fn with_ops(self: Rc<Self>, s: &str, f: fn(Vec<Rc<Val>>) -> Result<Rc<Val>>) -> Rc<Self> {
        let g = move |e: &Rc<Env>, v| {
            let l = to_vec(v).into_iter().map(|v| eval(e, v));
            f(l.collect::<Result<_>>()?)
        };
        self.ensure(s, Func(Box::new(g)).into())
    }
    fn with_func(self: Rc<Self>, s: &str, f: fn(&Rc<Env>, Rc<Val>) -> Result<Rc<Val>>) -> Rc<Self> {
        self.ensure(s, Func(Box::new(f)).into())
    }
    fn set(&self, s: &str, v: Rc<Val>) -> Result<()> {
        if let Some(x) = self.m.borrow_mut().get_mut(s) {
            *x = v;
            return Ok(());
        }
        self.next.as_ref().ok_or("set: not found")?.set(s, v)
    }
    fn new(next: Option<Rc<Env>>) -> Rc<Self> {
        Rc::new(Env {
            m: HashMap::new().into(),
            next,
        })
    }
}
fn default_env() -> Rc<Env> {
    Env::new(None)
        .with_fold("+", |x, y| Ok(Int(x.int()? + y.int()?).into()))
        .with_fold("*", |x, y| Ok(Int(x.int()? * y.int()?).into()))
        .with_fold("and", |x, y| Ok(Bool(x.bool() && y.bool()).into()))
        .with_fold("or", |x, y| Ok(Bool(x.bool() || y.bool()).into()))
        .with_ops("-", |v| Ok(Int(v[0].int()? - v[1].int()?).into()))
        .with_ops("/", |v| Ok(Int(v[0].int()? / v[1].int()?).into()))
        .with_ops("=", |v| Ok(Bool(v[0].int()? == v[1].int()?).into()))
        .with_ops("<", |v| Ok(Bool(v[0].int()? < v[1].int()?).into()))
        .with_ops("<=", |v| Ok(Bool(v[0].int()? <= v[1].int()?).into()))
        .with_ops(">", |v| Ok(Bool(v[0].int()? > v[1].int()?).into()))
        .with_ops(">=", |v| Ok(Bool(v[0].int()? >= v[1].int()?).into()))
        .with_ops("eq?", |v| Ok(Bool(v[0].eq(&v[1])).into()))
        .with_ops("cons", |v| Ok(new_pair(&v[0], &v[1]).into()))
        .with_ops("not", |v| Ok(Bool(!v[0].bool()).into()))
        .with_ops("car", |v| Ok(v[0].pair()?.0))
        .with_ops("cdr", |v| Ok(v[0].pair()?.1))
        .with_ops("print", |v| {
            println!("{}", v[0]);
            Ok(Nil().into())
        })
        .with_func("begin", |e, v| eval_list(e, to_vec(v)))
        .with_func("define", |e, v| {
            match (v.pair()?.0.as_ref(), v.pair()?.1) {
                (Symbol(name), _) => e.ensure(name, eval(e, v.pair()?.1.pair()?.0)?),
                (Pair(name, args), body) => {
                    let arg_body = new_pair(&args.borrow(), &body).into();
                    let l = new_pair(&Symbol("lambda".into()).into(), &arg_body);
                    e.ensure(name.borrow().symbol()?, eval(e, Rc::new(l))?)
                }
                _ => return Err("ill-formed define".into()),
            };
            Ok(Nil().into())
        })
        .with_func("lambda", |e, v| {
            let e = e.clone();
            Ok(Rc::new(Func(Box::new(move |e2, mut v2| {
                let (e, mut args) = (Env::new(Some(e.clone())), v.pair()?.0);
                while let (Ok((x, n_args)), Ok((y, n_v2))) = (args.pair(), v2.pair()) {
                    e.ensure(x.symbol()?, eval(e2, y)?);
                    args = n_args;
                    v2 = n_v2;
                }
                eval_list(&e, to_vec(v.pair()?.1))
            }))))
        })
        .with_func("let", |e, v| {
            let (e2, mut kvs) = (Env::new(Some(e.clone())), v.pair()?.0);
            while let Ok((kv, n_kvs)) = kvs.pair() {
                e2.ensure(kv.pair()?.0.symbol()?, eval(e, kv.pair()?.1.pair()?.0)?);
                kvs = n_kvs;
            }
            eval_list(&e2, to_vec(v.pair()?.1))
        })
        .with_func("let*", |e, v| {
            let (mut e, mut kvs) = (e.clone(), v.pair()?.0);
            while let Ok((ref kv, n_kvs)) = kvs.pair() {
                let x = eval(&e, kv.pair()?.1.pair()?.0)?;
                e = Env::new(Some(e)).ensure(kv.pair()?.0.symbol()?, x);
                kvs = n_kvs;
            }
            eval_list(&e, to_vec(v.pair()?.1))
        })
        .with_func("letrec", |e, v| {
            let (e, mut kvs) = (Env::new(Some(e.clone())), v.pair()?.0);
            while let Ok((ref kv, n_kvs)) = kvs.pair() {
                e.ensure(kv.pair()?.0.symbol()?, Nil().into());
                e.ensure(kv.pair()?.0.symbol()?, eval(&e, kv.pair()?.1.pair()?.0)?);
                kvs = n_kvs;
            }
            eval_list(&e, to_vec(v.pair()?.1))
        })
        .with_func("if", |e, v| match eval(e, v.pair()?.0)?.bool() {
            true => eval(e, v.pair()?.1.pair()?.0),
            false => eval(e, v.pair()?.1.pair()?.1.pair()?.0),
        })
        .with_func("cond", |e, mut v| loop {
            if eval(e, v.pair()?.0.pair()?.0)?.bool() {
                break eval(e, v.pair()?.0.pair()?.1.pair()?.0);
            }
            v = v.pair()?.1;
        })
        .with_func("set!", |e, v| {
            e.set(v.pair()?.0.symbol()?, eval(e, v.pair()?.1.pair()?.0)?)?;
            Ok(Nil().into())
        })
        .with_func("set-car!", |e, v| {
            match eval(e, v.pair()?.0)?.as_ref() {
                Pair(x, _) => *x.borrow_mut() = eval(e, v.pair()?.1.pair()?.0)?,
                _ => return Err("set-car: not pair".into()),
            };
            Ok(Nil().into())
        })
        .with_func("set-cdr!", |e, v| {
            match eval(e, v.pair()?.0)?.as_ref() {
                Pair(_, x) => *x.borrow_mut() = eval(e, v.pair()?.1.pair()?.0)?,
                _ => return Err("set-cdr: not pair".into()),
            };
            Ok(Nil().into())
        })
        .ensure("else", Bool(true).into())
}
