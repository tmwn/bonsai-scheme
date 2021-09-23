use std::{cell, collections, env, error::Error, fs, rc::Rc};
type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)? + " $";
    eval_list(&default_env(), vec(&Parser::new(code).list()))?;
    Ok(())
}
struct Parser {
    tokens: collections::VecDeque<String>,
}
impl Parser {
    fn new(mut code: String) -> Self {
        let cs = "()'".chars();
        cs.for_each(|c| code = code.replace(c, &format!(" {} ", c)));
        let tokens = code.split_whitespace().map(ToOwned::to_owned).collect();
        Self { tokens }
    }
    fn list(&mut self) -> V {
        match self.tokens.pop_front().expect("empty") {
            x if &x == ")" || &x == "$" => Nil().into(),
            x => Pair(self.value(Some(x)).into(), self.list().into()).into(),
        }
    }
    fn value(&mut self, first: Option<String>) -> V {
        match &*first.unwrap_or_else(|| self.tokens.pop_front().expect("empty")) {
            "(" => self.list(),
            "#t" => Bool(true).into(),
            "#f" => Bool(false).into(),
            "'" => Quote(self.value(None)).into(),
            x => match x.parse::<i64>() {
                Ok(i) => Int(i).into(),
                _ => Symbol(x.into()).into(),
            },
        }
    }
}
enum Val {
    Nil(),
    Bool(bool),
    Int(i64),
    Pair(cell::RefCell<V>, cell::RefCell<V>),
    Symbol(String),
    Quote(V),
    Func(Box<dyn Fn(&Rc<Env>, V) -> Result<V>>),
}
type V = Rc<Val>;
use Val::*;
impl Val {
    fn pair(&self) -> Result<(V, V)> {
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
fn to_list(v: &[V]) -> V {
    if let Some(x) = v.last() {
        return Pair(x.clone().into(), to_list(&v[0..v.len() - 1]).into()).into();
    }
    Nil().into()
}
fn vec(v: &V) -> Vec<V> {
    match v.pair() {
        Ok((x, y)) => [vec(&y), vec![x]].concat(),
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
fn eval(e: &Rc<Env>, v: &V) -> Result<V> {
    Ok(match v.as_ref() {
        Nil() | Bool(_) | Int(_) => v.clone(),
        Pair(x, y) => match eval(e, &x.borrow())?.as_ref() {
            Func(f) => f(e, y.borrow().clone())?,
            x => return Err(format!("not func: {}", x).into()),
        },
        Symbol(x) => e.lookup(x)?,
        Quote(x) => x.clone(),
        _ => return Err(format!("{}", v).into()),
    })
}
fn eval_list(e: &Rc<Env>, v: Vec<V>) -> Result<V> {
    let res = v.into_iter().rev().map(|x| eval(e, &x)).last();
    res.unwrap_or(Err("empty".into()))
}
struct Env {
    m: cell::RefCell<collections::HashMap<String, V>>,
    next: Option<Rc<Env>>,
}
impl Env {
    fn lookup(&self, s: &str) -> Result<V> {
        match (self.m.borrow().get(s), self.next.as_ref()) {
            (Some(v), _) => Ok(v.clone()),
            (_, Some(e)) => e.lookup(s),
            _ => Err(format!("not found: {}", s).into()),
        }
    }
    fn ensure<T: Into<V>>(self: &Rc<Self>, s: &str, v: T) -> Rc<Self> {
        self.m.borrow_mut().insert(s.to_string(), v.into());
        self.clone()
    }
    fn with_fold(self: Rc<Self>, s: &str, f: fn(V, V) -> Result<V>) -> Rc<Self> {
        let g = move |e: &Rc<Env>, v| {
            let l = vec(&v).into_iter().map(|v| eval(e, &v)).into_iter();
            l.reduce(|x, y| f(x?, y?)).unwrap_or(Err("empty".into()))
        };
        self.ensure(s, Func(Box::new(g)))
    }
    fn with_ops(self: Rc<Self>, s: &str, f: fn(Vec<V>) -> Result<V>) -> Rc<Env> {
        let g = move |e: &Rc<Env>, v| {
            let l = vec(&v).into_iter().map(|v| eval(e, &v));
            f(l.collect::<Result<_>>()?).map(Into::into)
        };
        self.ensure(s, Func(Box::new(g)))
    }
    fn with_form(self: Rc<Self>, s: &str, f: fn(&Rc<Env>, Vec<V>) -> Result<V>) -> Rc<Self> {
        self.ensure(s, Func(Box::new(move |e, v| f(e, vec(&v)))))
    }
    fn set(&self, s: &str, v: V) -> Result<()> {
        if let Some(x) = self.m.borrow_mut().get_mut(s) {
            *x = v;
            return Ok(());
        }
        self.next.as_ref().ok_or("set: not found")?.set(s, v)
    }
    fn new(next: Option<Rc<Env>>) -> Rc<Self> {
        Rc::new(Env {
            m: collections::HashMap::new().into(),
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
        .with_ops("-", |v| Ok(Int(v[1].int()? - v[0].int()?).into()))
        .with_ops("/", |v| Ok(Int(v[1].int()? / v[0].int()?).into()))
        .with_ops("=", |v| Ok(Bool(v[1].int()? == v[0].int()?).into()))
        .with_ops("<", |v| Ok(Bool(v[1].int()? < v[0].int()?).into()))
        .with_ops("<=", |v| Ok(Bool(v[1].int()? <= v[0].int()?).into()))
        .with_ops(">", |v| Ok(Bool(v[1].int()? > v[0].int()?).into()))
        .with_ops(">=", |v| Ok(Bool(v[1].int()? >= v[0].int()?).into()))
        .with_ops("eq?", |v| Ok(Bool(v[1].eq(&v[0])).into()))
        .with_ops("cons", |v| {
            Ok(Pair(v[1].clone().into(), v[0].clone().into()).into())
        })
        .with_ops("not", |v| Ok(Bool(!v[0].bool()).into()))
        .with_ops("car", |v| Ok(v[0].pair()?.0))
        .with_ops("cdr", |v| Ok(v[0].pair()?.1))
        .with_ops("print", |v| {
            println!("{}", v[0]);
            Ok(Nil().into())
        })
        .with_form("begin", eval_list)
        .with_form("define", |e, mut v| {
            let name_params = v.pop().ok_or("empty")?;
            match vec(&name_params).as_slice() {
                [params @ .., name] => {
                    let l = [v, vec![to_list(params), Symbol("lambda".into()).into()]].concat();
                    e.ensure(name.symbol()?, eval(e, &to_list(&l))?)
                }
                _ => e.ensure(name_params.symbol()?, eval(e, &v[0])?),
            };
            Ok(Nil().into())
        })
        .with_form("lambda", |e, mut v| {
            let (e, params) = (e.clone(), v.pop().ok_or("empty")?);
            Ok(Rc::new(Func(Box::new(move |e2, v2| {
                let (e, mut params, mut args) = (Env::new(Some(e.clone())), vec(&params), vec(&v2));
                while let (Some(x), Some(y)) = (params.pop(), args.pop()) {
                    e.ensure(x.symbol()?, eval(e2, &y)?);
                }
                eval_list(&e, v.clone())
            }))))
        })
        .with_form("let", |e, mut v| {
            let (e2, mut kvs) = (Env::new(Some(e.clone())), vec(&v.pop().ok_or("empty")?));
            while let Some(vk) = kvs.pop().map(|kv| (vec(&kv))) {
                e2.ensure(vk[1].symbol()?, eval(e, &vk[0])?);
            }
            eval_list(&e2, v)
        })
        .with_form("let*", |e, mut v| {
            let (mut e2, mut kvs) = (Env::new(Some(e.clone())), vec(&v.pop().ok_or("empty")?));
            while let Some(vk) = kvs.pop().map(|kv| (vec(&kv))) {
                let x = eval(&e2, &vk[0])?;
                e2 = Env::new(Some(e2)).ensure(vk[1].symbol()?, x);
            }
            eval_list(&e2, v)
        })
        .with_form("letrec", |e, mut v| {
            let (e2, mut kvs) = (Env::new(Some(e.clone())), vec(&v.pop().ok_or("empty")?));
            while let Some(vk) = kvs.pop().map(|kv| (vec(&kv))) {
                let s = vk[1].symbol()?;
                e2.ensure(s, Nil()).ensure(s, eval(&e2, &vk[0])?);
            }
            eval_list(&e2, v)
        })
        .with_form("if", |e, v| match eval(e, &v[2])?.bool() {
            true => eval(e, &v[1]),
            false => eval(e, &v[0]),
        })
        .with_form("cond", |e, mut v| loop {
            let u = vec(&v.pop().ok_or("empty")?);
            if eval(e, &u[1])?.bool() {
                return eval(e, &u[0]);
            }
        })
        .with_form("set!", |e, v| {
            e.set(v[1].symbol()?, eval(e, &v[0])?)?;
            Ok(Nil().into())
        })
        .with_form("set-car!", |e, v| {
            match eval(e, &v[1])?.as_ref() {
                Pair(x, _) => *x.borrow_mut() = eval(e, &v[0])?,
                _ => return Err("set-car: not pair".into()),
            };
            Ok(Nil().into())
        })
        .with_form("set-cdr!", |e, v| {
            match eval(e, &v[1])?.as_ref() {
                Pair(_, y) => *y.borrow_mut() = eval(e, &v[0])?,
                _ => return Err("set-cdr: not pair".into()),
            };
            Ok(Nil().into())
        })
        .ensure("else", Bool(true))
}
