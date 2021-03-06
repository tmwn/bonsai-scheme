use std::{cell, collections, collections::VecDeque, env, error::Error, fs, rc::Rc};
type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)? + " $";
    eval_list(&default_env(), vec(&list(&mut tokenize(code))))?;
    Ok(())
}
fn tokenize(mut code: String) -> VecDeque<String> {
    let cs = "()'".chars();
    cs.for_each(|c| code = code.replace(c, &format!(" {} ", c)));
    code.split_whitespace().map(ToOwned::to_owned).collect()
}
fn list(tokens: &mut VecDeque<String>) -> V {
    match tokens.pop_front().expect("empty") {
        x if &x == ")" || &x == "$" => Nil().into(),
        x => Pair(value(tokens, Some(x)).into(), list(tokens).into()).into(),
    }
}
fn value(tokens: &mut VecDeque<String>, first: Option<String>) -> V {
    match &*first.unwrap_or_else(|| tokens.pop_front().expect("empty")) {
        "(" => list(tokens),
        "#t" => Bool(true).into(),
        "#f" => Bool(false).into(),
        "'" => Quote(value(tokens, None)).into(),
        x => match x.parse::<i64>() {
            Ok(i) => Int(i).into(),
            _ => Symbol(x.into()).into(),
        },
    }
}
enum Val {
    Nil(),
    Bool(bool),
    Int(i64),
    Pair(cell::RefCell<V>, cell::RefCell<V>),
    Symbol(String),
    Quote(V),
    Func(Box<dyn Fn(Vec<V>) -> Result<V>>),
    Form(Box<dyn Fn(&Rc<Env>, Vec<V>) -> Result<V>>),
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
            Form(_) => write!(f, "<form>"),
            Func(_) => write!(f, "<func>"),
        }
    }
}
fn eval(e: &Rc<Env>, v: &V) -> Result<V> {
    Ok(match v.as_ref() {
        Nil() | Bool(_) | Int(_) => v.clone(),
        Pair(x, y) => match eval(e, &x.borrow())?.as_ref() {
            Form(f) => f(e, vec(&y.borrow()))?,
            Func(f) => {
                let u = vec(&y.borrow()).into_iter().map(|v| eval(e, &v));
                f(u.collect::<Result<_>>()?)?
            }
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
        let f = Func(Box::new(move |v| {
            let res = v.into_iter().map(Result::Ok).reduce(|x, y| f(x?, y?));
            res.unwrap_or(Err("empty".into()))
        }));
        self.ensure(s, f)
    }
    fn with_func(self: Rc<Self>, s: &str, f: fn(Vec<V>) -> Result<V>) -> Rc<Env> {
        self.ensure(s, Func(Box::new(f)))
    }
    fn with_form(self: Rc<Self>, s: &str, f: fn(&Rc<Env>, Vec<V>) -> Result<V>) -> Rc<Self> {
        self.ensure(s, Form(Box::new(f)))
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
        .with_func("-", |v| Ok(Int(v[1].int()? - v[0].int()?).into()))
        .with_func("/", |v| Ok(Int(v[1].int()? / v[0].int()?).into()))
        .with_func("=", |v| Ok(Bool(v[1].int()? == v[0].int()?).into()))
        .with_func("<", |v| Ok(Bool(v[1].int()? < v[0].int()?).into()))
        .with_func("<=", |v| Ok(Bool(v[1].int()? <= v[0].int()?).into()))
        .with_func(">", |v| Ok(Bool(v[1].int()? > v[0].int()?).into()))
        .with_func(">=", |v| Ok(Bool(v[1].int()? >= v[0].int()?).into()))
        .with_func("eq?", |v| Ok(Bool(v[1].eq(&v[0])).into()))
        .with_func("cons", |v| {
            Ok(Pair(v[1].clone().into(), v[0].clone().into()).into())
        })
        .with_func("not", |v| Ok(Bool(!v[0].bool()).into()))
        .with_func("car", |v| Ok(v[0].pair()?.0))
        .with_func("cdr", |v| Ok(v[0].pair()?.1))
        .with_func("print", |v| {
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
            Ok(Rc::new(Form(Box::new(move |e2, mut args| {
                let (e, mut params) = (Env::new(Some(e.clone())), vec(&params));
                while let (Some(x), Some(y)) = (params.pop(), args.pop()) {
                    e.ensure(x.symbol()?, eval(e2, &y)?);
                }
                eval_list(&e, v.clone())
            }))))
        })
        .with_form("let", |e, v| do_let(e, v, 0))
        .with_form("let*", |e, v| do_let(e, v, 1))
        .with_form("letrec", |e, v| do_let(e, v, 2))
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
        .with_form("set-car!", |e, v| set_pair(e, v, 0))
        .with_form("set-cdr!", |e, v| set_pair(e, v, 1))
        .ensure("else", Bool(true))
}
fn do_let(e: &Rc<Env>, mut v: Vec<V>, env: usize) -> Result<V> {
    let (mut e2, mut kvs) = (Env::new(Some(e.clone())), vec(&v.pop().ok_or("empty")?));
    while let Some(vk) = kvs.pop().map(|kv| (vec(&kv))) {
        let e3 = match env {
            2 => Env::new(Some(e2.clone())).ensure(vk[1].symbol()?, Nil()),
            _ => Env::new(Some(e2.clone())),
        };
        e2 = e3.ensure(vk[1].symbol()?, eval([e, &e2, &e3][env], &vk[0])?);
    }
    eval_list(&e2, v)
}
fn set_pair(e: &Rc<Env>, v: Vec<V>, i: usize) -> Result<V> {
    match eval(e, &v[1])?.as_ref() {
        Pair(x, y) => *[x, y][i].borrow_mut() = eval(e, &v[0])?,
        _ => return Err("set-cdr: not pair".into()),
    };
    Ok(Nil().into())
}
