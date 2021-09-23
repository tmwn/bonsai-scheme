use std::{cell::RefCell, collections::HashMap, env, error::Error, fs, rc::Rc};
type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)?;
    let v = Parser::new(&code).list();
    eval_list(&default_env(), &v)?;
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
        new_pair(self.value().into(), self.list().into())
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
    Func(Box<dyn Fn(&Rc<Env>, &Rc<Val>) -> Result<Rc<Val>>>),
}

use Val::*;
impl Val {
    fn car(&self) -> Result<Rc<Val>> {
        Ok(self.pair()?.0)
    }
    fn cdr(&self) -> Result<Rc<Val>> {
        Ok(self.pair()?.1)
    }
    fn pair(&self) -> Result<(Rc<Val>, Rc<Val>)> {
        if let Pair(x, y) = self {
            return Ok((x.borrow().clone(), y.borrow().clone()));
        }
        Err("pair: not pair".into())
    }
    fn int(&self) -> Result<&i64> {
        if let Int(x) = self {
            return Ok(x);
        }
        Err("not int".into())
    }
    fn bool(&self) -> bool {
        !self.eq(&Bool(false))
    }
    fn symbol(&self) -> Result<&str> {
        if let Symbol(x) = self {
            return Ok(x);
        }
        Err("not symbol".into())
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
fn new_pair(x: Rc<Val>, y: Rc<Val>) -> Val {
    Pair(RefCell::new(x), RefCell::new(y))
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
fn eval(e: &Rc<Env>, v: &Val) -> Result<Rc<Val>> {
    Ok(match v {
        Nil() => Nil().into(),
        Bool(x) => Bool(*x).into(),
        Int(x) => Int(*x).into(),
        Pair(x, y) => match eval(e, x.borrow().as_ref())?.as_ref() {
            Func(f) => f(e, &y.borrow())?,
            _ => return Err("not func".into()),
        },
        Symbol(x) => e.lookup(x)?,
        Quote(x) => x.clone(),
        _ => return Err(format!("{}", v).into()),
    })
}
fn eval_list(e: &Rc<Env>, v: &Val) -> Result<Rc<Val>> {
    let res = eval(e, v.car()?.as_ref())?;
    if let Nil() = v.cdr()?.as_ref() {
        return Ok(res);
    }
    eval_list(e, v.cdr()?.as_ref())
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
    fn ensure(&self, s: &str, v: Rc<Val>) -> &Self {
        self.m.borrow_mut().insert(s.to_string(), v);
        self
    }
    fn with_op1(&self, s: &str, f: fn(&Rc<Val>) -> Result<Rc<Val>>) -> &Self {
        self.with_func(s, Box::new(move |e, v| f(&eval(e, v.car()?.as_ref())?)))
    }
    fn with_op2(&self, s: &str, f: fn(&Rc<Val>, &Rc<Val>) -> Result<Rc<Val>>) -> &Self {
        self.with_func(
            s,
            Box::new(move |e, v| f(&eval(e, &*v.car()?)?, &eval(e, &*v.cdr()?.car()?)?)),
        )
    }
    fn with_fold(&self, s: &str, f: fn(&Rc<Val>, &Rc<Val>) -> Result<Rc<Val>>) -> &Self {
        self.with_func(
            s,
            Box::new(move |e, v| {
                let mut v = v.clone();
                let mut acc = eval(e, v.car()?.as_ref())?;
                while let Ok(x) = v.cdr()?.car() {
                    acc = f(&acc, &eval(e, &x)?)?;
                    v = v.cdr()?;
                }
                Ok(acc)
            }),
        )
    }
    fn with_func(&self, s: &str, f: Box<dyn Fn(&Rc<Env>, &Rc<Val>) -> Result<Rc<Val>>>) -> &Self {
        self.ensure(s, Func(f).into())
    }
    fn set(&self, s: &str, v: Rc<Val>) -> Result<()> {
        if self.m.borrow().contains_key(s) {
            self.m.borrow_mut().insert(s.to_string(), v);
            return Ok(());
        }
        self.next.as_ref().ok_or("set: not found")?.set(s, v)
    }
}
fn new_frame(e: &Rc<Env>) -> Rc<Env> {
    Env {
        m: HashMap::new().into(),
        next: Some(e.clone()),
    }
    .into()
}
fn default_env() -> Rc<Env> {
    let res = Env {
        m: RefCell::new(HashMap::new()),
        next: None,
    };
    res.with_fold("+", |x, y| Ok(Int(x.int()? + y.int()?).into()))
        .with_fold("*", |x, y| Ok(Int(x.int()? * y.int()?).into()))
        .with_fold("and", |x, y| Ok(Bool(x.bool() && y.bool()).into()))
        .with_fold("or", |x, y| Ok(Bool(x.bool() || y.bool()).into()))
        .with_op2("-", |x, y| Ok(Int(x.int()? - y.int()?).into()))
        .with_op2("/", |x, y| Ok(Int(x.int()? / y.int()?).into()))
        .with_op2("=", |x, y| Ok(Bool(x.int()? == y.int()?).into()))
        .with_op2("<", |x, y| Ok(Bool(x.int()? < y.int()?).into()))
        .with_op2("<=", |x, y| Ok(Bool(x.int()? <= y.int()?).into()))
        .with_op2(">", |x, y| Ok(Bool(x.int()? > y.int()?).into()))
        .with_op2(">=", |x, y| Ok(Bool(x.int()? >= y.int()?).into()))
        .with_op2("eq?", |x, y| Ok(Bool(x.eq(y)).into()))
        .with_op2("cons", |x, y| Ok(new_pair(x.clone(), y.clone()).into()))
        .with_op1("not", |x| Ok(Bool(!x.bool()).into()))
        .with_op1("car", |x| Ok(x.car()?.clone()))
        .with_op1("cdr", |x| Ok(x.cdr()?.clone()))
        .with_op1("print", |v| {
            println!("{}", v);
            Ok(Nil().into())
        })
        .with_func("begin", Box::new(|e, v| eval_list(e, v)))
        .with_func(
            "define",
            Box::new(|e, v| {
                match v.car()?.as_ref() {
                    Symbol(name) => e.ensure(name, eval(e, v.cdr()?.car()?.as_ref())?),
                    Pair(name, args) => e.ensure(
                        name.borrow().symbol()?,
                        eval(
                            e,
                            &new_pair(
                                Symbol("lambda".into()).into(),
                                new_pair(args.borrow().clone(), v.cdr()?).into(),
                            ),
                        )?,
                    ),
                    _ => return Err("ill-formed define".into()),
                };
                Ok(Nil().into())
            }),
        )
        .with_func(
            "lambda",
            Box::new(|e, v| {
                let (e, v) = (e.clone(), v.clone());
                Ok(Func(Box::new(move |e2, v2| {
                    let (e, mut args) = (new_frame(&e), v.car()?);
                    let mut v2 = v2.clone();
                    while let (Ok((x, n_args)), Ok((y, n_v2))) = (args.pair(), v2.pair()) {
                        e.ensure(x.symbol()?, eval(e2, y.as_ref())?);
                        args = n_args.clone();
                        v2 = n_v2;
                    }
                    eval_list(&e, v.cdr()?.as_ref())
                }))
                .into())
            }),
        )
        .with_func(
            "let",
            Box::new(|e, v| {
                let (e2, mut kvs) = (new_frame(e), v.car()?);
                while let Ok((ref kv, ref n_kvs)) = kvs.pair() {
                    e2.ensure(kv.car()?.symbol()?, eval(&e, kv.cdr()?.car()?.as_ref())?);
                    kvs = n_kvs.clone();
                }
                eval_list(&e2, v.cdr()?.as_ref())
            }),
        )
        .with_func(
            "let*",
            Box::new(|e, v| {
                let (mut e, mut kvs) = (e.clone(), v.car()?);
                while let Ok((ref kv, n_kvs)) = kvs.pair() {
                    let v = eval(&e, kv.cdr()?.car()?.as_ref())?;
                    e = new_frame(&e);
                    e.ensure(kv.car()?.symbol()?, v);
                    kvs = n_kvs;
                }
                eval_list(&e, v.cdr()?.as_ref())
            }),
        )
        .with_func(
            "letrec",
            Box::new(|e, v| {
                let (e, mut kvs) = (new_frame(e), v.car()?);
                while let Ok((ref kv, n_kvs)) = kvs.pair() {
                    e.ensure(kv.car()?.symbol()?, Nil().into());
                    e.ensure(kv.car()?.symbol()?, eval(&e, &*kv.cdr()?.car()?)?);
                    kvs = n_kvs;
                }
                eval_list(&e, v.cdr()?.as_ref())
            }),
        )
        .with_func(
            "if",
            Box::new(|e, v| match eval(e, v.car()?.as_ref())?.bool() {
                true => eval(e, v.cdr()?.car()?.as_ref()),
                false => eval(e, v.cdr()?.cdr()?.car()?.as_ref()),
            }),
        )
        .with_func(
            "cond",
            Box::new(|e, v| {
                let mut v = v.clone();
                loop {
                    let f = v.car()?;
                    if eval(e, f.car()?.as_ref())?.bool() {
                        break eval(e, f.cdr()?.car()?.as_ref());
                    }
                    v = v.cdr()?;
                }
            }),
        )
        .with_func(
            "set!",
            Box::new(|e, v| {
                e.set(v.car()?.symbol()?, eval(e, v.cdr()?.car()?.as_ref())?)?;
                Ok(Nil().into())
            }),
        )
        .with_func(
            "set-car!",
            Box::new(|e, v| {
                match eval(e, v.car()?.as_ref())?.as_ref() {
                    Pair(x, _) => *x.borrow_mut() = eval(e, v.cdr()?.car()?.as_ref())?,
                    _ => return Err("set-car: not pair".into()),
                };
                Ok(Nil().into())
            }),
        )
        .with_func(
            "set-cdr!",
            Box::new(|e, v| {
                match eval(e, v.car()?.as_ref())?.as_ref() {
                    Pair(_, x) => *x.borrow_mut() = eval(e, v.cdr()?.car()?.as_ref())?,
                    _ => return Err("set-cdr: not pair".into()),
                };
                Ok(Nil().into())
            }),
        )
        .ensure("else", Bool(true).into());
    res.into()
}
