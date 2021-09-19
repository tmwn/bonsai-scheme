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
        Pair(self.value().into(), self.list().into())
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
    Pair(Rc<Val>, Rc<Val>),
    Symbol(String),
    Quote(Rc<Val>),
    Func(Box<dyn Fn(&Rc<Env>, &Rc<Val>) -> Result<Rc<Val>>>),
}
use Val::*;
impl Val {
    fn first(&self) -> Result<&Rc<Val>> {
        if let Pair(x, _) = self {
            return Ok(x);
        }
        Err("first: not pair".into())
    }
    fn second(&self) -> Result<&Rc<Val>> {
        if let Pair(_, y) = self {
            return Ok(y);
        }
        Err("second: not pair".into())
    }
    fn int(&self) -> Result<&i64> {
        if let Int(x) = self {
            return Ok(x);
        }
        Err("not int".into())
    }
    fn bool(&self) -> bool {
        !matches!(self, Bool(false))
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
impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil() => write!(f, "()"),
            Bool(true) => write!(f, "#t"),
            Bool(false) => write!(f, "#f"),
            Int(i) => write!(f, "{}", i),
            Pair(x, y) => write!(f, "( {} {} )", x, y),
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
        Pair(x, y) => match eval(e, x)?.as_ref() {
            Func(f) => f(e, y)?,
            _ => return Err("not func".into()),
        },
        Symbol(x) => e.lookup(x)?,
        Quote(x) => x.clone(),
        _ => return Err(format!("{}", v).into()),
    })
}
fn eval_list(e: &Rc<Env>, v: &Val) -> Result<Rc<Val>> {
    let res = eval(e, v.first()?)?;
    if let Nil() = v.second()?.as_ref() {
        return Ok(res);
    }
    eval_list(e, v.second()?)
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
        self.with_func(s, Box::new(move |e, v| f(&eval(e, v.first()?)?)))
    }
    fn with_op2(&self, s: &str, f: fn(&Rc<Val>, &Rc<Val>) -> Result<Rc<Val>>) -> &Self {
        self.with_func(
            s,
            Box::new(move |e, v| f(&eval(e, v.first()?)?, &eval(e, v.second()?.first()?)?)),
        )
    }
    fn with_fold(&self, s: &str, f: fn(&Rc<Val>, &Rc<Val>) -> Result<Rc<Val>>) -> &Self {
        self.with_func(
            s,
            Box::new(move |e, mut v| {
                let mut acc = eval(e, v.first()?)?;
                while let Val::Pair(x, _) = v.second()?.as_ref() {
                    acc = f(&acc, &eval(e, &*x)?)?;
                    v = v.second()?;
                }
                Ok(acc)
            }),
        )
    }
    fn with_func(&self, s: &str, f: Box<dyn Fn(&Rc<Env>, &Rc<Val>) -> Result<Rc<Val>>>) -> &Self {
        self.ensure(s, Func(f).into())
    }
    fn new_frame(self: &Rc<Self>) -> Rc<Self> {
        Env {
            m: HashMap::new().into(),
            next: Some(self.clone()),
        }
        .into()
    }
    fn set(&self, s: &str, v: Rc<Val>) -> Result<()> {
        if self.m.borrow().contains_key(s) {
            self.m.borrow_mut().insert(s.to_string(), v);
            return Ok(());
        }
        self.next.as_ref().ok_or("set: not found")?.set(s, v)
    }
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
        .with_op2("cons", |x, y| Ok(Pair(x.clone(), y.clone()).into()))
        .with_op1("not", |x| Ok(Bool(!x.bool()).into()))
        .with_op1("car", |x| Ok(x.first()?.clone()))
        .with_op1("cdr", |x| Ok(x.second()?.clone()))
        .with_op1("print", |v| {
            println!("{}", v);
            Ok(Nil().into())
        })
        .with_func("begin", Box::new(|e, v| eval_list(e, v)))
        .with_func(
            "define",
            Box::new(|e, v| {
                match v.first()?.as_ref() {
                    Symbol(name) => e.ensure(name, eval(e, v.second()?.first()?)?),
                    Pair(name, args) => e.ensure(
                        name.symbol()?,
                        eval(
                            e,
                            &Pair(
                                Symbol("lambda".into()).into(),
                                Pair(args.clone(), v.second()?.clone()).into(),
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
                Ok(Func(Box::new(move |e2, mut v2| {
                    let (e, mut args) = (e.new_frame(), v.first()?);
                    while let (Pair(x, n_args), Pair(y, n_v2)) = (args.as_ref(), v2.as_ref()) {
                        e.ensure(x.symbol()?, eval(e2, y)?);
                        args = n_args;
                        v2 = n_v2;
                    }
                    eval_list(&e, v.second()?)
                }))
                .into())
            }),
        )
        .with_func(
            "let",
            Box::new(|e, v| {
                let (e2, mut kvs) = (e.new_frame(), v.first()?);
                while let Pair(kv, n_kvs) = kvs.as_ref() {
                    e2.ensure(kv.first()?.symbol()?, eval(&e, kv.second()?.first()?)?);
                    kvs = n_kvs;
                }
                eval_list(&e2, v.second()?)
            }),
        )
        .with_func(
            "let*",
            Box::new(|e, v| {
                let (mut e, mut kvs) = (e.clone(), v.first()?);
                while let Pair(kv, n_kvs) = kvs.as_ref() {
                    let v = eval(&e, kv.second()?.first()?)?;
                    e = e.new_frame();
                    e.ensure(kv.first()?.symbol()?, v);
                    kvs = n_kvs;
                }
                eval_list(&e, v.second()?)
            }),
        )
        .with_func(
            "letrec",
            Box::new(|e, v| {
                let (e, mut kvs) = (e.new_frame(), v.first()?);
                while let Pair(kv, n_kvs) = kvs.as_ref() {
                    e.ensure(kv.first()?.symbol()?, Nil().into());
                    e.ensure(kv.first()?.symbol()?, eval(&e, kv.second()?.first()?)?);
                    kvs = n_kvs;
                }
                eval_list(&e, v.second()?)
            }),
        )
        .with_func(
            "if",
            Box::new(|e, v| match eval(e, v.first()?)?.bool() {
                true => eval(e, v.second()?.first()?),
                false => eval(e, v.second()?.second()?.first()?),
            }),
        )
        .with_func(
            "cond",
            Box::new(|e, mut v| loop {
                let f = v.first()?;
                if eval(e, f.first()?)?.bool() {
                    break eval(e, f.second()?.first()?);
                }
                v = v.second()?;
            }),
        )
        .with_func(
            "set!",
            Box::new(|e, v| {
                e.set(v.first()?.symbol()?, eval(e, v.second()?.first()?)?)?;
                Ok(Nil().into())
            }),
        )
        .ensure("else", Bool(true).into());
    res.into()
}
