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
    fn list(&mut self) -> Value {
        if self.tokens.is_empty() || self.tokens.last().unwrap() == ")" {
            self.tokens.pop();
            return Nil();
        }
        Pair(self.value().into(), self.list().into())
    }
    fn value(&mut self) -> Value {
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
enum Value {
    Nil(),
    Bool(bool),
    Int(i64),
    Pair(Rc<Value>, Rc<Value>),
    Symbol(String),
    Quote(Rc<Value>),
    Func(Box<dyn Fn(&Env, &Value) -> Result<Rc<Value>>>),
}
use Value::*;
impl Value {
    fn first(&self) -> Result<&Value> {
        if let Pair(x, _) = self {
            return Ok(x);
        }
        Err("not pair".into())
    }
    fn second(&self) -> Result<&Value> {
        if let Pair(_, y) = self {
            return Ok(y);
        }
        Err("not pair".into())
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
}
impl std::fmt::Display for Value {
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
fn eval(e: &Env, v: &Value) -> Result<Rc<Value>> {
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
fn eval_list(e: &Env, v: &Value) -> Result<Rc<Value>> {
    let res = eval(e, v.first()?)?;
    if let Nil() = v.second()? {
        return Ok(res);
    }
    eval_list(e, v.second()?)
}
struct Env {
    m: RefCell<HashMap<String, Rc<Value>>>,
    next: Option<Box<Env>>,
}
impl Env {
    fn lookup(&self, s: &str) -> Result<Rc<Value>> {
        match (self.m.borrow().get(s), self.next.as_ref()) {
            (Some(v), _) => Ok(Rc::clone(v)),
            (_, Some(e)) => e.lookup(s),
            _ => Err(format!("not found: {}", s).into()),
        }
    }
    fn ensure(self, s: &str, v: Rc<Value>) -> Self {
        self.m.borrow_mut().insert(s.to_string(), v);
        self
    }
    fn with_op1(self, s: &str, f: fn(&Value) -> Result<Rc<Value>>) -> Self {
        self.with_func(s, Box::new(move |e, v| f(&*eval(e, v.first()?)?)))
    }
    fn with_op2(self, s: &str, f: fn(&Value, &Value) -> Result<Rc<Value>>) -> Self {
        self.with_func(
            s,
            Box::new(move |e, v| f(&*eval(e, v.first()?)?, &*eval(e, v.second()?.first()?)?)),
        )
    }
    fn with_fold(self, s: &str, f: fn(&Value, &Value) -> Result<Rc<Value>>) -> Self {
        self.with_func(
            s,
            Box::new(move |e, mut v| {
                let mut acc = eval(e, v.first()?)?;
                while let Value::Pair(x, _) = v.second()? {
                    acc = f(&*acc, &*eval(e, &*x)?)?;
                    v = v.second()?;
                }
                Ok(acc)
            }),
        )
    }
    fn with_func(self, s: &str, f: Box<dyn Fn(&Env, &Value) -> Result<Rc<Value>>>) -> Self {
        self.ensure(s, Func(f).into())
    }
}
fn default_env() -> Env {
    Env {
        m: RefCell::new(HashMap::new()),
        next: None,
    }
    .with_fold("+", |x, y| Ok(Int(x.int()? + y.int()?).into()))
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
    .with_op1("not", |x| Ok(Bool(!x.bool()).into()))
    .with_op1("print", |v| {
        println!("{}", v);
        Ok(Nil().into())
    })
    .with_func("begin", Box::new(|e, v| eval_list(e, v)))
}
