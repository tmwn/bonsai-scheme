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
            return Value::None();
        }
        Value::Pair(self.value().into(), self.list().into())
    }
    fn value(&mut self) -> Value {
        match self.tokens.pop().expect("EOF").as_str() {
            "(" => self.list(),
            "#t" => Value::Bool(true),
            "#f" => Value::Bool(false),
            sym => Value::Symbol(sym.to_string()),
        }
    }
}
enum Value {
    None(),
    Bool(bool),
    Pair(Rc<Value>, Rc<Value>),
    Symbol(String),
    Func(Box<dyn Fn(&Env, &Value) -> Result<Rc<Value>>>),
}
impl Value {
    fn first(&self) -> Result<&Value> {
        if let Value::Pair(x, _) = self {
            return Ok(x);
        }
        Err("not pair".into())
    }
    fn second(&self) -> Result<&Value> {
        if let Value::Pair(_, y) = self {
            return Ok(y);
        }
        Err("not pair".into())
    }
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None() => write!(f, "()"),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Pair(x, y) => write!(f, "( {} {} )", x, y),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::Func(_) => write!(f, "<func>"),
        }
    }
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
        self.ensure(
            s,
            Value::Func(Box::new(move |e, v| f(&*eval(e, v.first()?)?))).into(),
        )
    }
}
fn default_env() -> Env {
    Env {
        m: RefCell::new(HashMap::new()),
        next: None,
    }
    .with_op1("print", |v| {
        println!("{}", v);
        Ok(Value::None().into())
    })
}
fn eval(e: &Env, v: &Value) -> Result<Rc<Value>> {
    Ok(match v {
        Value::None() => Value::None().into(),
        Value::Bool(x) => Value::Bool(*x).into(),
        Value::Pair(x, y) => match eval(e, x)?.as_ref() {
            Value::Func(f) => f(e, y)?,
            _ => return Err("not func".into()),
        },
        Value::Symbol(x) => e.lookup(x)?,
        _ => return Err("BUG".into()),
    })
}
fn eval_list(e: &Env, v: &Value) -> Result<Rc<Value>> {
    let res = eval(e, v.first()?)?;
    if let Value::None() = v.second()? {
        return Ok(res);
    }
    eval_list(e, v.second()?)
}
