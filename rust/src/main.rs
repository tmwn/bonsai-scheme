use std::{cell::RefCell, collections::HashMap, env, error::Error, fs, rc::Rc};

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)?;
    let v = Parser::new(&code).list();
    evaluate_list(default_env(), &v)?;
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
    fn list(&mut self) -> RefValue {
        if self.tokens.is_empty() || self.tokens.last().unwrap() == ")" {
            self.tokens.pop();
            return Value::None().rv();
        }
        Value::Pair(self.value(), self.list()).rv()
    }
    fn value(&mut self) -> RefValue {
        match self.tokens.pop().expect("EOF").as_str() {
            "(" => self.list(),
            "#t" => Value::Bool(true).rv(),
            "#f" => Value::Bool(false).rv(),
            sym => Value::Symbol(sym.to_string()).rv(),
        }
    }
}

enum Value {
    None(),
    Bool(bool),
    Pair(RefValue, RefValue),
    Symbol(String),
    Func(fn(RefEnv, &RefValue) -> RefValue),
}

impl Value {
    fn rv(self) -> RefValue {
        RefValue {
            r: Rc::new(RefCell::new(self)),
        }
    }
}

#[derive(Clone)]
struct RefValue {
    r: Rc<RefCell<Value>>,
}

struct Env {
    m: HashMap<String, RefValue>,
    next: Option<RefEnv>,
}

#[derive(Clone)]
struct RefEnv(Rc<RefCell<Env>>);

impl RefEnv {
    fn lookup(&self, s: &str) -> RefValue {
        if let Some(v) = self.0.borrow().m.get(s) {
            return v.clone();
        }
        self.0
            .borrow()
            .next
            .as_ref()
            .expect(&format!("not found: {}", s))
            .lookup(s)
    }
    fn ensure(self, s: &str, v: RefValue) -> Self {
        self.0.borrow_mut().m.insert(s.to_string(), v);
        return self;
    }
}

impl std::fmt::Display for RefValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.r.borrow() {
            Value::None() => write!(f, "()"),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Pair(x, y) => write!(f, "( {} {} )", x, y),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::Func(_) => write!(f, "<func>"),
        }
    }
}

fn default_env() -> RefEnv {
    RefEnv(Rc::new(RefCell::new(Env {
        m: HashMap::new(),
        next: None,
    })))
    .ensure(
        "print",
        Value::Func(|e, v| {
            if let Value::Pair(ref x, _) = *(Rc::clone(&v.r)).borrow() {
                let res = evaluate(e, x);
                println!("{}", res);
                Value::None().rv()
            } else {
                panic!("not pair")
            }
        })
        .rv(),
    )
}

fn evaluate(e: RefEnv, v: &RefValue) -> RefValue {
    match &*v.r.borrow() {
        Value::None() => Value::None().rv(),
        Value::Bool(x) => Value::Bool(*x).rv(),
        Value::Pair(ref x, ref y) => {
            if let Value::Func(ref f) = &*Rc::clone(&evaluate(e.clone(), x).r).borrow_mut() {
                return f(e, y);
            }
            panic!("not func")
        }
        Value::Symbol(x) => e.lookup(x),
        _ => panic!("BUG"),
    }
}

fn to_vec(v: &RefValue) -> Result<Vec<RefValue>, String> {
    match &*v.r.borrow() {
        Value::Pair(ref x, ref y) => {
            let mut res = vec![x.clone()];
            res.append(&mut to_vec(y)?);
            Ok(res)
        }
        Value::None() => Ok(Vec::new()),
        _ => Err(format!("unexpected type {}", v)),
    }
}

fn evaluate_list(e: RefEnv, v: &RefValue) -> Result<RefValue, String> {
    let mut res = Value::None().rv();
    for x in to_vec(v)? {
        res = evaluate(e.clone(), &x);
    }
    Ok(res)
}
