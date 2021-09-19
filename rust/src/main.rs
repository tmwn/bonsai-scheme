use std::{cell::RefCell, collections::HashMap, env, error::Error, fs, rc::Rc};

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)?;
    let v = Parser::new(&code).list();
    evaluate_list(&default_env(), &v)?;
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
        new_pair(self.value(), self.list())
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
    Func(fn(&Env, &Value) -> Value),
}

fn new_pair(x: Value, y: Value) -> Value {
    Value::Pair(Rc::new(x), Rc::new(y))
}

struct Env {
    m: RefCell<HashMap<String, Rc<Value>>>,
    next: Option<Box<Env>>,
}

impl Env {
    fn lookup(&self, s: &str) -> Rc<Value> {
        if let Some(v) = self.m.borrow().get(s) {
            return Rc::clone(v);
        }
        self.next
            .as_ref()
            .expect(&format!("not found: {}", s))
            .lookup(s)
    }
    fn ensure(self, s: &str, v: Rc<Value>) -> Self {
        self.m.borrow_mut().insert(s.to_string(), v);
        return self;
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

fn default_env() -> Env {
    Env {
        m: RefCell::new(HashMap::new()),
        next: None,
    }
    .ensure(
        "print",
        Rc::new(Value::Func(|e, v| {
            if let Value::Pair(ref x, _) = v {
                let res = evaluate(e, x);
                println!("{}", res);
                return Value::None();
            }
            panic!("not pair")
        })),
    )
}

fn evaluate(e: &Env, v: &Value) -> Rc<Value> {
    match v {
        Value::None() => Rc::new(Value::None()),
        Value::Bool(x) => Rc::new(Value::Bool(*x)),
        Value::Pair(ref x, ref y) => {
            if let Value::Func(ref f) = &*evaluate(e, x) {
                Rc::new(f(e, y))
            } else {
                panic!("not func")
            }
        }
        Value::Symbol(x) => e.lookup(x),
        _ => panic!("BUG"),
    }
}

fn to_vec(v: &Value) -> Result<Vec<Rc<Value>>, String> {
    match v {
        Value::Pair(ref x, ref y) => {
            let mut res = vec![x.clone()];
            res.append(&mut to_vec(y)?);
            Ok(res)
        }
        Value::None() => Ok(Vec::new()),
        _ => Err(format!("unexpected type {}", v)),
    }
}

fn evaluate_list(e: &Env, v: &Value) -> Result<Rc<Value>, String> {
    let mut res = Rc::new(Value::None());
    for x in to_vec(v)? {
        res = evaluate(e.clone(), &x);
    }
    Ok(res)
}
