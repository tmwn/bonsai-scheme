use std::{cell::RefCell, collections::HashMap, env, error::Error, fs, rc::Rc};

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)?;
    let v = Parser::new(&code).list();
    println!("#t");
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
        if self.tokens.is_empty() || self.tokens[0] == ")" {
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
    next: Rc<Env>,
}

impl Env {
    fn lookup(&self, s: &str) -> RefValue {
        if let Some(v) = self.m.get(s) {
            return v.clone();
        }
        self.next.lookup(s)
    }
    fn ensure(&mut self, s: &str, v: RefValue) -> &Self {
        self.m.insert(s.to_string(), v);
        return self;
    }
}
