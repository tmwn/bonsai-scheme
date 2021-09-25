use std::{cell::RefCell, collections::HashMap, env, error::Error, fs, rc::Rc};
use typed_arena::Arena;
type Ctx<'a> = Arena<Val<'a>>;
type Result<T> = std::result::Result<T, Box<dyn Error>>;
fn main() -> Result<()> {
    let code = fs::read_to_string(env::args().nth(1).ok_or("no file")?)? + " $";
    run(&Ctx::new(), code)?;
    Ok(())
}
enum Val<'a> {
    Nil(),
    Bool(bool),
    Int(i64),
    Pair(&'a Val<'a>, &'a Val<'a>),
    Symbol(String),
    Quote(&'a Val<'a>),
    Func(fn(&'a Ctx<'a>, Vec<&Val>) -> Result<&'a Val<'a>>),
    // Form(Box<dyn Fn(&Rc<Env>, Vec<&'a Val<'a>>) -> Result<&'a Val<'a>>>),
}
use Val::*;
impl<'a> Val<'a> {
    fn to_vec(&self) -> Vec<&Val<'a>> {
        match self {
            Pair(x, y) => [y.to_vec(), vec![x]].concat(),
            _ => vec![],
        }
    }
    fn int(&self) -> Result<i64> {
        match self {
            Int(x) => Ok(*x),
            _ => Err("not int".into()),
        }
    }
}
impl<'a> std::fmt::Display for Val<'a> {
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
fn run<'a>(ctx: &'a Ctx<'a>, mut code: String) -> Result<&'a Val<'a>> {
    let cs = "()'".chars();
    cs.for_each(|c| code = code.replace(c, &format!(" {} ", c)));
    let mut tokens = code.split_whitespace().rev().collect();
    let root = list(ctx, &mut tokens)?;
    eval_list(ctx, &default_env(ctx), root.to_vec())
}
fn list<'a>(ctx: &'a Ctx<'a>, tokens: &mut Vec<&str>) -> Result<&'a Val<'a>> {
    Ok(match tokens.pop().ok_or("empty")? {
        x if x == ")" || x == "$" => ctx.alloc(Nil()),
        x => {
            tokens.push(x);
            ctx.alloc(Pair(value(ctx, tokens)?.into(), list(ctx, tokens)?.into()))
        }
    })
}
fn value<'a>(ctx: &'a Ctx<'a>, tokens: &mut Vec<&str>) -> Result<&'a Val<'a>> {
    Ok(match tokens.pop().ok_or("empty")? {
        "(" => list(ctx, tokens)?,
        "#t" => ctx.alloc(Bool(true)),
        "#f" => ctx.alloc(Bool(false)),
        "'" => ctx.alloc(Quote(value(ctx, tokens)?)),
        x => match x.parse::<i64>() {
            Ok(i) => ctx.alloc(Int(i)),
            _ => ctx.alloc(Symbol(x.into())),
        },
    })
}
fn eval_list<'a>(ctx: &'a Ctx<'a>, e: &Rc<Env<'a>>, v: Vec<&'a Val<'a>>) -> Result<&'a Val<'a>> {
    let res = v.into_iter().rev().map(|x| eval(ctx, e, x)).last();
    res.unwrap_or(Err("empty".into()))
}
fn eval<'a>(ctx: &'a Ctx<'a>, e: &Rc<Env<'a>>, v: &'a Val<'a>) -> Result<&'a Val<'a>> {
    Ok(match v {
        Nil() | Bool(_) | Int(_) => v,
        Pair(x, y) => match eval(ctx, e, x)? {
            Func(f) => {
                let u = y.to_vec().into_iter().map(|v| eval(ctx, e, v));
                f(ctx, u.collect::<Result<_>>()?)?
            }
            x => return Err(format!("not func: {}", x).into()),
        },
        Symbol(x) => e.lookup(x)?,
        Quote(x) => x.clone(),
        _ => return Err(format!("eval: {}", v).into()),
    })
}
struct Env<'a> {
    m: RefCell<HashMap<String, &'a Val<'a>>>,
    next: Option<Rc<Env<'a>>>,
}
impl<'a> Env<'a> {
    fn new(next: Option<Rc<Env<'a>>>) -> Rc<Self> {
        Rc::new(Env {
            m: HashMap::new().into(),
            next,
        })
    }
    fn lookup(&self, s: &str) -> Result<&'a Val<'a>> {
        match (self.m.borrow().get(s), self.next.as_ref()) {
            (Some(v), _) => Ok(v),
            (_, Some(e)) => e.lookup(s),
            _ => Err(format!("not found: {}", s).into()),
        }
    }
    fn ensure(self: &Rc<Self>, s: &str, v: &'a Val<'a>) -> Rc<Self> {
        self.m.borrow_mut().insert(s.to_string(), v);
        self.clone()
    }
    fn with_func(
        self: Rc<Self>,
        ctx: &'a Ctx<'a>,
        s: &str,
        f: fn(&'a Ctx<'a>, Vec<&Val<'_>>) -> Result<&'a Val<'a>>,
    ) -> Rc<Self> {
        self.ensure(s, ctx.alloc(Val::<'a>::Func(f)))
    }
}
fn default_env<'a>(ctx: &'a Ctx<'a>) -> Rc<Env<'a>> {
    Env::new(None)
        .with_func(ctx, "-", |ctx, v| {
            Ok(ctx.alloc(Int(v[1].int()? - v[0].int()?)))
        })
        .with_func(ctx, "print", |ctx, v| {
            print!("{}", v[0]);
            Ok(ctx.alloc(Nil()))
        })
}
