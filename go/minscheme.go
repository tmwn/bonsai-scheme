package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

func main() {
	b, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	code = string(b) + ")"

	newEnv().evalList(list())
}

var code string

type Kind int

const (
	KindNone Kind = iota
	KindBool
	KindInt
	KindPair
	KindSymbol
	KindFunc
	KindQuote
)

type Value struct {
	kind     Kind
	first    *Value // Pair
	second   *Value
	symbol   string
	boolVal  bool
	intVal   int
	function func(e *Env, v *Value) (*Env, *Value)
	quoted   *Value
}

func (v *Value) asBool() bool {
	switch v.kind {
	case KindBool:
		return v.boolVal
	}
	return true
}

func (v *Value) eq(u *Value) bool {
	if v.kind != u.kind {
		return false
	}
	switch v.kind {
	case KindNone:
		return true
	case KindBool:
		return v.boolVal == u.boolVal
	case KindInt:
		return v.intVal == u.intVal
	case KindQuote:
		return v.quoted.eq(u.quoted)
	case KindSymbol:
		return v.symbol == u.symbol
	}
	return false
}

func next() byte {
	c := code[0]
	code = code[1:]
	return c
}

func peek() byte {
	return code[0]
}

func skip() {
	if len(code) == 0 {
		return
	}
	c := peek()
	if c == ' ' || c == '\n' {
		next()
		skip()
	}
}

func token() string {
	skip()
	c := next()
	if c == '(' {
		return "("
	} else if c == ')' {
		return ")"
	} else if c == '#' {
		return string(c) + string(next())
	} else if c == '\'' {
		return "'"
	} else {
		s := ""
		for {
			s += string(c)
			c = peek()
			if c != '(' && c != ')' && c != ' ' && c != '\n' {
				next()
				continue
			}
			return s
		}
	}
}

func list() *Value {
	skip()
	if peek() == ')' {
		next()
		return &Value{kind: KindNone}
	}
	return &Value{
		kind:   KindPair,
		first:  value(),
		second: list(),
	}
}

func value() *Value {
	t := token()
	if t[0] == '(' {
		return list()
	} else if t[0] == '#' {
		return &Value{
			kind:    KindBool,
			boolVal: t[1] == 't',
		}
	} else if t == "'" {
		return &Value{
			kind:   KindQuote,
			quoted: value(),
		}
	} else if '0' <= t[0] && t[0] <= '9' || t[0] == '-' && len(t) > 1 {
		i, err := strconv.Atoi(t)
		if err != nil {
			panic(err)
		}
		return &Value{
			kind:   KindInt,
			intVal: i,
		}
	} else {
		return &Value{
			kind:   KindSymbol,
			symbol: t,
		}
	}
}

type Env struct {
	symbol string
	value  *Value
	next   *Env
}

func (e *Env) lookup(s string) *Value {
	if e == nil {
		panic("not found: " + s)
	}
	if e.symbol == s {
		return e.value
	}
	return e.next.lookup(s)
}

func (e *Env) set(s string, v *Value) {
	if e == nil {
		panic("not found symbol: " + s)
	}
	if e.symbol == s {
		e.value = v
		return
	}
	e.next.set(s, v)
}

func (e *Env) pushed(symbol string, value *Value) *Env {
	return &Env{
		symbol: symbol,
		value:  value,
		next:   e,
	}
}

func (e *Env) eval(v *Value) (*Env, *Value) {
	// fmt.Printf("%v\n", v)
	switch v.kind {
	case KindNone:
		return e, v
	case KindBool:
		return e, v
	case KindInt:
		return e, v
	case KindQuote:
		return e, v.quoted
	case KindPair:
		e, u := e.eval(v.first)
		return u.function(e, v.second)
	case KindSymbol:
		return e, e.lookup(v.symbol)
	}
	panic(v.symbol)
}

func (e *Env) evalList(v *Value) *Value {
	res := &Value{kind: KindNone}
	for v.kind != KindNone {
		e, res = e.eval(v.first)
		v = v.second
	}
	return res
}

func (v *Value) String() string {
	switch v.kind {
	case KindNone:
		return "()"
	case KindBool:
		if v.boolVal {
			return "#t"
		} else {
			return "#f"
		}
	case KindInt:
		return strconv.Itoa(v.intVal)
	case KindPair:
		return fmt.Sprintf("( %s . %s )", v.first, v.second)
	case KindSymbol:
		return v.symbol
	default:
		panic("BUG")
	}
}

func newEnv() *Env {
	var env *Env
	return env.pushed("-", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:   KindInt,
				intVal: x.intVal - y.intVal,
			}
		},
	}).pushed("+", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:   KindInt,
				intVal: x.intVal + y.intVal,
			}
		},
	}).pushed("*", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			res := 1
			for ; v.kind != KindNone; v = v.second {
				_, x := e.eval(v.first)
				res *= x.intVal
			}
			return e, &Value{
				kind:   KindInt,
				intVal: res,
			}
		},
	}).pushed("/", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:   KindInt,
				intVal: x.intVal / y.intVal,
			}
		},
	}).pushed("=", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: x.intVal == y.intVal,
			}
		},
	}).pushed("<", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: x.intVal < y.intVal,
			}
		},
	}).pushed("<=", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: x.intVal <= y.intVal,
			}
		},
	}).pushed(">", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: x.intVal > y.intVal,
			}
		},
	}).pushed(">=", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: x.intVal >= y.intVal,
			}
		},
	}).pushed("and", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			res := true
			for v.kind != KindNone {
				_, u := e.eval(v.first)
				res = res && u.asBool()
				v = v.second
			}
			return e, &Value{
				kind:    KindBool,
				boolVal: res,
			}
		},
	}).pushed("or", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			res := false
			for v.kind != KindNone {
				_, u := e.eval(v.first)
				res = res || u.asBool()
				v = v.second
			}
			return e, &Value{
				kind:    KindBool,
				boolVal: res,
			}
		},
	}).pushed("print", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, u := e.eval(v.first)
			fmt.Printf("%s\n", u)
			return e, &Value{kind: KindNone}
		},
	}).pushed("begin", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			res := &Value{kind: KindNone}
			for v.kind != KindNone {
				_, res = e.eval(v.first)
				v = v.second
			}
			return e, res
		},
	}).pushed("not", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, u := e.eval(v.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: !u.boolVal,
			}
		},
	}).pushed("car", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, u := e.eval(v.first)
			return e, u.first
		},
	}).pushed("cdr", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, u := e.eval(v.first)
			return e, u.second
		},
	}).pushed("cons", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, f := e.eval(v.first)
			_, s := e.eval(v.second.first)
			return e, &Value{
				kind:   KindPair,
				first:  f,
				second: s,
			}
		},
	}).pushed("eq?", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, x := e.eval(v.first)
			_, y := e.eval(v.second.first)
			return e, &Value{
				kind:    KindBool,
				boolVal: x.eq(y),
			}
		},
	}).pushed("define", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			funArgs, body := v.first, v.second
			if funArgs.kind == KindSymbol {
				_, u := e.eval(body.first)
				return e.pushed(funArgs.symbol, u), &Value{kind: KindNone}
			}
			e = e.pushed(funArgs.first.symbol, &Value{
				kind: KindFunc,
				function: func(e2 *Env, v *Value) (*Env, *Value) {
					ne := e
					for args := funArgs.second; v.kind != KindNone; {
						_, val := e2.eval(v.first)
						ne = ne.pushed(
							args.first.symbol,
							val,
						)
						args = args.second
						v = v.second
					}
					return e, ne.evalList(body)
				},
			})
			return e, &Value{kind: KindNone}
		},
	}).pushed("lambda", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			args, body := v.first, v.second
			return e, &Value{
				kind: KindFunc,
				function: func(e2 *Env, v *Value) (*Env, *Value) {
					ne := e
					a := args
					for v.kind != KindNone {
						_, val := e2.eval(v.first)
						ne = ne.pushed(
							a.first.symbol,
							val,
						)
						a = a.second
						v = v.second
					}
					return e2, ne.evalList(body)
				},
			}
		},
	}).pushed("let", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			args, body := v.first, v.second

			ne := e
			for args.kind != KindNone {
				kv := args.first
				_, val := e.eval(kv.second.first)
				ne = ne.pushed(
					kv.first.symbol,
					val,
				)
				args = args.second
			}
			return e, ne.evalList(body)
		},
	}).pushed("let*", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			args, body := v.first, v.second

			ne := e
			for args.kind != KindNone {
				kv := args.first
				_, val := ne.eval(kv.second.first)
				ne = ne.pushed(
					kv.first.symbol,
					val,
				)
				args = args.second
			}
			return e, ne.evalList(body)
		},
	}).pushed("letrec", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			args, body := v.first, v.second

			ne := e
			for args.kind != KindNone {
				kv := args.first
				ne = ne.pushed(
					kv.first.symbol,
					nil,
				)
				_, ne.value = ne.eval(kv.second.first)
				args = args.second
			}
			return e, ne.evalList(body)
		},
	}).pushed("if", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			cond, th, el := v.first, v.second.first, v.second.second.first

			_, b := e.eval(cond)
			if b.boolVal {
				return e.eval(th)
			} else {
				return e.eval(el)
			}
		},
	}).pushed("cond", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			for v.kind != KindNone {
				kv := v.first
				if _, b := e.eval(kv.first); b.boolVal {
					return e.eval(kv.second.first)
				}
				v = v.second
			}
			panic("cond has no else")
		},
	}).pushed("else", &Value{
		kind:    KindBool,
		boolVal: true,
	}).pushed("set!", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, y := e.eval(v.second.first)
			e.set(v.first.symbol, y)
			return e, &Value{kind: KindNone}
		},
	}).pushed("set-car!", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, pair := e.eval(v.first)
			_, u := e.eval(v.second.first)
			pair.first = u
			return e, &Value{kind: KindNone}
		},
	}).pushed("set-cdr!", &Value{
		kind: KindFunc,
		function: func(e *Env, v *Value) (*Env, *Value) {
			_, pair := e.eval(v.first)
			_, u := e.eval(v.second.first)
			pair.second = u
			return e, &Value{kind: KindNone}
		},
	})
}
