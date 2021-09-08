package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

var code string

func main() {
	b, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	code = string(b)

	newEnv().evalList(list())
}

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

type Func func(e *Env, v *Value) (*Env, *Value)

type Value struct {
	kind     Kind
	first    *Value // Pair
	second   *Value
	symbol   string
	boolVal  bool
	intVal   int
	function Func
	quoted   *Value
}

func newNone() *Value            { return &Value{kind: KindNone} }
func newBool(x bool) *Value      { return &Value{kind: KindBool, boolVal: x} }
func newInt(x int) *Value        { return &Value{kind: KindInt, intVal: x} }
func newPair(x, y *Value) *Value { return &Value{kind: KindPair, first: x, second: y} }
func newSymbol(x string) *Value  { return &Value{kind: KindSymbol, symbol: x} }
func newFunc(f Func) *Value      { return &Value{kind: KindFunc, function: f} }
func newQuote(x *Value) *Value   { return &Value{kind: KindQuote, quoted: x} }

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
	}
	panic("BUG")
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
	switch c := next(); c {
	case '(', ')', '\'':
		return string(c)
	case '#':
		return "#" + string(next())
	default:
		s := string(c)
		for c = peek(); c != ')' && c != ' ' && c != '\n'; c = peek() {
			s += string(c)
			next()
		}
		return s
	}
}

func list() *Value {
	skip()
	if len(code) == 0 || peek() == ')' {
		return newNone()
	}
	return newPair(value(), list())
}

func value() *Value {
	t := token()
	if t[0] == '(' {
		res := list()
		next()
		return res
	} else if t[0] == '#' {
		return newBool(t[1] == 't')
	} else if t == "'" {
		return newQuote(value())
	} else if '0' <= t[0] && t[0] <= '9' || t[0] == '-' && len(t) > 1 {
		i, err := strconv.Atoi(t)
		if err != nil {
			panic(err)
		}
		return newInt(i)
	} else {
		return newSymbol(t)
	}
}

type Env struct {
	symbol string
	value  *Value
	next   *Env
}

func (e *Env) lookup(s string) *Value {
	if e == nil {
		panic("lookup: " + s)
	}
	if e.symbol == s {
		return e.value
	}
	return e.next.lookup(s)
}

func (e *Env) set(s string, v *Value) {
	if e == nil {
		panic("set: " + s)
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

func (e *Env) eval(v *Value) *Value {
	_, v = e.eval2(v)
	return v
}

func (e *Env) eval2(v *Value) (*Env, *Value) {
	switch v.kind {
	case KindNone, KindBool, KindInt:
		return e, v
	case KindQuote:
		return e, v.quoted
	case KindPair:
		return e.eval(v.first).function(e, v.second)
	case KindSymbol:
		return e, e.lookup(v.symbol)
	}
	panic("eval2: " + v.symbol)
}

func (e *Env) evalList(v *Value) *Value {
	e, res := e.eval2(v.first)
	if v.second.kind == KindNone {
		return res
	}
	return e.evalList(v.second)
}

func (e *Env) fold(v *Value, f func(x, y *Value) *Value) *Value {
	res := e.eval(v.first)
	if v.second.kind == KindNone {
		return res
	}
	return f(res, e.fold(v.second, f))
}

func symValIter(kvs *Value, f func(sym string, val *Value)) {
	for kvs.kind != KindNone {
		kv := kvs.first
		f(kv.first.symbol, kv.second.first)
		kvs = kvs.second
	}
}

func zipIter(xs, ys *Value, f func(x, y *Value)) {
	if xs.kind != KindNone {
		f(xs.first, ys.first)
		zipIter(xs.second, ys.second, f)
	}
}

func newEnv() *Env {
	var env *Env
	return env.pushed("-", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newInt(x.intVal - y.intVal)
	})).pushed("+", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		return e, e.fold(v, func(x, y *Value) *Value {
			return newInt(x.intVal + y.intVal)
		})
	})).pushed("*", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		return e, e.fold(v, func(x, y *Value) *Value {
			return newInt(x.intVal * y.intVal)
		})
	})).pushed("/", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newInt(x.intVal / y.intVal)
	})).pushed("=", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newBool(x.intVal == y.intVal)
	})).pushed("<", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newBool(x.intVal < y.intVal)
	})).pushed("<=", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newBool(x.intVal <= y.intVal)
	})).pushed(">", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newBool(x.intVal > y.intVal)
	})).pushed(">=", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newBool(x.intVal >= y.intVal)
	})).pushed("and", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		return e, e.fold(v, func(x, y *Value) *Value {
			return newBool(x.asBool() && y.asBool())
		})
	})).pushed("or", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		return e, e.fold(v, func(x, y *Value) *Value {
			return newBool(x.asBool() || y.asBool())
		})
	})).pushed("print", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		u := e.eval(v.first)
		fmt.Println(u)
		return e, newNone()
	})).pushed("begin", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		return e, e.evalList(v)
	})).pushed("not", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		u := e.eval(v.first)
		return e, newBool(!u.boolVal)
	})).pushed("car", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		u := e.eval(v.first)
		return e, u.first
	})).pushed("cdr", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		u := e.eval(v.first)
		return e, u.second
	})).pushed("cons", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		f := e.eval(v.first)
		s := e.eval(v.second.first)
		return e, newPair(f, s)
	})).pushed("eq?", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return e, newBool(x.eq(y))
	})).pushed("define", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		funArgs, body := v.first, v.second
		if funArgs.kind == KindSymbol {
			u := e.eval(body.first)
			return e.pushed(funArgs.symbol, u), newNone()
		}
		e = e.pushed(funArgs.first.symbol, newFunc(func(e2 *Env, v2 *Value) (*Env, *Value) {
			ne := e
			zipIter(funArgs.second, v2, func(x, y *Value) {
				val := e2.eval(y)
				ne = ne.pushed(x.symbol, val)
			})
			return e2, ne.evalList(body)
		}))
		return e, newNone()
	})).pushed("lambda", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		return e, newFunc(func(e2 *Env, v2 *Value) (*Env, *Value) {
			zipIter(v.first, v2, func(x, y *Value) {
				val := e2.eval(y)
				e = e.pushed(x.symbol, val)
			})
			return e2, e.evalList(v.second)
		})
	})).pushed("let", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		ne := e
		symValIter(v.first, func(sym string, val *Value) {
			val = e.eval(val)
			ne = ne.pushed(sym, val)
		})
		return e, ne.evalList(v.second)
	})).pushed("let*", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		ne := e
		symValIter(v.first, func(sym string, val *Value) {
			val = ne.eval(val)
			ne = ne.pushed(sym, val)
		})
		return e, ne.evalList(v.second)
	})).pushed("letrec", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		ne := e
		symValIter(v.first, func(sym string, val *Value) {
			ne.pushed(sym, nil)
			ne.value = ne.eval(val)
		})
		return e, ne.evalList(v.second)
	})).pushed("if", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		u := v.second.first
		if b := e.eval(v.first); !b.boolVal {
			u = v.second.second.first
		}
		u = e.eval(u)
		return e, u
	})).pushed("cond", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		for v.kind != KindNone {
			kv := v.first
			if b := e.eval(kv.first); b.boolVal {
				v = e.eval(kv.second.first)
				return e, v
			}
			v = v.second
		}
		panic("cond has no else")
	})).pushed("else", newBool(true)).pushed("set!", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		y := e.eval(v.second.first)
		e.set(v.first.symbol, y)
		return e, newNone()
	})).pushed("set-car!", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		pair := e.eval(v.first)
		u := e.eval(v.second.first)
		pair.first = u
		return e, newNone()
	})).pushed("set-cdr!", newFunc(func(e *Env, v *Value) (*Env, *Value) {
		pair := e.eval(v.first)
		u := e.eval(v.second.first)
		pair.second = u
		return e, newNone()
	}))
}
