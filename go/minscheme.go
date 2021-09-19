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

type Func func(e *Env, v *Value) *Value

type Value struct {
	kind    Kind
	first   *Value // Pair
	second  *Value
	symbol  string
	boolVal bool
	intVal  int
	fun     Func
	quoted  *Value
}

func newNone() *Value            { return &Value{kind: KindNone} }
func newBool(x bool) *Value      { return &Value{kind: KindBool, boolVal: x} }
func newInt(x int) *Value        { return &Value{kind: KindInt, intVal: x} }
func newPair(x, y *Value) *Value { return &Value{kind: KindPair, first: x, second: y} }
func newSymbol(x string) *Value  { return &Value{kind: KindSymbol, symbol: x} }
func newFunc(f Func) *Value      { return &Value{kind: KindFunc, fun: f} }
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
	case KindQuote:
		return "'" + v.quoted.String()
	case KindFunc:
		return "<func>"
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
	symbol    string
	value     *Value
	next      *Env
	nextFrame *Env
}

func (e *Env) lookup(s string) *Value {
	if e == nil {
		panic("lookup: " + s)
	}
	if e.symbol == s {
		return e.value
	}
	if e.nextFrame != nil {
		return e.nextFrame.lookup(s)
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
	if e.nextFrame != nil {
		e.nextFrame.set(s, v)
		return
	}
	e.next.set(s, v)
}

func (e *Env) ensure(s string, v *Value) { // ensure the value within frame
	if e == nil {
		panic("ensure: " + s)
	}
	if e.symbol == s {
		e.value = v
		return
	}
	if e.next != nil {
		e.next.ensure(s, v)
		return
	}
	e.next = &Env{
		symbol:    s,
		value:     v,
		nextFrame: e.nextFrame,
	}
	e.nextFrame = nil
}

func (e *Env) newFrame() *Env {
	return &Env{
		nextFrame: e,
	}
}

func (e *Env) with(symbol string, value *Value) *Env {
	return &Env{
		symbol: symbol,
		value:  value,
		next:   e,
	}
}

func (e *Env) withOp1(symbol string, f func(x *Value) *Value) *Env {
	return e.with(symbol, newFunc(func(e *Env, v *Value) *Value {
		x := e.eval(v.first)
		return f(x)
	}))
}

func (e *Env) withOp2(symbol string, f func(x, y *Value) *Value) *Env {
	return e.with(symbol, newFunc(func(e *Env, v *Value) *Value {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return f(x, y)
	}))
}

func (e *Env) fold(v *Value, f func(x, y *Value) *Value) *Value {
	res := e.eval(v.first)
	if v.second.kind == KindNone {
		return res
	}
	return f(res, e.fold(v.second, f))
}

func (e *Env) withFoldop(symbol string, f func(x, y *Value) *Value) *Env {
	return e.with(symbol, newFunc(func(e *Env, v *Value) *Value {
		return e.fold(v, f)
	}))
}

func (e *Env) eval(v *Value) *Value {
	if e == nil {
		panic("eval: e = nil")
	}
	switch v.kind {
	case KindNone, KindBool, KindInt:
		return v
	case KindQuote:
		return v.quoted
	case KindPair:
		return e.eval(v.first).fun(e, v.second)
	case KindSymbol:
		return e.lookup(v.symbol)
	}
	panic("eval: " + v.symbol)
}

func (e *Env) evalList(v *Value) *Value {
	res := e.eval(v.first)
	if v.second.kind == KindNone {
		return res
	}
	return e.evalList(v.second)
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
	return env.
		withFoldop("+", func(x, y *Value) *Value { return newInt(x.intVal + y.intVal) }).
		withFoldop("*", func(x, y *Value) *Value { return newInt(x.intVal * y.intVal) }).
		withOp2("-", func(x, y *Value) *Value { return newInt(x.intVal - y.intVal) }).
		withOp2("/", func(x, y *Value) *Value { return newInt(x.intVal / y.intVal) }).
		withOp2("=", func(x, y *Value) *Value { return newBool(x.intVal == y.intVal) }).
		withOp2("<", func(x, y *Value) *Value { return newBool(x.intVal < y.intVal) }).
		withOp2("<=", func(x, y *Value) *Value { return newBool(x.intVal <= y.intVal) }).
		withOp2(">", func(x, y *Value) *Value { return newBool(x.intVal > y.intVal) }).
		withOp2(">=", func(x, y *Value) *Value { return newBool(x.intVal >= y.intVal) }).
		withFoldop("and", func(x, y *Value) *Value { return newBool(x.asBool() && y.asBool()) }).
		withFoldop("or", func(x, y *Value) *Value { return newBool(x.asBool() || y.asBool()) }).
		withOp1("not", func(v *Value) *Value { return newBool(!v.boolVal) }).
		withOp1("car", func(v *Value) *Value { return v.first }).
		withOp1("cdr", func(v *Value) *Value { return v.second }).
		withOp2("cons", func(x, y *Value) *Value { return newPair(x, y) }).
		withOp2("eq?", func(x, y *Value) *Value { return newBool(x.eq(y)) }).
		withOp1("print", func(x *Value) *Value { fmt.Println(x); return newNone() }).
		with("begin", newFunc(func(e *Env, v *Value) *Value { return e.evalList(v) })).
		with("define", newFunc(func(e *Env, v *Value) *Value {
			funArgs, body := v.first, v.second
			var name, value *Value
			if funArgs.kind == KindSymbol {
				name = funArgs
				value = body.first
			} else {
				name = funArgs.first
				value = newPair(
					newSymbol("lambda"),
					newPair(
						funArgs.second,
						body,
					),
				)
			}
			e.ensure(name.symbol, e.eval(value))
			return newNone()
		})).
		with("lambda", newFunc(func(e *Env, v *Value) *Value {
			return newFunc(func(e2 *Env, v2 *Value) *Value {
				ne := e.newFrame()
				zipIter(v.first, v2, func(x, y *Value) {
					val := e2.eval(y)
					ne = ne.with(x.symbol, val)
				})
				return ne.evalList(v.second)
			})
		})).
		with("let", newFunc(func(e *Env, v *Value) *Value {
			e2 := e
			symValIter(v.first, func(sym string, val *Value) {
				e = e.with(sym, e2.eval(val))
			})
			return e.evalList(v.second)
		})).
		with("let*", newFunc(func(e *Env, v *Value) *Value {
			symValIter(v.first, func(sym string, val *Value) {
				e = e.with(sym, e.eval(val))
			})
			return e.evalList(v.second)
		})).
		with("letrec", newFunc(func(e *Env, v *Value) *Value {
			symValIter(v.first, func(sym string, val *Value) {
				e = e.with(sym, nil)
				e.value = e.eval(val)
			})
			return e.evalList(v.second)
		})).
		with("if", newFunc(func(e *Env, v *Value) *Value {
			u := v.second.first
			if !e.eval(v.first).boolVal {
				u = v.second.second.first
			}
			return e.eval(u)
		})).
		with("cond", newFunc(func(e *Env, v *Value) *Value {
			for {
				f := v.first
				if e.eval(f.first).boolVal {
					return e.eval(f.second.first)
				}
				v = v.second
			}
		})).
		with("else", newBool(true)).
		with("set!", newFunc(func(e *Env, v *Value) *Value {
			e.set(v.first.symbol, e.eval(v.second.first))
			return newNone()
		})).
		with("set-car!", newFunc(func(e *Env, v *Value) *Value {
			e.eval(v.first).first = e.eval(v.second.first)
			return newNone()
		})).
		with("set-cdr!", newFunc(func(e *Env, v *Value) *Value {
			e.eval(v.first).second = e.eval(v.second.first)
			return newNone()
		}))
}
