package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
)

func main() {
	code, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	tokenize(string(code))

	defaultEnv().evalList(parseList())
}

func tokenize(code string) { // update tokens
	s := regexp.MustCompile(`([()'])`).ReplaceAllString(code, " $1 ")
	for _, x := range regexp.MustCompile(`\s+`).Split(s, -1) {
		if x != "" {
			tokens = append(tokens, x)
		}
	}
}

var tokens []string

func token() string {
	res := tokens[0]
	tokens = tokens[1:]
	return res
}

func parseList() *value {
	if len(tokens) == 0 || tokens[0] == ")" {
		return newNone()
	}
	return newPair(parseValue(), parseList())
}

func parseValue() *value {
	t := token()
	if t == "(" {
		res := parseList()
		token() // skip ")"
		return res
	} else if t[0] == '#' {
		return newBool(t[1] == 't')
	} else if t == "'" {
		return newQuote(parseValue())
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

type kind int

const (
	kindNone kind = iota
	kindBool
	kindInt
	kindPair
	kindSymbol
	kindFunc
	kindQuote
)

type value struct {
	kind    kind
	first   *value
	second  *value
	symbol  string
	boolVal bool
	intVal  int
	fun     func(*env, *value) *value
	quoted  *value
}

func newNone() *value                            { return &value{kind: kindNone} }
func newBool(x bool) *value                      { return &value{kind: kindBool, boolVal: x} }
func newInt(x int) *value                        { return &value{kind: kindInt, intVal: x} }
func newPair(x, y *value) *value                 { return &value{kind: kindPair, first: x, second: y} }
func newSymbol(x string) *value                  { return &value{kind: kindSymbol, symbol: x} }
func newFunc(f func(*env, *value) *value) *value { return &value{kind: kindFunc, fun: f} }
func newQuote(x *value) *value                   { return &value{kind: kindQuote, quoted: x} }

func (v *value) String() string {
	switch v.kind {
	case kindNone:
		return "()"
	case kindBool:
		if v.boolVal {
			return "#t"
		}
		return "#f"
	case kindInt:
		return strconv.Itoa(v.intVal)
	case kindPair:
		return fmt.Sprintf("( %s . %s )", v.first, v.second)
	case kindSymbol:
		return v.symbol
	case kindQuote:
		return "'" + v.quoted.String()
	case kindFunc:
		return "<func>"
	}
	panic("BUG")
}

func (v *value) asBool() bool {
	switch v.kind {
	case kindBool:
		return v.boolVal
	}
	return true
}

func (v *value) eq(u *value) bool {
	if v.kind != u.kind {
		return false
	}
	switch v.kind {
	case kindNone:
		return true
	case kindBool:
		return v.boolVal == u.boolVal
	case kindInt:
		return v.intVal == u.intVal
	case kindQuote:
		return v.quoted.eq(u.quoted)
	case kindSymbol:
		return v.symbol == u.symbol
	}
	return false
}

type env struct {
	m    map[string]*value
	next *env
}

func (e *env) lookup(s string) *value {
	if v, ok := e.m[s]; ok {
		return v
	}
	return e.next.lookup(s)
}

func (e *env) set(s string, v *value) {
	if _, ok := e.m[s]; ok {
		e.m[s] = v
		return
	}
	e.next.set(s, v)
}

// ensure ensures the value within frame, and returns e for convenience.
func (e *env) ensure(s string, v *value) *env {
	e.m[s] = v
	return e
}

func (e *env) newFrame() *env {
	return &env{
		m:    make(map[string]*value),
		next: e,
	}
}

func (e *env) withOp1(symbol string, f func(x *value) *value) *env {
	return e.ensure(symbol, newFunc(func(e *env, v *value) *value {
		x := e.eval(v.first)
		return f(x)
	}))
}

func (e *env) withOp2(symbol string, f func(x, y *value) *value) *env {
	return e.ensure(symbol, newFunc(func(e *env, v *value) *value {
		x := e.eval(v.first)
		y := e.eval(v.second.first)
		return f(x, y)
	}))
}

func (e *env) fold(v *value, f func(x, y *value) *value) *value {
	res := e.eval(v.first)
	if v.second.kind == kindNone {
		return res
	}
	return f(res, e.fold(v.second, f))
}

func (e *env) withFoldop(symbol string, f func(x, y *value) *value) *env {
	return e.ensure(symbol, newFunc(func(e *env, v *value) *value {
		return e.fold(v, f)
	}))
}

func (e *env) eval(v *value) *value {
	switch v.kind {
	case kindNone, kindBool, kindInt:
		return v
	case kindQuote:
		return v.quoted
	case kindPair:
		return e.eval(v.first).fun(e, v.second)
	case kindSymbol:
		return e.lookup(v.symbol)
	}
	panic("eval: " + v.symbol)
}

func (e *env) evalList(v *value) *value {
	res := e.eval(v.first)
	if v.second.kind == kindNone {
		return res
	}
	return e.evalList(v.second)
}

func symValIter(kvs *value, f func(sym string, val *value)) {
	for kvs.kind != kindNone {
		kv := kvs.first
		f(kv.first.symbol, kv.second.first)
		kvs = kvs.second
	}
}

func zipIter(xs, ys *value, f func(x, y *value)) {
	if xs.kind != kindNone {
		f(xs.first, ys.first)
		zipIter(xs.second, ys.second, f)
	}
}

func defaultEnv() *env {
	return ((*env)(nil)).newFrame().
		withFoldop("+", func(x, y *value) *value { return newInt(x.intVal + y.intVal) }).
		withFoldop("*", func(x, y *value) *value { return newInt(x.intVal * y.intVal) }).
		withOp2("-", func(x, y *value) *value { return newInt(x.intVal - y.intVal) }).
		withOp2("/", func(x, y *value) *value { return newInt(x.intVal / y.intVal) }).
		withOp2("=", func(x, y *value) *value { return newBool(x.intVal == y.intVal) }).
		withOp2("<", func(x, y *value) *value { return newBool(x.intVal < y.intVal) }).
		withOp2("<=", func(x, y *value) *value { return newBool(x.intVal <= y.intVal) }).
		withOp2(">", func(x, y *value) *value { return newBool(x.intVal > y.intVal) }).
		withOp2(">=", func(x, y *value) *value { return newBool(x.intVal >= y.intVal) }).
		withFoldop("and", func(x, y *value) *value { return newBool(x.asBool() && y.asBool()) }).
		withFoldop("or", func(x, y *value) *value { return newBool(x.asBool() || y.asBool()) }).
		withOp1("not", func(v *value) *value { return newBool(!v.boolVal) }).
		withOp1("car", func(v *value) *value { return v.first }).
		withOp1("cdr", func(v *value) *value { return v.second }).
		withOp2("cons", func(x, y *value) *value { return newPair(x, y) }).
		withOp2("eq?", func(x, y *value) *value { return newBool(x.eq(y)) }).
		withOp1("print", func(x *value) *value { fmt.Println(x); return newNone() }).
		ensure("begin", newFunc(func(e *env, v *value) *value { return e.evalList(v) })).
		ensure("define", newFunc(func(e *env, v *value) *value {
			funArgs, body := v.first, v.second
			var name, val *value
			if funArgs.kind == kindSymbol {
				name = funArgs
				val = body.first
			} else {
				name = funArgs.first
				val = newPair(newSymbol("lambda"), newPair(funArgs.second, body))
			}
			e.ensure(name.symbol, e.eval(val))
			return newNone()
		})).
		ensure("lambda", newFunc(func(e *env, v *value) *value {
			return newFunc(func(e2 *env, v2 *value) *value {
				e := e.newFrame()
				zipIter(v.first, v2, func(x, y *value) {
					e.ensure(x.symbol, e2.eval(y))
				})
				return e.evalList(v.second)
			})
		})).
		ensure("let", newFunc(func(e *env, v *value) *value {
			e2 := e.newFrame()
			symValIter(v.first, func(sym string, val *value) {
				e2.ensure(sym, e.eval(val))
			})
			return e2.evalList(v.second)
		})).
		ensure("let*", newFunc(func(e *env, v *value) *value {
			symValIter(v.first, func(sym string, val *value) {
				e = e.newFrame().ensure(sym, e.eval(val))
			})
			return e.evalList(v.second)
		})).
		ensure("letrec", newFunc(func(e *env, v *value) *value {
			e = e.newFrame()
			symValIter(v.first, func(sym string, val *value) {
				e.ensure(sym, nil)
				e.ensure(sym, e.eval(val))
			})
			return e.evalList(v.second)
		})).
		ensure("if", newFunc(func(e *env, v *value) *value {
			u := v.second.first
			if !e.eval(v.first).boolVal {
				u = v.second.second.first
			}
			return e.eval(u)
		})).
		ensure("cond", newFunc(func(e *env, v *value) *value {
			for {
				f := v.first
				if e.eval(f.first).boolVal {
					return e.eval(f.second.first)
				}
				v = v.second
			}
		})).
		ensure("else", newBool(true)).
		ensure("set!", newFunc(func(e *env, v *value) *value {
			e.set(v.first.symbol, e.eval(v.second.first))
			return newNone()
		})).
		ensure("set-car!", newFunc(func(e *env, v *value) *value {
			e.eval(v.first).first = e.eval(v.second.first)
			return newNone()
		})).
		ensure("set-cdr!", newFunc(func(e *env, v *value) *value {
			e.eval(v.first).second = e.eval(v.second.first)
			return newNone()
		}))
}
