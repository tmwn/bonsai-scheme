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
	code = string(b)

	v := value()
	(&Env{}).eval(v)
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
	function func(v *Value) *Value
	quoted   *Value
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
}

func (e *Env) eval(v *Value) *Value {
	switch v.kind {
	case KindNone:
		return v
	case KindBool:
		return v
	case KindInt:
		return v
	case KindQuote:
		return v.quoted
	case KindPair:
		return e.eval(v.first).function(v.second)
	case KindSymbol:
		if v.symbol == "print" {
			return &Value{
				kind: KindFunc,
				function: func(v *Value) *Value {
					fmt.Printf("%s\n", e.eval(v.first))
					return &Value{kind: KindNone}
				},
			}
		}
		if v.symbol == "begin" {
			return &Value{
				kind: KindFunc,
				function: func(v *Value) *Value {
					res := &Value{kind: KindNone}
					for v.kind != KindNone {
						res = e.eval(v.first)
						v = v.second
					}
					return res
				},
			}
		}
	}
	panic(v.symbol)
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
	case KindPair:
		return fmt.Sprintf("( %s . %s )", v.first, v.second)
	case KindSymbol:
		return v.symbol
	default:
		panic("BUG")
	}
}
