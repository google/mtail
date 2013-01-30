package main

import (
	"fmt"
	//"math/rand"
	"crypto/rand"
	"math/big"
	//"time"
)

type node struct {
	alts [][]string
	term string
}

var table = map[string]node{
	"start":     {[][]string{{"stmt_list"}}, ""},
	"stmt_list": {[][]string{{""}, {"stmt_list", "stmt"}}, ""},
	"stmt": {[][]string{{"cond", "{", "stmt_list", "}"},
		{"expr"},
		{"decl"},
		{"def_spec"},
		{"deco_spec"},
		{"next"},
		{"const", "ID", "pattern_expr"}}, ""},
	"expr": {[][]string{{"assign_expr"}}, ""},
	"assign_expr": {[][]string{{"rel_expr"}, {"unary_expr", "=", "rel_expr"},
		{"unary_expr", "+=", "rel_expr"}}, ""},
	"rel_expr": {[][]string{{"additive_expr"},
		{"additive_expr", "relop", "additive_expr"}}, ""},
	"relop":         {[][]string{{"<"}, {">"}, {"<="}, {">="}, {"=="}, {"!="}}, ""},
	"additive_expr": {[][]string{{"unary_expr"}, {"additive_expr", "+", "unary_expr"}, {"additive_expr", "-", "unary_expr"}}, ""},
	"unary_expr":    {[][]string{{"postfix_expr"}, {"BUILTIN", "(", ")"}, {"BUILTIN", "(", "arg_expr_list", ")"}}, ""},
	"arg_expr_list": {[][]string{{"assign_expr"}, {"arg_expr_list", ",", "assign_expr"}}, ""},
	"postfix_expr":  {[][]string{{"primary_expr"}, {"postfix_expr", "++"}, {"postfix_expr", "[", "expr", "]"}}, ""},
	"primary_expr":  {[][]string{{"ID"}, {"CAPREF"}, {"STRING"}, {"(", "expr", ")"}, {"NUMERIC"}}, ""},
	"cond":          {[][]string{{"pattern_expr"}, {"rel_expr"}}, ""},
	"pattern_expr":  {[][]string{{"REGEX"}, {"pattern_expr", "+", "REGEX"}, {"pattern_expr", "+", "ID"}}, ""},
	"decl":          {[][]string{{"hide_spec", "type_spec", "declarator"}}, ""},
	"hide_spec":     {[][]string{{""}, {"hidden"}}, ""},
	"declarator":    {[][]string{{"declarator", "by_spec"}, {"declarator", "as_spec"}, {"ID"}, {"STRING"}}, ""},
	"type_spec":     {[][]string{{"counter"}, {"gauge"}}, ""},
	"by_spec":       {[][]string{{"by", "by_expr_list"}}, ""},
	"by_expr_list":  {[][]string{{"ID"}, {"STRING"}, {"by_expr_list", ",", "ID"}, {"by_expr_list", ",", "STRING"}}, ""},
	"as_spec":       {[][]string{{"as", "STRING"}}, ""},
	"def_spec":      {[][]string{{"def", "ID", "{", "stmt_list", "}"}}, ""},
	"deco_spec":     {[][]string{{"deco", "{", "stmt_list", "}"}}, ""},

	"BUILTIN": {[][]string{{"strptime"}, {"timestamp"}, {"len"}, {"tolower"}}, ""},

	"CAPREF":  {[][]string{}, "$1"},
	"REGEX":   {[][]string{}, "/foo/"},
	"STRING":  {[][]string{}, "\"bar\""},
	"ID":      {[][]string{}, "quux"},
	"NUMERIC": {[][]string{}, "37"},
}

func main() {
	//rand.Seed(7)
	var states = []string{"start"}
	for len(states) > 0 {
		state := states[len(states)-1]
		states = states[:len(states)-1]
		//fmt.Println("state", state, "states", states)

		if n, ok := table[state]; ok {
			//fmt.Println("n", n)
			if len(n.alts) > 0 {
				//a := rand.Intn(len(n.alts))
				a1, _ := rand.Int(rand.Reader, big.NewInt(int64(len(n.alts))))
				a := a1.Int64()
				//fmt.Println("a", a, n.alts[a], len(n.alts[a]))
				for i := 0; i < len(n.alts[a]); i++ {
					//fmt.Println("i", i, n.alts[a][len(n.alts[a])-i-1])
					states = append(states, n.alts[a][len(n.alts[a])-i-1])
				}
				//fmt.Println("states", states)
			} else {
				//fmt.Println("(term)", state, n.term)
				fmt.Print(n.term, " ")
			}
		} else {
			//fmt.Println("(state)", state, state)
			fmt.Print(state, " ")
		}
	}
}
