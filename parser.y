// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

%{
/*
emtail is a metric exporting, modular tail.

This file describes a parser for the emtail language.
*/
package main

import (
    "io"
    "fmt"
    "regexp"
    "sort"
    "strconv"
)

%}

%union
{
    value int
    text string
    texts []string
    flag bool
    n node
    mtype metric_type
}

%type <n> stmt_list stmt cond arg_expr_list
%type <n> expr primary_expr additive_expr postfix_expr unary_expr assign_expr rel_expr
%type <n> decl declarator def deco
%type <mtype> type_spec
%type <text> as_spec
%type <texts> by_spec by_expr_list
%type <flag> hide_spec
%type <value> relop
%type <text> pattern_expr
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Types
%token COUNTER GAUGE
// Reserved words
%token AS BY CONST HIDDEN DEF NEXT
// Builtins
%token <text> BUILTIN
// Literals: re2 syntax regular expression, quoted strings, regex capture group
// references, identifiers, decorators, and numerical constants.
%token <text> REGEX
%token <text> STRING
%token <text> CAPREF
%token <text> ID
%token <text> DECO
%token <value> NUMERIC
// Operators, in order of precedence
%token INC MINUS PLUS
%token <value> LT GT LE GE EQ NE
%token ADD_ASSIGN ASSIGN
// Punctuation
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token COMMA

%start start
%%

start
  : stmt_list
  {
      $1.(*stmtlistNode).s = Emtaillex.(*parser).s
      Emtaillex.(*parser).endScope()
      Emtaillex.(*parser).root = $1
  }
  ;

stmt_list
  : /* empty */
  {  
      $$ = &stmtlistNode{}
      Emtaillex.(*parser).startScope()
  }
  | stmt_list stmt
  {
      $$ = $1
      if ($2 != nil) {
        $$.(*stmtlistNode).children = append($$.(*stmtlistNode).children, $2)
      }
  }
  ;

stmt
  : cond LCURLY stmt_list RCURLY
  {
      $3.(*stmtlistNode).s = Emtaillex.(*parser).s
      Emtaillex.(*parser).endScope()
      if $1 != nil {
          $$ = &condNode{$1, []node{$3}}
      } else {
          $$ = $3
      }
  }
  | expr
  {
    $$ = $1
  }
  | decl
  {
    $$ = $1
  }
  | def
  {
    $$ = $1
  }
  | deco
  {
    $$ = $1
  }
  | NEXT
  {
    $$ = &nextNode{}
  }
  | CONST ID pattern_expr
  {
    // Store the regex for concatenation
    Emtaillex.(*parser).res[$2] = $3
  }
  ;

expr
  : assign_expr
  {
    $$ = $1
  }
  ;

assign_expr
  : rel_expr
  {
     $$ = $1
  }
  | unary_expr ASSIGN rel_expr
  {
    $$ = &assignExprNode{$1, $3}
  }
  | unary_expr ADD_ASSIGN rel_expr
  {
    $$ = &incByExprNode{$1, $3}
  }
  ;

rel_expr
  : additive_expr
  {
    $$ = $1
  }
  | additive_expr relop additive_expr
  {
    $$ = &relNode{$1, $3, $2}
  }
  ;

relop
  : LT
  { $$ = $1 }
  | GT
  { $$ = $1 }
  | LE
  { $$ = $1 }
  | GE
  { $$ = $1 }
  | EQ
  { $$ = $1 }
  | NE
  { $$ = $1 }
  ;
  
additive_expr
  : unary_expr
  {
    $$ = $1
  }
  | additive_expr PLUS unary_expr
  {
    $$ = &additiveExprNode{$1, $3, '+'}
  }
  | additive_expr MINUS unary_expr
  {
    $$ = &additiveExprNode{$1, $3, '-'}
  }
  ;

unary_expr
  : postfix_expr
  {
    $$ = $1
  }
  | BUILTIN LPAREN RPAREN
  {
    $$ = &builtinNode{name: $1}
  }
  | BUILTIN LPAREN arg_expr_list RPAREN
  {
    $$ = &builtinNode{$1, $3}
  }
  ;

arg_expr_list
  : assign_expr
  {
    $$ = &exprlistNode{}
    $$.(*exprlistNode).children = append($$.(*exprlistNode).children, $1)
  }
  | arg_expr_list COMMA assign_expr
  {
     $$ = $1
     $$.(*exprlistNode).children = append($$.(*exprlistNode).children, $3)
  }
  ;

postfix_expr
  : primary_expr
  {
    $$ = $1
  }
  | postfix_expr INC
  {
    $$ = &incExprNode{$1}
  }
  | postfix_expr LSQUARE expr RSQUARE
  {
    $$ = &indexedExprNode{$1, $3}
  }
  ;

primary_expr
  : ID
  {
    if sym, ok := Emtaillex.(*parser).s.lookupSym($1, IdSymbol); ok {
      $$ = &idNode{$1, sym}
    } else {
      Emtaillex.Error(fmt.Sprintf("Identifier '%s' not declared.", $1))
    }
  }
  | CAPREF
  {
    if sym, ok := Emtaillex.(*parser).s.lookupSym($1, CaprefSymbol); ok {
      $$ = &caprefNode{$1, sym}
    } else {
      Emtaillex.Error(fmt.Sprintf("Capture group $%s not defined " +
                                  "by prior regular expression in " +
                                  "this or an outer scope",  $1))
      // TODO(jaq) force a parse error
    }
  }
  | STRING
  {
    $$ = &stringNode{$1}
  }
  | LPAREN expr RPAREN
  {
    $$ = $2
  }
  | NUMERIC
  {
    $$ = &numericExprNode{$1}
  }
  ;


cond
  : pattern_expr
  {
    if re, err := regexp.Compile($1); err != nil {
      Emtaillex.(*parser).ErrorP(fmt.Sprintf(err.Error()), Emtaillex.(*parser).pos)
      // TODO(jaq): force a parse error
    } else {
      $$ = &regexNode{pattern: $1}
      // We can reserve storage for these capturing groups, storing them in
      // the current scope, so that future CAPTUREGROUPs can retrieve their
      // value.  At parse time, we can warn about nonexistent names.
      for i := 1; i < re.NumSubexp() + 1; i++ {
        sym := Emtaillex.(*parser).s.addSym(fmt.Sprintf("%d", i),
                                            CaprefSymbol, $$,
                                            Emtaillex.(*parser).pos)
        sym.addr = i - 1
      }
      for i, capref := range re.SubexpNames() {
        if capref != "" {
          sym := Emtaillex.(*parser).s.addSym(capref, CaprefSymbol, $$,
                                              Emtaillex.(*parser).pos)
          sym.addr = i
        }
      }
    }
  }
  | rel_expr
  {
    $$ = $1
  }
  ;

pattern_expr
  : REGEX
  {
    // Stash the start of the pattern_expr in a state variable.
    // We know it's the start because pattern_expr is left associative.
    Emtaillex.(*parser).pos = Emtaillex.(*parser).t.pos
    $$ = $1
  }
  | pattern_expr PLUS REGEX
  {
    $$ = $1 + $3
  }
  | pattern_expr PLUS ID
  {
    if s, ok := Emtaillex.(*parser).res[$3]; ok {
      $$ = $1 + s
    } else {
      Emtaillex.Error(fmt.Sprintf("Constant '%s' not defined.", $3))
    }
  }
  ;


decl
  : hide_spec type_spec declarator
  {
    $$ = $3
    d := $$.(*declNode)
    d.kind = $2

    var n string
    if d.exported_name != "" {
        n = d.exported_name
	} else {
        n = d.name
   	}
      sort.Sort(sort.StringSlice(d.keys))
      d.m = NewMetric(n, Emtaillex.(*parser).name, d.kind, d.keys...)
      d.sym = Emtaillex.(*parser).s.addSym(d.name, IdSymbol, d.m,
                                           Emtaillex.(*parser).t.pos)
      if !$1 {
        ExportMetric(d.m)
      }
  }
  ;

hide_spec
  : /* empty */
  {
    $$ = false
  }
  | HIDDEN
  {
    $$ = true
  }
  ;

declarator
  : declarator by_spec
  {
    $$ = $1
    $$.(*declNode).keys = $2
  }
  | declarator as_spec
  {
    $$ = $1
    $$.(*declNode).exported_name = $2
  }
  | ID
  {
    $$ = &declNode{name: $1}

  }
  | STRING
  {
    $$ = &declNode{name: $1}
  }
  ;

type_spec
  : COUNTER
  {
    $$ = Counter
  }
  | GAUGE
  {
    $$ = Gauge
  }
  ;

by_spec
  : BY by_expr_list
  {
    $$ = $2
  }
  ;

by_expr_list
  : ID
  {
    $$ = make([]string, 0)
    $$ = append($$, $1)
  }
  | STRING
  {
    $$ = make([]string, 0)
    $$ = append($$, $1)
  }
  | by_expr_list COMMA ID
  {
    $$ = $1
    $$ = append($$, $3)
  }
  | by_expr_list COMMA STRING
  {
    $$ = $1
    $$ = append($$, $3)
  }
  ;

as_spec
  : AS STRING
  {
    $$ = $2
  }
  ;

def
  : DEF ID LCURLY stmt_list RCURLY
  {
      $4.(*stmtlistNode).s = Emtaillex.(*parser).s
      Emtaillex.(*parser).endScope()
      $$ = &defNode{name: $2, children: []node{$4}}
      d := $$.(*defNode)
      d.sym = Emtaillex.(*parser).s.addSym(d.name, DefSymbol, d, Emtaillex.(*parser).t.pos)
  }
  ;

deco
  : DECO LCURLY stmt_list RCURLY
  {
    $3.(*stmtlistNode).s = Emtaillex.(*parser).s
    Emtaillex.(*parser).endScope()
    if sym, ok := Emtaillex.(*parser).s.lookupSym($1, DefSymbol); ok {
      $$ = &decoNode{$1, []node{$3}, sym.binding.(*defNode)}
    } else {
      Emtaillex.Error(fmt.Sprintf("Decorator %s not defined", $1))
      // TODO(jaq): force a parse error.
    }
  }
  ;

%%

const EOF = 0

type parser struct {
    name   string
    root   node
    errors []string
    l      *lexer
    t      Token             // Most recently lexed token.
    pos Position             // Maybe contains the position of the start of a node.
    s      *scope
    res    map[string]string // Mapping of regex constants to patterns
}

func NewParser(name string, input io.Reader) *parser {
    return &parser{name: name, l: NewLexer(name, input), res: make(map[string]string)}
}

func (p *parser) ErrorP(s string, pos Position) {
    e := fmt.Sprintf("%s:%s: %s", p.l.name, pos, s)
    p.errors = append(p.errors, e)
}

func (p *parser) Error(s string) {
    p.ErrorP(s, p.t.pos)
}

func (p *parser) Lex(lval *EmtailSymType) int {
    p.t = p.l.NextToken()
    switch p.t.kind {
    case INVALID:
        p.Error(p.t.text)
        return EOF
    case NUMERIC:
        var err error
        lval.value, err = strconv.Atoi(p.t.text)
        if err != nil {
            p.Error(fmt.Sprintf("bad number '%s': %s", p.t.text, err))
            return INVALID
        }
    case LT, GT, LE, GE, NE, EQ:
        lval.value = int(p.t.kind)
    default:
        lval.text = p.t.text
    }
    return int(p.t.kind)
}

func (p *parser) startScope() {
    s := &scope{p.s, map[string][]*symbol{}}
    p.s = s
}

func (p *parser) endScope() {
    if p.s != nil && p.s.parent != nil {
        p.s = p.s.parent
    }
}
