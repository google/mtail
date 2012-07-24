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
%type <n> expr primary_expr additive_expr postfix_expr unary_expr assign_expr
%type <n> decl declarator
%type <mtype> type_spec
%type <text> as_spec
%type <texts> by_spec by_expr_list
%type <flag> hide_spec
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Reserved words
%token COUNTER GAUGE AS BY HIDDEN
// Builtins
%token <text> BUILTIN
// Literals: re2 syntax regular expression, quoted strings, regex capture group
// references, identifiers, and numerical constants
%token <text> REGEX
%token <text> STRING
%token <text> CAPREF
%token <text> ID
%token <value> CONST
// Operators
%token INC MINUS PLUS
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
      $$.(*stmtlistNode).children = append($$.(*stmtlistNode).children, $2)
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
  ;


expr
  : assign_expr
  {
    $$ = $1
  }
  ;

assign_expr
  : additive_expr
  {
     $$ = $1
  }
  | unary_expr ASSIGN assign_expr
  {
    $$ = &assignExprNode{$1, $3}
  }
  | unary_expr ADD_ASSIGN assign_expr
  {
    $$ = &incByExprNode{$1, $3}
  }
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
    if sym, ok := Emtaillex.(*parser).s.lookupSym($1); ok {
      $$ = &idNode{$1, sym}
    } else {
      Emtaillex.Error(fmt.Sprintf("Identifier %s not declared.", $1))
    }
  }
  | CAPREF
  {
      if sym, ok := Emtaillex.(*parser).s.lookupSym($1); ok {
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
  | CONST
  {
    $$ = &constExprNode{$1}
  }
  ;


cond
  : REGEX
  {
      if re, err := regexp.Compile($1); err != nil {
          Emtaillex.(*parser).Error(fmt.Sprintf(err.Error()))
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
            sym.addr = i
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
    if len(d.keys) > 0 {
      d.m = &Metric{Name: n, Kind: d.kind,
                    Keys:   d.keys,
                    hidden: $1,
                    Values: make(map[string]*Datum, 0)}
      d.sym = Emtaillex.(*parser).s.addSym(d.name, DimensionedMetricSymbol, d.m,
                                           Emtaillex.(*parser).pos)
    } else {
      d.m = &Metric{Name: n, Kind: d.kind,
                    hidden: $1,
                    D: &Datum{}}
      d.sym = Emtaillex.(*parser).s.addSym(d.name, ScalarMetricSymbol, d.m,
                                           Emtaillex.(*parser).pos)
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

%%

const EOF = 0

type parser struct {
    root   node
    errors []string
    l      *lexer
    pos    Position
    s      *scope
}

func NewParser(name string, input io.Reader) *parser {
    return &parser{l: NewLexer(name, input)}
}

func (p *parser) Error(s string) {
    e := fmt.Sprintf("%s:%s: %s", p.l.name, p.pos, s)
    p.errors = append(p.errors, e)
}

func (p *parser) Lex(lval *EmtailSymType) int {
    token := p.l.NextToken()
    p.pos = token.pos
    if token.kind == INVALID {
    	p.Error(token.text)
        return EOF
    }
    lval.text = token.text
    return int(token.kind)
}

func (p *parser) startScope() {
    s := &scope{p.s, map[string]*symbol{}}
    p.s = s
}

func (p *parser) endScope() {
    if p.s != nil && p.s.parent != nil {
        p.s = p.s.parent
    }
}
