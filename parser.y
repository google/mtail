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
    text string
    n node
    mtype metric_type
}


%type <n> stmt_list stmt cond arg_expr_list
%type <n> expr primary_expr additive_expr postfix_expr unary_expr assign_expr
%type <n> decl declarator
%type <mtype> type_spec
%type <text> as_spec
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Reserved words
%token EXPORT COUNTER GAUGE AS
// Builtins
%token <text> BUILTIN
// Literals: re2 syntax regular expression, quoted strings, regex capture group
// references, and identifiers
%token <text> REGEX
%token <text> STRING
%token <text> CAPREF
%token <text> ID
// Punctuation
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token COMMA
%token MINUS PLUS ASSIGN


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
  | postfix_expr LSQUARE expr RSQUARE
  {
    $$ = &indexedExprNode{$1, $3}
  }
  ; 

primary_expr
  : ID
  {
      $$ = &idNode{$1}
  }
  | CAPREF
  {
      if index, ok := Emtaillex.(*parser).s.lookupSym($1); ok {
          $$ = &caprefNode{$1, index}
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
  ;


cond
  : REGEX
  {
      /* Emtaillex.(*parser).startScope() */
      if re, err := regexp.Compile($1); err != nil {
          Emtaillex.(*parser).Error(fmt.Sprintf(err.Error()))
          // TODO(jaq): force a parse error
      } else {
          // We can reserve storage for these capturing groups, storing them in
          // the current scope, so that future CAPTUREGROUPs can retrieve their
          // value.  At parse time, we can warn about nonexistent names.
          for i := 1; i < re.NumSubexp() + 1; i++ {
              Emtaillex.(*parser).s.addSym(fmt.Sprintf("%d", i), i)
          }
          for i, capref := range re.SubexpNames() {
                if capref != "" {
                  Emtaillex.(*parser).s.addSym(capref, i)
              }
          }
          $$ = &regexNode{$1}
      }
  }
  ;

decl
  : type_spec declarator
  {
    $$ = $2
    $$.(*declNode).kind = $1
  }
  ;

declarator
  : declarator as_spec
  {
    $$ = $1
    $$.(*declNode).exported_name = $2
  }
  | ID
  {
    $$ = &declNode{name: $1, exported: true}

  }
  | STRING
  {
    $$ = &declNode{name: $1, exported: true}
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
    s := &scope{p.s, map[string]int{}}
    p.s = s
}

func (p *parser) endScope() {
    if p.s != nil && p.s.parent != nil {
        p.s = p.s.parent
    }
}
