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
}


%type <n> stmt_list stmt cond expr_list expr opt_expr_list

// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Builtins
%token <text> BUILTIN
// Literals: re2 syntax regular expression, quoted strings, regex capture group
// references, and identifiers
%token <text> REGEX
%token <text> STRING
%token <text> CAPREF
%token <text> ID
// Punctuation
%token LCURLY RCURLY LPAREN RPAREN
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

/*
    // //parser.StartNewScope()
    // } stmt_list '}'
      /*
       * $4->set_scope(parser->end_scope());
*/

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
  | BUILTIN LPAREN opt_expr_list RPAREN
  {
  	$$ = &builtinNode{$1, []node{$3}}
  }
  ;

opt_expr_list
  : /* empty */
  {
	$$ = &exprlistNode{}
  }
  | expr_list
  {
	$$ = $1
  }
  ;

expr_list
  : expr
  {
	$$ = &exprlistNode{[]node{$1}}
  }
  | expr_list COMMA expr
  {
	$$ = $1
	$$.(*exprlistNode).children = append($$.(*exprlistNode).children, $3)
  }
  ;

expr
  : ID
  {
    $$ = idNode{$1}
  }
  | CAPREF
  {
	if Emtaillex.(*parser).s.lookupSym($1) {
		$$ = &caprefNode{$1}
	} else {
      Emtaillex.Error(fmt.Sprintf("Capture group $%s not defined by prior regular expression in " +
                                  "this or outer scopes",  $1))
        // TODO(jaq) force a parse error
		Nerrs++
		$$ = nil
	}
  }
  | STRING
  {
	$$ = &stringNode{$1}
  }
  ;

cond
  : /* empty */
  {
    /* Emtaillex.(*parser).startScope() */
    $$ = nil
  }
  | REGEX
  {
    /* Emtaillex.(*parser).startScope() */
    if re, err := regexp.Compile($1); err != nil {
      Emtaillex.(*parser).Error(fmt.Sprintf(err.Error()))
        // TODO(jaq): force a parse error
        $$ = nil
    } else {
        // We can reserve storage for these capturing groups, storing them in
        // the current scope, so that future CAPTUREGROUPs can retrieve their
        // value.  At parse time, we can warn about nonexistent names.
        for i := 1; i < re.NumSubexp() + 1; i++ {
            Emtaillex.(*parser).s.addSym(fmt.Sprintf("%d", i))
        }
        // TODO(jaq): when supported add named capturing groups
        $$ = &regexNode{$1}
    }
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
