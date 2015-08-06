// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

%{
package vm

import (
    "io"
    "flag"
    "fmt"
    "regexp"
    "strconv"

    "github.com/google/mtail/metrics"
)

%}

%union
{
    value int
    text string
    texts []string
    flag bool
    n node
    mtype metrics.MetricType
}

%type <n> stmt_list stmt cond arg_expr_list compound_statement conditional_statement expression_statement
%type <n> expr primary_expr multiplicative_expr additive_expr postfix_expr unary_expr assign_expr rel_expr shift_expr bitwise_expr
%type <n> declaration declarator definition decoration_statement
%type <mtype> type_spec
%type <text> as_spec
%type <texts> by_spec by_expr_list
%type <flag> hide_spec
%type <value> relop shift_op bitwise_op
%type <text> pattern_expr
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Types
%token COUNTER GAUGE TIMER
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
%token <value> INC
%token <value> DIV MUL MINUS PLUS
%token <value> SHL SHR
%token <value> LT GT LE GE EQ NE
%token <value> AND OR XOR NOT
%token <value> ADD_ASSIGN ASSIGN
// Punctuation
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token COMMA
%token NL

%start start
%%

start
  : { mtaillex.(*parser).startScope() } stmt_list
  {
      $2.(*stmtlistNode).s = mtaillex.(*parser).s
      mtaillex.(*parser).endScope()
      mtaillex.(*parser).root = $2
  }
  ;

stmt_list
  : /* empty */
  {
      $$ = &stmtlistNode{}
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
  : conditional_statement 
  { $$ = $1 }
  | expression_statement
  { $$ = $1 }
  | declaration
  { $$ = $1 }
  | definition
  { $$ = $1 }
  | decoration_statement
  { $$ = $1 }
  | NEXT
  {
    $$ = &nextNode{}
  }
  | CONST ID pattern_expr
  {
    // Store the regex for concatenation
    mtaillex.(*parser).res[$2] = $3
  }
  ;

conditional_statement
  : cond compound_statement
  {
      if $1 != nil && $2 != nil {
          $$ = &condNode{$1, []node{$2}}
      } else {
          $$ = $2
      }
  }
  ;

expression_statement
  : NL
  { $$ = nil }
  | expr NL
  { $$ = $1 }
  ;

compound_statement
  : LCURLY { mtaillex.(*parser).startScope() } stmt_list RCURLY
  {
    $$ = $3
    $$.(*stmtlistNode).s = mtaillex.(*parser).s
    mtaillex.(*parser).endScope()
  }
  ;

expr
  : assign_expr
  {
    $$ = $1
  }
  ;

assign_expr
  : bitwise_expr
  {
     $$ = $1
  }
  | unary_expr ASSIGN bitwise_expr
  {
    $$ = &binaryExprNode{$1, $3, $2}
  }
  | unary_expr ADD_ASSIGN bitwise_expr
  {
    $$ = &binaryExprNode{$1, $3, $2}
  }
  ;

bitwise_expr
  : rel_expr
  { $$ = $1 }
  | bitwise_expr bitwise_op rel_expr
  {
    $$ = &binaryExprNode{$1, $3, $2}
  }
  ;

bitwise_op
  : AND
  { $$ = $1 }
  | OR
  { $$ = $1 }
  | XOR
  { $$ = $1 }
  ;

rel_expr
  : shift_expr
  { $$ = $1 }
  | rel_expr relop shift_expr
  { 
    $$ = &binaryExprNode{$1, $3, $2}
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

shift_expr
  : additive_expr
   { $$ = $1 }
  | shift_expr shift_op additive_expr
  {
    $$ = &binaryExprNode{$1, $3, $2}
  }
  ;

shift_op
  : SHL
  { $$ = $1 }
  | SHR
  { $$ = $1 }
  ;

additive_expr
  : multiplicative_expr
  { $$ = $1 }
  | additive_expr PLUS multiplicative_expr
  {
    $$ = &binaryExprNode{$1, $3, '+'}
  }
  | additive_expr MINUS multiplicative_expr
  {
    $$ = &binaryExprNode{$1, $3, '-'}
  }
  ;

multiplicative_expr
  : unary_expr
  {
    $$ = $1
  }
  | multiplicative_expr MUL unary_expr
  {
    $$ = &binaryExprNode{$1, $3, '*'}
  }
  | multiplicative_expr DIV unary_expr
  {
    $$ = &binaryExprNode{$1, $3, '/'}
  }
  ;

unary_expr
  : postfix_expr
  {
    $$ = $1
  }
  | NOT postfix_expr
  {
    $$ = &unaryExprNode{$2, $1}
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
    $$ = &unaryExprNode{$1, $2}
  }
  | postfix_expr LSQUARE expr RSQUARE
  {
    $$ = &indexedExprNode{$1, $3}
  }
  ;

primary_expr
  : ID
  {
    if sym, ok := mtaillex.(*parser).s.lookupSym($1, IDSymbol); ok {
      $$ = &idNode{$1, sym}
    } else {
      mtaillex.Error(fmt.Sprintf("Identifier '%s' not declared.", $1))
    }
  }
  | CAPREF
  {
    if sym, ok := mtaillex.(*parser).s.lookupSym($1, CaprefSymbol); ok {
      $$ = &caprefNode{$1, sym}
    } else {
      mtaillex.Error(fmt.Sprintf("Capture group $%s not defined " +
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
      mtaillex.(*parser).ErrorP(fmt.Sprintf(err.Error()), mtaillex.(*parser).pos)
      // TODO(jaq): force a parse error
    } else {
      $$ = &regexNode{pattern: $1}
      // We can reserve storage for these capturing groups, storing them in
      // the current scope, so that future CAPTUREGROUPs can retrieve their
      // value.  At parse time, we can warn about nonexistent names.
      for i := 1; i < re.NumSubexp() + 1; i++ {
        sym := mtaillex.(*parser).s.addSym(fmt.Sprintf("%d", i),
                                            CaprefSymbol, $$,
                                            mtaillex.(*parser).pos)
        sym.addr = i - 1
      }
      for i, capref := range re.SubexpNames() {
        if capref != "" {
          sym := mtaillex.(*parser).s.addSym(capref, CaprefSymbol, $$,
                                              mtaillex.(*parser).pos)
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
  : DIV { mtaillex.(*parser).inRegex() } REGEX DIV
  {
    // Stash the start of the pattern_expr in a state variable.
    // We know it's the start because pattern_expr is left associative.
    mtaillex.(*parser).pos = mtaillex.(*parser).t.pos
    $$ = $3
  }
  | pattern_expr PLUS opt_nl DIV { mtaillex.(*parser).inRegex() } REGEX DIV
  {
    $$ = $1 + $6
  }
  | pattern_expr PLUS ID
  {
    if s, ok := mtaillex.(*parser).res[$3]; ok {
      $$ = $1 + s
    } else {
      mtaillex.Error(fmt.Sprintf("Constant '%s' not defined.", $3))
    }
  }
  ;


declaration
  : hide_spec type_spec declarator
  {
    $$ = $3
    d := $$.(*declNode)
    d.kind = $2

    var n string
    if d.exportedName != "" {
        n = d.exportedName
	} else {
        n = d.name
   	}
    d.m = metrics.NewMetric(n, mtaillex.(*parser).name, d.kind, d.keys...)
    d.sym = mtaillex.(*parser).s.addSym(d.name, IDSymbol, d.m,
                                          mtaillex.(*parser).t.pos)
    if !$1 {
       mtaillex.(*parser).ms.Add(d.m)
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
    $$.(*declNode).exportedName = $2
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
    $$ = metrics.Counter
  }
  | GAUGE
  {
    $$ = metrics.Gauge
  }
  | TIMER
  {
    $$ = metrics.Timer
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

definition
: DEF ID compound_statement
  {
      $$ = &defNode{name: $2, children: []node{$3}}
      d := $$.(*defNode)
      d.sym = mtaillex.(*parser).s.addSym(d.name, DefSymbol, d, mtaillex.(*parser).t.pos)
  }
  ;

decoration_statement
  : DECO compound_statement
  {
    if sym, ok := mtaillex.(*parser).s.lookupSym($1, DefSymbol); ok {
      $$ = &decoNode{$1, []node{$2}, sym.binding.(*defNode)}
    } else {
      mtaillex.Error(fmt.Sprintf("Decorator %s not defined", $1))
      // TODO(jaq): force a parse error.
    }
  }
  ;

opt_nl
  :
  | NL
  ;

%%
const EOF = 0

type parser struct {
    name   string
    root   node
    errors ErrorList
    l      *lexer
    t      token             // Most recently lexed token.
    pos position             // Maybe contains the position of the start of a node.
    s      *scope
    res    map[string]string // Mapping of regex constants to patterns.
    ms     *metrics.Store     // List of metrics exported by this program.
}

func newParser(name string, input io.Reader, ms *metrics.Store) *parser {
    mtailDebug = *mtailDebugFlag
    return &parser{name: name, l: newLexer(name, input), res: make(map[string]string), ms: ms}
}

func (p *parser) ErrorP(s string, pos position) {
    p.errors.Add(pos, s)
}

func (p *parser) Error(s string) {
    p.errors.Add(p.t.pos, s)
}

func (p *parser) Lex(lval *mtailSymType) int {
    p.t = p.l.nextToken()
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
    case LT, GT, LE, GE, NE, EQ, SHL, SHR, AND, OR, XOR, NOT, INC, DIV, MUL, MINUS, PLUS, ASSIGN, ADD_ASSIGN:
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

func (p *parser) inRegex() {
    p.l.in_regex = true
}

var mtailDebugFlag = flag.Int("mtailDebug", 0, "Set parser debug level.")
