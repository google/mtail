// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

%{
package vm

import (
    "fmt"
    "regexp/syntax"

    "github.com/google/mtail/metrics"
)

%}

%union
{
    intVal int64
    floatVal float64
    op int
    text string
    texts []string
    flag bool
    n node
    kind metrics.Kind
}

%type <n> stmt_list stmt cond arg_expr_list compound_statement conditional_statement expression_statement
%type <n> expr primary_expr multiplicative_expr additive_expr postfix_expr unary_expr assign_expr rel_expr shift_expr bitwise_expr
%type <n> declaration declarator definition decoration_statement
%type <kind> type_spec
%type <text> as_spec
%type <texts> by_spec by_expr_list
%type <flag> hide_spec
%type <op> relop shift_op bitwise_op
%type <text> pattern_expr
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Types
%token COUNTER GAUGE TIMER
// Reserved words
%token AS BY CONST HIDDEN DEF NEXT OTHERWISE ELSE
// Builtins
%token <text> BUILTIN
// Literals: re2 syntax regular expression, quoted strings, regex capture group
// references, identifiers, decorators, and numerical constants.
%token <text> REGEX
%token <text> STRING
%token <text> CAPREF
%token <text> ID
%token <text> DECO
%token <intVal> INTLITERAL
%token <floatVal> FLOATLITERAL
// Operators, in order of precedence
%token <op> INC
%token <op> DIV MOD MUL MINUS PLUS POW
%token <op> SHL SHR
%token <op> LT GT LE GE EQ NE
%token <op> AND OR XOR NOT
%token <op> ADD_ASSIGN ASSIGN
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
  : cond compound_statement ELSE compound_statement
  {
    $$ = &condNode{$1, $2, $4}
  }
  | cond compound_statement
  {
      if $1 != nil {
          $$ = &condNode{$1, $2, nil}
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
  | multiplicative_expr MOD unary_expr
  {
    $$ = &binaryExprNode{$1, $3, '%'}
  }
  | multiplicative_expr POW unary_expr
  {
    $$ = &binaryExprNode{$1, $3, $2}
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
      mtaillex.Error(fmt.Sprintf("Identifier '%s' not declared.\n\tTry " +
                                 "adding `counter %s' to the top of " +
                                 "the program.", $1, $1))
    }
  }
  | CAPREF
  {
    if sym, ok := mtaillex.(*parser).s.lookupSym($1, CaprefSymbol); ok {
      $$ = &caprefNode{$1, sym}
    } else {
      mtaillex.Error(fmt.Sprintf("Capture group $%s not defined " +
                                 "by prior regular expression in " +
                                 "this or an outer scope.\n\tTry " +
                                 "using `(?P<%s>...)' to name the " +
                                 "capture group.", $1, $1))
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
  | INTLITERAL
  {
    $$ = &numericExprNode{true, $1, 0}
  }
  | FLOATLITERAL
  {
    $$ = &numericExprNode{false, 0, $1}
  }
  ;


cond
  : pattern_expr
  {
    if re, err := syntax.Parse($1, syntax.Perl); err != nil {
      mtaillex.(*parser).ErrorP(fmt.Sprintf(err.Error()), mtaillex.(*parser).pos)
      // TODO(jaq): force a parse error
    } else {
      $$ = &regexNode{pattern: $1, re_ast: re}
      // We can reserve storage for these capturing groups, storing them in
      // the current scope, so that future CAPTUREGROUPs can retrieve their
      // value.  At parse time, we can warn about nonexistent names.
      for i := 1; i <= re.MaxCap(); i++ {
        sym := mtaillex.(*parser).s.addSym(fmt.Sprintf("%d", i),
                                            CaprefSymbol, $$,
                                            mtaillex.(*parser).pos)
        sym.addr = i - 1
      }
      for i, capref := range re.CapNames() {
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
  | OTHERWISE
  {
    $$ = &otherwiseNode{}
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
      mtaillex.Error(fmt.Sprintf("Constant '%s' not defined.\n\tTry adding `const %s /.../' earlier in the program.", $3, $3))
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
      mtaillex.Error(fmt.Sprintf("Decorator %s not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", $1, $1))
      // TODO(jaq): force a parse error.
    }
  }
  ;

opt_nl
  : /* empty */
  | NL
  ;

%%
