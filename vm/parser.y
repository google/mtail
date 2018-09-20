// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

%{
package vm

import (
    "github.com/google/mtail/metrics"
    "github.com/golang/glog"
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
    n astNode
    kind metrics.Kind
}

%type <n> stmt_list stmt arg_expr_list compound_statement conditional_statement expression_statement
%type <n> expr primary_expr multiplicative_expr additive_expr postfix_expr unary_expr assign_expr
%type <n> rel_expr shift_expr bitwise_expr logical_expr indexed_expr id_expr concat_expr pattern_expr
%type <n> declaration declarator definition decoration_statement regex_pattern match_expr
%type <kind> type_spec
%type <text> as_spec
%type <texts> by_spec by_expr_list
%type <flag> hide_spec
%type <op> rel_op shift_op bitwise_op logical_op add_op mul_op match_op
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Types
%token COUNTER GAUGE TIMER TEXT
// Reserved words
%token AS BY CONST HIDDEN DEF DEL NEXT OTHERWISE ELSE
// Builtins
%token <text> BUILTIN
// Literals: re2 syntax regular expression, quoted strings, regex capture group
// references, identifiers, decorators, and numerical constants.
%token <text> REGEX
%token <text> STRING
%token <text> CAPREF CAPREF_NAMED
%token <text> ID
%token <text> DECO
%token <intVal> INTLITERAL
%token <floatVal> FLOATLITERAL
// Operators, in order of precedence
%token <op> INC DEC
%token <op> DIV MOD MUL MINUS PLUS POW
%token <op> SHL SHR
%token <op> LT GT LE GE EQ NE
%token <op> BITAND XOR BITOR NOT AND OR
%token <op> ADD_ASSIGN ASSIGN
%token <op> CONCAT
%token <op> MATCH NOT_MATCH
// Punctuation
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token COMMA
%token NL

%start start

// The %error directive takes a list of tokens describing a parser state in error, and an error message.
// See "Generating LR syntax error messages from examples", Jeffery, ACM TOPLAS Volume 24 Issue 5 Sep 2003.
%error stmt_list stmt expression_statement mark_pos DIV in_regex INVALID  : "unexpected end of file"
%%

start
  : stmt_list
  {
    mtaillex.(*parser).root = $1
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
    $$ = &nextNode{tokenpos(mtaillex)}
  }
  | CONST id_expr concat_expr
  {
    $$ = &patternFragmentDefNode{id: $2, expr: $3}
  }
  | DEL postfix_expr
  {
    $$ = &delNode{tokenpos(mtaillex), $2}
  }
  | INVALID
  {
    $$ = &errorNode{tokenpos(mtaillex), $1}
  }
  ;

conditional_statement
  : logical_expr compound_statement ELSE compound_statement
  {
    $$ = &condNode{$1, $2, $4, nil}
  }
  | logical_expr compound_statement
  {
    if $1 != nil {
      $$ = &condNode{$1, $2, nil, nil}
    } else {
      $$ = $2
    }
  }
  | OTHERWISE compound_statement
  {
    o := &otherwiseNode{tokenpos(mtaillex)}
    $$ = &condNode{o, $2, nil, nil}
  }
  ;

expression_statement
  : NL
  { $$ = nil }
  | expr NL
  { $$ = $1 }
  ;

compound_statement
  : LCURLY stmt_list RCURLY
  {
    $$ = $2
  }
  ;

expr
  : assign_expr
  { $$ = $1 }
  ;

assign_expr
  : logical_expr
  {
    $$ = $1
  }
  | unary_expr ASSIGN opt_nl logical_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  | unary_expr ADD_ASSIGN opt_nl logical_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  ;

logical_expr
  : bitwise_expr
  { $$ = $1 }
  | match_expr
  { $$ = $1 }
  | logical_expr logical_op opt_nl bitwise_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  | logical_expr logical_op opt_nl match_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  ;

logical_op
  : AND
  { $$ = $1 }
  | OR
  { $$ = $1 }
  ;

bitwise_expr
  : rel_expr
  { $$ = $1 }
  | bitwise_expr bitwise_op opt_nl rel_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  ;

bitwise_op
  : BITAND
  { $$ = $1 }
  | BITOR
  { $$ = $1 }
  | XOR
  { $$ = $1 }
  ;

rel_expr
  : shift_expr
  { $$ = $1 }
  | rel_expr rel_op opt_nl shift_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  ;

rel_op
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
  | shift_expr shift_op opt_nl additive_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
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
  | additive_expr add_op opt_nl multiplicative_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  ;

match_expr
  : pattern_expr
  { $$ = $1 }
  | primary_expr match_op opt_nl pattern_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  | primary_expr match_op opt_nl primary_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }  
  ;

match_op
  : MATCH
  { $$ = $1 }
  | NOT_MATCH
  { $$ = $1 }
  ;

pattern_expr
  : concat_expr
  {
    $$ = &patternExprNode{expr: $1}
  }
  ;

concat_expr
  : regex_pattern
  { $$ = $1 }
  | concat_expr PLUS opt_nl regex_pattern
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: CONCAT}
  }
  | concat_expr PLUS opt_nl id_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: CONCAT}
  }
  ;

add_op
  : PLUS
  { $$ = $1 }
  | MINUS
  { $$ = $1 }
  ;

multiplicative_expr
  : unary_expr
  { $$ = $1 }
  | multiplicative_expr mul_op opt_nl unary_expr
  {
    $$ = &binaryExprNode{lhs: $1, rhs: $4, op: $2}
  }
  ;

mul_op
  : MUL
  { $$ = $1 }
  | DIV
  { $$ = $1 }
  | MOD
  { $$ = $1 }
  | POW
  { $$ = $1 }
  ;

unary_expr
  : postfix_expr
  { $$ = $1 }
  | NOT unary_expr
  {
    $$ = &unaryExprNode{pos: tokenpos(mtaillex), expr: $2, op: $1}
  }

postfix_expr
  : primary_expr
  { $$ = $1 }
  | postfix_expr INC
  {
    $$ = &unaryExprNode{pos: tokenpos(mtaillex), expr: $1, op: $2}
  }
  ;

primary_expr
  : indexed_expr
  { $$ = $1 }
  | BUILTIN LPAREN RPAREN
  {
    $$ = &builtinNode{pos: tokenpos(mtaillex), name: $1, args: nil}
  }
  | BUILTIN LPAREN arg_expr_list RPAREN
  {
    $$ = &builtinNode{pos: tokenpos(mtaillex), name: $1, args: $3}
  }
  | CAPREF
  {
    $$ = &caprefNode{tokenpos(mtaillex), $1, false, nil}
  }
  | CAPREF_NAMED
  {
    $$ = &caprefNode{tokenpos(mtaillex), $1, true, nil}
  }
  | STRING
  {
    $$ = &stringConstNode{tokenpos(mtaillex), $1}
  }
  | LPAREN expr RPAREN
  {
    $$ = $2
  }
  | INTLITERAL
  {
    $$ = &intConstNode{tokenpos(mtaillex), $1}
  }
  | FLOATLITERAL
  {
    $$ = &floatConstNode{tokenpos(mtaillex), $1}
  }
  ;

indexed_expr
  : id_expr
  {
    $$ = &indexedExprNode{lhs: $1, index: &exprlistNode{}}
  }
  | indexed_expr LSQUARE arg_expr_list RSQUARE
  {
    $$ = $1
      $$.(*indexedExprNode).index.(*exprlistNode).children = append(
        $$.(*indexedExprNode).index.(*exprlistNode).children,
        $3.(*exprlistNode).children...)
  }
  ;

id_expr
  : ID
  {
    $$ = &idNode{tokenpos(mtaillex), $1, nil, false}
  }
  ;

arg_expr_list
  : bitwise_expr
  {
    $$ = &exprlistNode{}
    $$.(*exprlistNode).children = append($$.(*exprlistNode).children, $1)
  }
  | arg_expr_list COMMA bitwise_expr
  {
    $$ = $1
    $$.(*exprlistNode).children = append($$.(*exprlistNode).children, $3)
  }
  ;

regex_pattern
  : mark_pos DIV in_regex REGEX DIV
  {
    mp := markedpos(mtaillex)
    tp := tokenpos(mtaillex)
    pos := MergePosition(&mp, &tp)
    $$ = &patternConstNode{pos: *pos, pattern: $4}
  }
  ;

declaration
  : hide_spec type_spec declarator
  {
    $$ = $3
    d := $$.(*declNode)
    d.kind = $2
    d.hidden = $1
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
    $$ = &declNode{pos: tokenpos(mtaillex), name: $1}
  }
  | STRING
  {
    $$ = &declNode{pos: tokenpos(mtaillex), name: $1}
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
  | TEXT
  {
    $$ = metrics.Text
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
  : mark_pos DEF ID compound_statement
  {
    $$ = &decoDefNode{pos: markedpos(mtaillex), name: $3, block: $4}
  }
  ;

decoration_statement
  : mark_pos DECO compound_statement
  {
    $$ = &decoNode{markedpos(mtaillex), $2, $3, nil, nil}
  }
  ;

// mark_pos is an epsilon (marker nonterminal) that records the current token
// position as the parser position.  Use markedpos() to fetch the position and
// merge with tokenpos for exotic productions.
mark_pos
  : /* empty */
  {
    glog.V(2).Infof("position marked at %v", tokenpos(mtaillex))
    mtaillex.(*parser).pos = tokenpos(mtaillex)
  }
  ;

// in_regex is a marker nonterminal that tells the parser and lexer it is now
// in a regular expression
in_regex
  :  /* empty */
  {
    mtaillex.(*parser).inRegex()
  }
  ;

// opt_nl optionally accepts a newline when a line break could occur inside an
// expression for formatting.  Newlines terminate expressions so must be
// handled explicitly.
opt_nl
  : /* empty */
  | NL
  ;


%%

//  tokenpos returns the position of the current token.
func tokenpos(mtaillex mtailLexer) position {
    return mtaillex.(*parser).t.pos
}

// markedpos returns the position recorded from the most recent mark_pos
// production.
func markedpos(mtaillex mtailLexer) position {
    return mtaillex.(*parser).pos
}
