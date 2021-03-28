// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

%{
package parser

import (
    "time"

    "github.com/google/mtail/internal/metrics"
    "github.com/google/mtail/internal/vm/ast"
    "github.com/google/mtail/internal/vm/position"
    "github.com/golang/glog"
)

%}

%union
{
    intVal int64
    floatVal float64
    floats []float64
    op int
    text string
    texts []string
    flag bool
    n ast.Node
    kind metrics.Kind
    duration time.Duration
}

%type <n> stmt_list stmt arg_expr_list compound_stmt conditional_stmt expr_stmt
%type <n> expr primary_expr multiplicative_expr additive_expr postfix_expr unary_expr assign_expr
%type <n> rel_expr shift_expr bitwise_expr logical_expr indexed_expr id_expr concat_expr pattern_expr
%type <n> declaration decl_attribute_spec decorator_declaration decoration_stmt regex_pattern match_expr
%type <n> delete_stmt var_name_spec implicit_match_expr
%type <kind> type_spec
%type <text> as_spec id_or_string
%type <texts> by_spec by_expr_list
%type <flag> hide_spec
%type <op> rel_op shift_op bitwise_op logical_op add_op mul_op match_op postfix_op
%type <floats> buckets_spec buckets_list
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Types
%token COUNTER GAUGE TIMER TEXT HISTOGRAM
// Reserved words
%token AFTER AS BY CONST HIDDEN DEF DEL NEXT OTHERWISE ELSE STOP BUCKETS
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
%token <duration> DURATIONLITERAL
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
%error stmt_list stmt expr_stmt mark_pos DIV in_regex INVALID  : "unexpected end of file, expecting '/' to end regex"
%error stmt_list stmt conditional_stmt logical_expr LCURLY stmt_list $end : "unexpected end of file, expecting '}' to end block"
%error stmt_list stmt conditional_stmt logical_expr compound_stmt ELSE LCURLY stmt_list $end : "unexpected end of file, expecting '}' to end block"
%error stmt_list stmt conditional_stmt OTHERWISE LCURLY stmt_list $end : "unexpected end of file, expecting '}' to end block"
%error stmt_list stmt conditional_stmt logical_expr compound_stmt conditional_stmt logical_expr LSQUARE : "unexpected indexing of an expression"
%error stmt_list stmt conditional_stmt pattern_expr NL : "statement with no effect, missing an assignment, `+' concatenation, or `{}' block?"

%%

/* An `mtail` program is a list of statements. */
start
  : stmt_list
  {
    mtaillex.(*parser).root = $1
  }
  ;

/* A statement list is either empty, or recurses another statement list and a statement. */
stmt_list
  : /* empty */
  {
    $$ = &ast.StmtList{}
  }
  | stmt_list stmt
  {
    $$ = $1
    if ($2 != nil) {
      $$.(*ast.StmtList).Children = append($$.(*ast.StmtList).Children, $2)
    }
  }
  ;

/* Types of statements. */
stmt
  : conditional_stmt
  { $$ = $1 }
  | expr_stmt
  { $$ = $1 }
  | declaration
  { $$ = $1 }
  | decorator_declaration
  { $$ = $1 }
  | decoration_stmt
  { $$ = $1 }
  | delete_stmt
  { $$ = $1 }
  | NEXT
  {
    $$ = &ast.NextStmt{tokenpos(mtaillex)}
  }
  | CONST id_expr concat_expr
  {
    $$ = &ast.PatternFragment{Id: $2, Expr: $3}
  }
  | STOP
  {
    $$ = &ast.StopStmt{tokenpos(mtaillex)}
  }
  | INVALID
  {
    $$ = &ast.Error{tokenpos(mtaillex), $1}
  }
  ;

/* Conditional statement is a test condition, and then actions executed depending on the result of the test. */
conditional_stmt
  : logical_expr compound_stmt ELSE compound_stmt
  {
    $$ = &ast.CondStmt{$1, $2, $4, nil}
  }
  | logical_expr compound_stmt
  {
    if $1 != nil {
      $$ = &ast.CondStmt{$1, $2, nil, nil}
    } else {
      $$ = $2
    }
  }
  | OTHERWISE compound_stmt
  {
    o := &ast.OtherwiseStmt{tokenpos(mtaillex)}
    $$ = &ast.CondStmt{o, $2, nil, nil}
  }
  ;

/* Expression statement is a statement that is also an expression. */
expr_stmt
  : NL
  { $$ = nil }
  | expr NL
  { $$ = $1 }
  ;

/* Compound statement is a nested statement list with its own scope. */
compound_stmt
  : LCURLY stmt_list RCURLY
  {
    $$ = $2
  }
  ;

/* Expressions perform a computation and return a result. The expression tree is ordered in ascending associativity of the operator types. */
expr
  : assign_expr
  { $$ = $1 }
  | postfix_expr
  { $$ = $1 }
  ;

/* Assignment expressions assign a value to the left hand side from the results of the right hand side. */
assign_expr
  : unary_expr ASSIGN opt_nl logical_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  | unary_expr ADD_ASSIGN opt_nl logical_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  ;

/* Logical expressions perform comparisons with logical operators. */
logical_expr
  : bitwise_expr
  { $$ = $1 }
  | match_expr
  { $$ = $1 }
  | logical_expr logical_op opt_nl bitwise_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  | logical_expr logical_op opt_nl match_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  ;

logical_op
  : AND
  { $$ = $1 }
  | OR
  { $$ = $1 }
  ;

/* Bitwise expression performs bitwise comparisons of the left and right hand sides. */
bitwise_expr
  : rel_expr
  { $$ = $1 }
  | bitwise_expr bitwise_op opt_nl rel_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
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

/* Relational expressions perform relational comparisons, e.g. less than */
rel_expr
  : shift_expr
  { $$ = $1 }
  | rel_expr rel_op opt_nl shift_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
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

/* Shift expressions perform bitshift operations on the left hand side. */
shift_expr
  : additive_expr
  { $$ = $1 }
  | shift_expr shift_op opt_nl additive_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  ;

shift_op
  : SHL
  { $$ = $1 }
  | SHR
  { $$ = $1 }
  ;

/* Additive expressions perform addition and subtraction */
additive_expr
  : multiplicative_expr
  { $$ = $1 }
  | additive_expr add_op opt_nl multiplicative_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  ;

add_op
  : PLUS
  { $$ = $1 }
  | MINUS
  { $$ = $1 }
  ;

/* Match expressions perform pattern matching against the left hand side. */
match_expr
  : implicit_match_expr
  {
    $$ = $1
  }
  | primary_expr match_op opt_nl pattern_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  | primary_expr match_op opt_nl primary_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
  }
  ;

match_op
  : MATCH
  { $$ = $1 }
  | NOT_MATCH
  { $$ = $1 }
  ;

/* Implicit match expression has only a pattern, and matches against the line of input. */
implicit_match_expr
  : pattern_expr
  {
    $$ = &ast.UnaryExpr{P: tokenpos(mtaillex), Expr: $1, Op: MATCH}
  }
  ;

/* Pattern expression constructs a regular expression. */
pattern_expr
  : concat_expr
  {
    $$ = &ast.PatternExpr{Expr: $1}
  }
  ;

/* Concatenation expression forms a regular expression pattern from fragments. */
concat_expr
  : regex_pattern
  { $$ = $1 }
  | concat_expr PLUS opt_nl regex_pattern
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: CONCAT}
  }
  | concat_expr PLUS opt_nl id_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: CONCAT}
  }
  ;

/* Multiplicative expression performs multiply and division operations. */
multiplicative_expr
  : unary_expr
  { $$ = $1 }
  | multiplicative_expr mul_op opt_nl unary_expr
  {
    $$ = &ast.BinaryExpr{Lhs: $1, Rhs: $4, Op: $2}
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

/* Unary expressions perform negation */
unary_expr
  : postfix_expr
  { $$ = $1 }
  | NOT unary_expr
  {
    $$ = &ast.UnaryExpr{P: tokenpos(mtaillex), Expr: $2, Op: $1}
  }
  ;

/* Postfix expressions perform increment and decrement. */
postfix_expr
  : primary_expr
  { $$ = $1 }
  | postfix_expr postfix_op
  {
    $$ = &ast.UnaryExpr{P: tokenpos(mtaillex), Expr: $1, Op: $2}
  }
  ;

postfix_op
  : INC
  { $$ = $1 }
  | DEC
  { $$ = $1 }
  ;

/* Primary expression contains indexing, builtin calls, and terminal symbols. */
primary_expr
  : indexed_expr
  { $$ = $1 }
  | BUILTIN LPAREN RPAREN
  {
    $$ = &ast.BuiltinExpr{P: tokenpos(mtaillex), Name: $1, Args: nil}
  }
  | BUILTIN LPAREN arg_expr_list RPAREN
  {
    $$ = &ast.BuiltinExpr{P: tokenpos(mtaillex), Name: $1, Args: $3}
  }
  | CAPREF
  {
    $$ = &ast.CaprefTerm{tokenpos(mtaillex), $1, false, nil}
  }
  | CAPREF_NAMED
  {
    $$ = &ast.CaprefTerm{tokenpos(mtaillex), $1, true, nil}
  }
  | STRING
  {
    $$ = &ast.StringLit{tokenpos(mtaillex), $1}
  }
  | LPAREN logical_expr RPAREN
  {
    $$ = $2
  }
  | INTLITERAL
  {
    $$ = &ast.IntLit{tokenpos(mtaillex), $1}
  }
  | FLOATLITERAL
  {
    $$ = &ast.FloatLit{tokenpos(mtaillex), $1}
  }
  ;

/* Indexed expression performs index lookup. */
indexed_expr
  : id_expr
  {
    $$ = &ast.IndexedExpr{Lhs: $1, Index: &ast.ExprList{}}
  }
  | indexed_expr LSQUARE arg_expr_list RSQUARE
  {
    $$ = $1
      $$.(*ast.IndexedExpr).Index.(*ast.ExprList).Children = append(
        $$.(*ast.IndexedExpr).Index.(*ast.ExprList).Children,
        $3.(*ast.ExprList).Children...)
  }
  ;

/* Identifier expression names a variable. */
id_expr
  : ID
  {
    $$ = &ast.IdTerm{tokenpos(mtaillex), $1, nil, false}
  }
  ;

/* Argument expression list describes the part of a builtin call inside the parentheses. */
arg_expr_list
  : bitwise_expr
  {
    $$ = &ast.ExprList{}
    $$.(*ast.ExprList).Children = append($$.(*ast.ExprList).Children, $1)
  }
  | arg_expr_list COMMA bitwise_expr
  {
    $$ = $1
    $$.(*ast.ExprList).Children = append($$.(*ast.ExprList).Children, $3)
  }
  ;

/* Regular expression pattern describes a regular expression literal. */
regex_pattern
  : mark_pos DIV in_regex REGEX DIV
  {
    mp := markedpos(mtaillex)
    tp := tokenpos(mtaillex)
    pos := ast.MergePosition(&mp, &tp)
    $$ = &ast.PatternLit{P: *pos, Pattern: $4}
  }
  ;

/* Declaration creates a new metric. */
declaration
  : hide_spec type_spec decl_attribute_spec
  {
    $$ = $3
    d := $$.(*ast.VarDecl)
    d.Kind = $2
    d.Hidden = $1
  }
  ;

/* A hide specification can mark a declaration as hidden from export. */
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

/* A declaration attribute specification adds attributes to the declaration such as index keys, exported name, or bucket enumerations. */
decl_attribute_spec
  : decl_attribute_spec by_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).Keys = $2
  }
  | decl_attribute_spec as_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).ExportedName = $2
  }
  | decl_attribute_spec buckets_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).Buckets = $2
  }
  | var_name_spec
  {
    $$ = $1
  }
  ;

/* Variable name spec names a variable in a declaration. */
var_name_spec
  : ID
  {
    $$ = &ast.VarDecl{P: tokenpos(mtaillex), Name: $1}
  }
  | STRING
  {
    $$ = &ast.VarDecl{P: tokenpos(mtaillex), Name: $1}
  }
  ;

/* Type specfication enumerates the type classification of a variable. */
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
  | HISTOGRAM
  {
    $$ = metrics.Histogram
  }
  ;

/* By specification describes index keys for a multidimensional variable. */
by_spec
  : BY by_expr_list
  {
    $$ = $2
  }
  ;

by_expr_list
  : id_or_string
  {
    $$ = make([]string, 0)
    $$ = append($$, $1)
  }
  | by_expr_list COMMA id_or_string
  {
    $$ = $1
    $$ = append($$, $3)
  }
  ;

/* As specification describes how to rename a variable for export. */
as_spec
  : AS STRING
  {
    $$ = $2
  }
  ;

/* Bucket specification describes the bucketing arrangement in a histogram type. */
buckets_spec
  : BUCKETS buckets_list
  {
    $$ = $2
  }

buckets_list
  : FLOATLITERAL
  {
    $$ = make([]float64, 0)
    $$ = append($$, $1)
  }
  | INTLITERAL
  {
    $$ = make([]float64, 0)
    $$ = append($$, float64($1))
  }
  | buckets_list COMMA FLOATLITERAL
  {
    $$ = $1
    $$ = append($$, $3)
  }
  | buckets_list COMMA INTLITERAL
  {
    $$ = $1
    $$ = append($$, float64($3))
  }

/* Decorator declaration parses the declaration and definition of a match decorator. */
decorator_declaration
  : mark_pos DEF ID compound_stmt
  {
    $$ = &ast.DecoDecl{P: markedpos(mtaillex), Name: $3, Block: $4}
  }
  ;

/* Decoration statement parses an instantiation of a decorator. */
decoration_stmt
  : mark_pos DECO compound_stmt
  {
    $$ = &ast.DecoStmt{markedpos(mtaillex), $2, $3, nil, nil}
  }
  ;

/* Delete statement parses a delete command. */
delete_stmt
  : DEL postfix_expr AFTER DURATIONLITERAL
  {
    $$ = &ast.DelStmt{P: tokenpos(mtaillex), N: $2, Expiry: $4}
  }
  | DEL postfix_expr
  {
    $$ = &ast.DelStmt{P: tokenpos(mtaillex), N: $2}
  }

/* Identifier or String parses where an ID or a string can be expected. */
id_or_string
  : ID
  {
    $$ = $1
  }
  | STRING
  {
    $$ = $1
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
func tokenpos(mtaillex mtailLexer) position.Position {
    return mtaillex.(*parser).t.Pos
}

// markedpos returns the position recorded from the most recent mark_pos
// production.
func markedpos(mtaillex mtailLexer) position.Position {
    return mtaillex.(*parser).pos
}
