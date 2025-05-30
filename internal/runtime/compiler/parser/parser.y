// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

%{
/* #nosec G104 generated code, errors reported do not make sense */
package parser

import (
    "time"

    "github.com/jaqx0r/mtail/internal/metrics"
    "github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
    "github.com/jaqx0r/mtail/internal/runtime/compiler/position"
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

%type <n> stmt_list stmt arg_expr_list compound_stmt conditional_stmt conditional_expr expr_stmt
%type <n> expr primary_expr multiplicative_expr additive_expr postfix_expr unary_expr assign_expr
%type <n> rel_expr shift_expr bitwise_expr logical_expr indexed_expr id_expr concat_expr pattern_expr
%type <n> metric_declaration metric_decl_attr_spec decorator_declaration decoration_stmt regex_pattern match_expr
%type <n> delete_stmt metric_name_spec builtin_expr arg_expr
%type <kind> metric_type_spec
%type <intVal> metric_limit_spec
%type <text> metric_as_spec id_or_string metric_by_expr
%type <texts> metric_by_spec metric_by_expr_list
%type <flag> metric_hide_spec
%type <op> rel_op shift_op bitwise_op logical_op add_op mul_op match_op postfix_op
%type <floats> metric_buckets_spec metric_buckets_list
// Tokens and types are defined here.
// Invalid input
%token <text> INVALID
// Types
%token COUNTER GAUGE TIMER TEXT HISTOGRAM
// Reserved words
%token AFTER AS BY CONST HIDDEN DEF DEL NEXT OTHERWISE ELSE STOP BUCKETS LIMIT
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
%token <op> MATCH NOT_MATCH
// Punctuation
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token COMMA
%token NL

%start start

// The %error directive takes a list of tokens describing a parser state in error, and an error message.
// See "Generating LR syntax error messages from examples", Jeffery, ACM TOPLAS Volume 24 Issue 5 Sep 2003.
%error stmt_list stmt expr_stmt mark_pos DIV in_regex INVALID  : "unexpected end of file, expecting '/' to end regex"
%error stmt_list stmt conditional_stmt conditional_expr LCURLY stmt_list $end : "unexpected end of file, expecting '}' to end block"
%error stmt_list stmt conditional_stmt conditional_expr compound_stmt ELSE LCURLY stmt_list $end : "unexpected end of file, expecting '}' to end block"
%error stmt_list stmt conditional_stmt OTHERWISE LCURLY stmt_list $end : "unexpected end of file, expecting '}' to end block"
%error stmt_list stmt conditional_stmt conditional_expr compound_stmt conditional_stmt conditional_expr LSQUARE : "unexpected indexing of an expression"
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
  | metric_declaration
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
  | CONST id_expr opt_nl concat_expr
  {
    $$ = &ast.PatternFragment{ID: $2, Expr: $4}
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
  : conditional_expr compound_stmt ELSE compound_stmt
  {
    $$ = &ast.CondStmt{$1, $2, $4, nil}
  }
  | conditional_expr compound_stmt
  {
    if $1 != nil {
      $$ = &ast.CondStmt{$1, $2, nil, nil}
    } else {
      $$ = $2
    }
  }
  | mark_pos OTHERWISE compound_stmt
  {
    o := &ast.OtherwiseStmt{positionFromMark(mtaillex)}
    $$ = &ast.CondStmt{o, $3, nil, nil}
  }
  ;

conditional_expr
  : pattern_expr
  {
    $$ = &ast.UnaryExpr{P: tokenpos(mtaillex), Expr: $1, Op: MATCH}
  }
  | pattern_expr logical_op opt_nl logical_expr
  {
    $$ = &ast.BinaryExpr{
      LHS: &ast.UnaryExpr{P: tokenpos(mtaillex), Expr: $1, Op: MATCH},
      RHS: $4,
      Op: $2,
    }
  }
  | logical_expr
  { $$ = $1 }
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
  }
  | unary_expr ADD_ASSIGN opt_nl logical_expr
  {
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
  }
  | logical_expr logical_op opt_nl match_expr
  {
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
  : primary_expr match_op opt_nl pattern_expr
  {
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
  }
  | primary_expr match_op opt_nl primary_expr
  {
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
  }
  ;

match_op
  : MATCH
  { $$ = $1 }
  | NOT_MATCH
  { $$ = $1 }
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
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: PLUS}
  }
  | concat_expr PLUS opt_nl id_expr
  {
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: PLUS}
  }
  ;

/* Multiplicative expression performs multiply and division operations. */
multiplicative_expr
  : unary_expr
  { $$ = $1 }
  | multiplicative_expr mul_op opt_nl unary_expr
  {
    $$ = &ast.BinaryExpr{LHS: $1, RHS: $4, Op: $2}
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
  | builtin_expr
  { $$ = $1 }
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
    // Build an empty IndexedExpr so that the recursive rule below doesn't need to handle the alternative.
    $$ = &ast.IndexedExpr{LHS: $1, Index: &ast.ExprList{}}
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
    $$ = &ast.IDTerm{tokenpos(mtaillex), $1, nil, false}
  }
  ;

/* Builtin expression describes the builtin function calls. */
builtin_expr
  : mark_pos BUILTIN LPAREN RPAREN
  {
    $$ = &ast.BuiltinExpr{P: positionFromMark(mtaillex), Name: $2, Args: nil}
  }
  | mark_pos BUILTIN LPAREN arg_expr_list RPAREN
  {
    $$ = &ast.BuiltinExpr{P: positionFromMark(mtaillex), Name: $2, Args: $4}
  }
  ;


/* Argument expression list describes the part of a builtin call inside the parentheses. */
arg_expr_list
  : arg_expr
  {
    $$ = &ast.ExprList{}
    $$.(*ast.ExprList).Children = append($$.(*ast.ExprList).Children, $1)
  }
  | arg_expr_list COMMA arg_expr
  {
    $$ = $1
    $$.(*ast.ExprList).Children = append($$.(*ast.ExprList).Children, $3)
  }
  ;

arg_expr
  : logical_expr
  { $$ = $1 }
  | pattern_expr
  { $$ = $1 }
  ;

/* Regular expression pattern describes a regular expression literal. */
regex_pattern
  : mark_pos DIV in_regex REGEX DIV
  {
    $$ = &ast.PatternLit{P: positionFromMark(mtaillex), Pattern: $4}
  }
  ;

/* Declaration creates a new metric. */
metric_declaration
  : metric_hide_spec metric_type_spec metric_decl_attr_spec
  {
    $$ = $3
    d := $$.(*ast.VarDecl)
    d.Kind = $2
    d.Hidden = $1
  }
  ;

/* A hide specification can mark a metric as hidden from export. */
metric_hide_spec
  : /* empty */
  {
    $$ = false
  }
  | HIDDEN
  {
    $$ = true
  }
  ;

/* A declaration attribute specification adds attributes to the metric declaration such as index keys, exported name, or bucket enumerations. */
metric_decl_attr_spec
  : metric_decl_attr_spec metric_by_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).Keys = $2
  }
  | metric_decl_attr_spec metric_as_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).ExportedName = $2
  }
  | metric_decl_attr_spec metric_buckets_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).Buckets = $2
  }
  | metric_decl_attr_spec metric_limit_spec
  {
    $$ = $1
    $$.(*ast.VarDecl).Limit = $2
  }
  | metric_name_spec
  {
    $$ = $1
  }
  ;

/* Variable name spec names a metric  in a declaration. */
metric_name_spec
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
metric_type_spec
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
metric_by_spec
  : BY metric_by_expr_list
  {
    $$ = $2
  }
  ;

metric_by_expr_list
  : metric_by_expr
  {
    $$ = make([]string, 0)
    $$ = append($$, $1)
  }
  | metric_by_expr_list COMMA metric_by_expr
  {
    $$ = $1
    $$ = append($$, $3)
  }
  ;

metric_by_expr
  : id_or_string
  { $$ = $1 }
  ;

/* As specification describes how to rename a variable for export. */
metric_as_spec
  : AS STRING
  {
    $$ = $2
  }
  ;

metric_limit_spec
  : LIMIT INTLITERAL
  {
    $$ = $2
  }
  ;

/* Bucket specification describes the bucketing arrangement in a histogram type. */
metric_buckets_spec
  : BUCKETS metric_buckets_list
  {
    $$ = $2
  }

metric_buckets_list
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
  | metric_buckets_list COMMA FLOATLITERAL
  {
    $$ = $1
    $$ = append($$, $3)
  }
  | metric_buckets_list COMMA INTLITERAL
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
  : mark_pos DEL postfix_expr AFTER DURATIONLITERAL
  {
    $$ = &ast.DelStmt{P: positionFromMark(mtaillex), N: $3, Expiry: $5}
  }
  | mark_pos DEL postfix_expr
  {
    $$ = &ast.DelStmt{P: positionFromMark(mtaillex), N: $3}
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

// positionFromMark returns a position spanning from the last mark to the current position.
func positionFromMark(mtaillex mtailLexer) position.Position {
    tp := tokenpos(mtaillex)
    mp := markedpos(mtaillex)
    return *position.Merge(&mp, &tp)
}
