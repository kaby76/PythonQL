/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : PythonQL, based on python3-parser; an ANTLR4 grammar for Python 3
 *                https://github.com/bkiers/python3-parser
 * Developed by : Pavel Velikhov, pavel.velikhov@gmail.com
 */
parser grammar PythonQLParser;

// All comments that start with "///" are copy-pasted from
// The Python Language Reference: https://docs.python.org/3.3/reference/grammar.html

options { tokenVocab = PythonQLLexer; }

/*
 * parser rules
 */

/// single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
single_input
 : NEWLINE
 | simple_stmt
 | compound_stmt NEWLINE
 ;

/// file_input: (NEWLINE | stmt)* ENDMARKER
file_input
 : ( NEWLINE | stmt )* EOF
 ;

/// eval_input: testlist NEWLINE* ENDMARKER
eval_input
 : testlist NEWLINE* EOF
 ;

/// decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
decorator
 : '@' dotted_name ( '(' arglist? ')' )? NEWLINE
 ;

/// decorators: decorator+
decorators
 : decorator+
 ;

/// decorated: decorators (classdef | funcdef)
decorated
 : decorators ( classdef | funcdef )
 ;

/// funcdef: 'def' NAME parameters ['->' test] ':' suite
funcdef
 : DEF NAME parameters ( '->' test )? ':' suite
 ;

/// parameters: '(' [typedargslist] ')'
parameters
 : '(' typedargslist? ')'
 ;

/// typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [','
///                ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]]
///              |  '*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef)
typedargslist
 : tfpdef ( '=' test )? ( ',' tfpdef ( '=' test )? )* ( ',' ( '*' tfpdef? ( ',' tfpdef ( '=' test )? )* ( ',' '**' tfpdef )? 
                                                            | '**' tfpdef 
                                                            )? 
                                                      )?
 | '*' tfpdef? ( ',' tfpdef ( '=' test )? )* ( ',' '**' tfpdef )? 
 | '**' tfpdef
 ;

/// tfpdef: NAME [':' test]
tfpdef
 : NAME ( ':' test )?
 ;

/// varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [','
///       ['*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef]]
///     |  '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef)
varargslist
 : vfpdef ( '=' test )? ( ',' vfpdef ( '=' test )? )* ( ',' ( '*' vfpdef? ( ',' vfpdef ( '=' test )? )* ( ',' '**' vfpdef )? 
                                                            | '**' vfpdef 
                                                            )? 
                                                      )?
 | '*' vfpdef? ( ',' vfpdef ( '=' test )? )* ( ',' '**' vfpdef )?
 | '**' vfpdef
 ;

/// vfpdef: NAME
vfpdef
 : NAME
 ;

/// stmt: simple_stmt | compound_stmt
stmt
 : simple_stmt 
 | compound_stmt
 ;

/// simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
simple_stmt
 : small_stmt ( ';' small_stmt )* ';'? NEWLINE
 ;

/// small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
///              import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
small_stmt
 : expr_stmt 
 | del_stmt 
 | pass_stmt 
 | flow_stmt 
 | import_stmt 
 | global_stmt 
 | nonlocal_stmt 
 | assert_stmt
 ;

/// expr_stmt: testlist_star_expr (augassign (yield_expr|testlist) |
///                      ('=' (yield_expr|testlist_star_expr))*)
expr_stmt
 : testlist_star_expr ( augassign ( yield_expr | testlist) 
                      | ( '=' ( yield_expr| testlist_star_expr ) )*
                      )
 ;           

/// testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
testlist_star_expr
 : ( test | star_expr ) ( ',' ( test |  star_expr ) )* ','?
 ;

/// augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
///             '<<=' | '>>=' | '**=' | '//=')
augassign
 : '+=' 
 | '-=' 
 | '*=' 
 | '@=' // PEP 465
 | '/=' 
 | '%=' 
 | '&=' 
 | '|=' 
 | '^=' 
 | '<<=' 
 | '>>=' 
 | '**=' 
 | '//='
 ;

/// del_stmt: 'del' exprlist
del_stmt
 : DEL exprlist
 ;

/// pass_stmt: 'pass'
pass_stmt
 : PASS
 ;

/// flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
flow_stmt
 : break_stmt 
 | continue_stmt 
 | return_stmt 
 | raise_stmt 
 | yield_stmt
 ;

/// break_stmt: 'break'
break_stmt
 : BREAK
 ;

/// continue_stmt: 'continue'
continue_stmt
 : CONTINUE
 ;

/// return_stmt: 'return' [testlist]
return_stmt
 : RETURN testlist?
 ;

/// yield_stmt: yield_expr
yield_stmt
 : yield_expr
 ;

/// raise_stmt: 'raise' [test ['from' test]]
raise_stmt
 : RAISE ( test ( FROM test )? )?
 ;

/// import_stmt: import_name | import_from
import_stmt
 : import_name 
 | import_from
 ;

/// import_name: 'import' dotted_as_names
import_name
 : IMPORT dotted_as_names
 ;

/// # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
/// import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
///               'import' ('*' | '(' import_as_names ')' | import_as_names))
import_from
 : FROM ( ( '.' | '...' )* dotted_name 
        | ('.' | '...')+ 
        )
   IMPORT ( '*' 
          | '(' import_as_names ')' 
          | import_as_names
          )         
 ;

/// import_as_name: NAME ['as' NAME]
import_as_name
 : NAME ( AS NAME )?
 ;

/// dotted_as_name: dotted_name ['as' NAME]
dotted_as_name
 : dotted_name ( AS NAME )?
 ;

/// import_as_names: import_as_name (',' import_as_name)* [',']
import_as_names
 : import_as_name ( ',' import_as_name )* ','?
 ;

/// dotted_as_names: dotted_as_name (',' dotted_as_name)*
dotted_as_names
 : dotted_as_name ( ',' dotted_as_name )*
 ;

/// dotted_name: NAME ('.' NAME)*
dotted_name
 : NAME ( '.' NAME )*
 ;

/// global_stmt: 'global' NAME (',' NAME)*
global_stmt
 : GLOBAL NAME ( ',' NAME )*
 ;

/// nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
nonlocal_stmt
 : NONLOCAL NAME ( ',' NAME )*
 ;

/// assert_stmt: 'assert' test [',' test]
assert_stmt
 : ASSERT test ( ',' test )?
 ;

/// compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
compound_stmt
 : if_stmt 
 | while_stmt 
 | for_stmt 
 | try_stmt 
 | with_stmt 
 | funcdef 
 | classdef 
 | decorated
 ;

/// if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
if_stmt
 : IF test ':' suite ( ELIF test ':' suite )* ( ELSE ':' suite )?
 ;

/// while_stmt: 'while' test ':' suite ['else' ':' suite]
while_stmt
 : WHILE test ':' suite ( ELSE ':' suite )?
 ;

/// for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
for_stmt
 : FOR exprlist IN testlist ':' suite ( ELSE ':' suite )?
 ;

/// try_stmt: ('try' ':' suite
///            ((except_clause ':' suite)+
///       ['else' ':' suite]
///       ['finally' ':' suite] |
///      'finally' ':' suite))
try_stmt
 : TRY ':' suite ( ( except_clause ':' suite )+ 
                   ( ELSE ':' suite )? 
                   ( FINALLY ':' suite )?
                 | FINALLY ':' suite
                 )
 ;

/// with_stmt: 'with' with_item (',' with_item)*  ':' suite
with_stmt
 : WITH with_item ( ',' with_item )* ':' suite
 ;

/// with_item: test ['as' expr]
with_item
 : test ( AS expr )?
 ;

/// # NB compile.c makes sure that the default except clause is last
/// except_clause: 'except' [test ['as' NAME]]
except_clause
 : EXCEPT ( test ( AS NAME )? )?
 ;

/// suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
suite
 : simple_stmt 
 | NEWLINE INDENT stmt+ DEDENT
 ;

// Path step of the query language
test
 : try_catch_expr (path_step)*
 ;

path_step
  : child_path_step
  | desc_path_step
  ;

child_path_step: './' try_catch_expr;
desc_path_step: './/' try_catch_expr;

try_catch_expr
  : old_test
  | 'try'  old_test  'except' old_test 
  ;

opt_exception: old_test?;

/// old_test: or_test ['if' or_test 'else' old_test] | lambdef
old_test
 : or_test ( IF or_test ELSE old_test )?
 | lambdef
 ;

/// test_nocond: or_test | lambdef_nocond
test_nocond
 : or_test 
 | lambdef_nocond
 ;

/// lambdef: 'lambda' [varargslist] ':' test
lambdef
 : LAMBDA varargslist? ':' test
 ;

/// lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
lambdef_nocond
 : LAMBDA varargslist? ':' test_nocond
 ;

/// or_test: and_test ('or' and_test)*
or_test
 : and_test ( OR and_test )*
 ;

/// and_test: not_test ('and' not_test)*
and_test
 : not_test ( AND not_test )*
 ;

/// not_test: 'not' not_test | comparison
not_test
 : NOT not_test 
 | comparison
 ;

/// comparison: star_expr (comp_op star_expr)*
comparison
 : star_expr ( comp_op star_expr )*
 ;

/// # <> isn't actually a valid comparison operator in Python. It's here for the
/// # sake of a __future__ import described in PEP 401
/// comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
comp_op
 : '<'
 | '>'
 | '=='
 | '>='
 | '<='
 | '<>'
 | '!='
 | IN
 | NOT IN
 | IS
 | IS NOT
 ;

/// star_expr: ['*'] expr
star_expr
 : '*'? expr
 ;

/// expr: xor_expr ('|' xor_expr)*
expr
 : xor_expr ( '|' xor_expr )*
 ;

/// xor_expr: and_expr ('^' and_expr)*
xor_expr
 : and_expr ( '^' and_expr )*
 ;

/// and_expr: shift_expr ('&' shift_expr)*
and_expr
 : shift_expr ( '&' shift_expr )*
 ;

/// shift_expr: arith_expr (('<<'|'>>') arith_expr)*
shift_expr
 : arith_expr ( '<<' arith_expr 
              | '>>' arith_expr 
              )*
 ;

/// arith_expr: term (('+'|'-') term)*
arith_expr
 : term ( '+' term
        | '-' term 
        )*
 ;

/// term: factor (('*'|'/'|'%'|'//') factor)*
term
 : factor ( '*' factor
          | '/' factor
          | '%' factor 
          | '//' factor 
          | '@' factor // PEP 465
          )*
 ;

/// factor: ('+'|'-'|'~') factor | power
factor
 : '+' factor 
 | '-' factor 
 | '~' factor 
 | power
 ;

/// power: atom trailer* ['**' factor]
power
 : atom trailer* ( '**' factor )?
 ;

/// atom: ('(' [yield_expr|testlist_comp] ')' |
///        '[' [testlist_comp] ']' |
///        '{' [dictorsetmaker] '}' |
///        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
atom :
// '(' ( yield_expr | testlist_comp )? ')' 
// | '[' testlist_comp? ']'  
// | '{' dictorsetmaker? '}' 
  NAME 
 | number 
 | string+ 
 | '...' 
 | NONE
 | TRUE
 | FALSE
 | gen_query_expression 
 | list_query_expression 
 | set_query_expression
 ;

// This is our addition to the grammar, the Query Expression
gen_query_expression
 : '(' ( yield_expr | testseq_query )? ')'
 ;

list_query_expression
 : '[' testlist_query ? ']'
 ;

set_query_expression
 : '{' dictorsetmaker? '}'
 ;

query_expression: 
  select_clause
  (for_clause|let_clause|window_clause)
  (for_clause|let_clause|window_clause|
      group_by_clause|where_clause|order_by_clause|count_clause)*
  ;

query_map_expression: 
  map_select_clause
  (for_clause|let_clause|window_clause)
  (for_clause|let_clause|window_clause|
      group_by_clause|where_clause|order_by_clause|count_clause)*
  ;

select_clause: ('select')? test
;

map_select_clause: ('select')? test ':' test
;

for_clause: 'for' for_clause_entry (',' for_clause_entry)*
;

for_clause_entry
  : exprlist 'in' test 
  ;

let_clause: 'let' let_clause_entry (',' let_clause_entry)*
;

let_clause_entry
  : NAME '=' test 
  ;

window_clause: tumbling_window | sliding_window;

tumbling_window: 'for' 'tumbling' 'window' NAME 'in' test
                 window_start_cond (window_end_cond)?;

sliding_window: 'for' 'sliding' 'window' NAME 'in' test
		window_start_cond window_end_cond;

window_start_cond: 'start' window_vars 'when' test;

window_end_cond: opt_only 'end' window_vars 'when' test;

opt_only: ('only')?;

window_vars: current_item? positional_var? previous_var? next_var?;

current_item: NAME;
positional_var: 'at' NAME;
previous_var: 'previous' NAME;
next_var: 'following' NAME;

order_by_clause: 'order' 'by' orderlist
;

orderlist
  : orderlist_el (',' orderlist_el)*
  ;

orderlist_el
  : test ('asc'|'desc')?
  ;

group_by_clause: 'group' 'by' group_by_vars
;

group_by_vars
  : group_by_var (',' group_by_var)*
  ;

group_by_var: old_test ('as' NAME)?;

where_clause: ('where'|'if') test
;

count_clause: 'count' NAME;

testseq_query
 : test_as (',' test_as )* ','?
 | query_expression;

test_as
 : test ('as' NAME)?
 ;

testlist_query
 : test (',' test)* ','?
 | query_expression;

/// trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
trailer
 : '(' arglist? ')' 
 | '[' subscriptlist ']' 
 | '.' NAME
 ;

/// subscriptlist: subscript (',' subscript)* [',']
subscriptlist
 : subscript ( ',' subscript )* ','?
 ;

/// subscript: test | [test] ':' [test] [sliceop]
subscript
 : test 
 | test? ':' test? sliceop?
 ;

/// sliceop: ':' [test]
sliceop
 : ':' test?
 ;

/// exprlist: star_expr (',' star_expr)* [',']
exprlist
 : star_expr ( ',' star_expr )* ','?
 ;

/// testlist: test (',' test)* [',']
testlist
 : test ( ',' test )* ','?
 ;

/// dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
///                   (test (comp_for | (',' test)* [','])) )
dictorsetmaker
 : test ':' test ( ',' test ':' test )* ','? 
 | query_map_expression 
 | test ( ',' test )* ','? 
 | query_expression
 ;

/// classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
classdef
 : CLASS NAME ( '(' arglist? ')' )? ':' suite
 ;

/// arglist: (argument ',')* (argument [',']
///                          |'*' test (',' argument)* [',' '**' test]
///                          |'**' test)
arglist
 : ( argument ',' )* ( argument ','?
                     | '*' test ( ',' argument )* ( ',' '**' test )?
                     | '**' test
                     )
 ;

/// # The reason that keywords are test nodes instead of NAME is that using NAME
/// # results in an ambiguity. ast.c makes sure it's a NAME.
/// argument: test [comp_for] | test '=' test  # Really [keyword '='] test
argument
 : test comp_for? 
 | test '=' test
 ;

/// comp_iter: comp_for | comp_if
comp_iter
 : comp_for 
 | comp_if
 ;

/// comp_for: 'for' exprlist 'in' or_test [comp_iter]
comp_for
 : FOR exprlist IN or_test comp_iter?
 ;

/// comp_if: 'if' test_nocond [comp_iter]
comp_if
 : IF test_nocond comp_iter?
 ;

/// yield_expr: 'yield' [testlist]
yield_expr
 : YIELD yield_arg?
 ;

/// yield_arg: 'from' test | testlist
yield_arg
 : FROM test 
 | testlist
 ;

string
 : STRING_LITERAL
 | BYTES_LITERAL
 ;

number
 : integer
 | FLOAT_NUMBER
 | IMAG_NUMBER
 ;

/// integer        ::=  decimalinteger | octinteger | hexinteger | bininteger
integer
 : DECIMAL_INTEGER
 | OCT_INTEGER
 | HEX_INTEGER
 | BIN_INTEGER
 ;
