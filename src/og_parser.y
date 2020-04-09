%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <cdk/compiler.h>
#include "ast/all.h"
#define LINE               compiler->scanner()->lineno()
#define yylex()            compiler->scanner()->scan()
#define yyerror(s)         compiler->scanner()->error(s)
#define YYPARSE_PARAM_TYPE std::shared_ptr<cdk::compiler>
#define YYPARSE_PARAM      compiler
//-- don't change *any* of these --- END!
%}

%union {
  int                   i;	/* integer value */
  double                d;
  std::string           *s;	/* symbol name or string literal */
  cdk::basic_node       *node;	/* node pointer */
  cdk::sequence_node    *sequence;
  cdk::expression_node  *expression; /* expression nodes */
  cdk::lvalue_node      *lvalue;
};

%token <i> tINTEGER
%token <d> tREAL
%token <s> tIDENTIFIER tSTRING
%token tAUTO tINT_TYPE tREAL_TYPE tSTRING_TYPE tPTR
%token tFOR tDO tBREAK tCONTINUE
%token tPUBLIC tREQUIRE tPROCEDURE tRETURN tSIZEOF tNULLPTR
%token tREAD tPRINT tPRINTLN
%token tIF tTHEN tELIF tELSE
%token tOR tAND

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
// %nonassoc tPRIMARY

%type <node> stmt
%type <sequence> list exprs
%type <expression> expr
%type <lvalue> lval

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

list : stmt         { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt    { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

exprs : expr           { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

type : tINT_TYPE
     | tREAL_TYPE
     | tSTRING_TYPE
     | tPTR '<' type '>'
     | tPTR '<' tAUTO '>'
     ;

stmt : expr ';'                         { $$ = new og::evaluation_node(LINE, $1); }
     | tPRINT expr ';'                  { $$ = new og::print_node(LINE, $2); }
     | tREAD ';'                        { $$ = new og::read_node(LINE); }
     | tFOR exprs ';' exprs ';' exprs tDO stmt { $$ = new og::for_node(LINE, $2, $4, $6, $8); }
     | tIF '(' expr ')' stmt %prec tIFX { $$ = new og::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt { $$ = new og::if_else_node(LINE, $3, $5, $7); }
     | '{' list '}'                     { $$ = $2; }
     ;

expr : tINTEGER                 { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING                  { $$ = new cdk::string_node(LINE, $1); }
     | '-' expr %prec tUNARY    { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUNARY    { $$ = new og::identity_node(LINE, $2); }
     | lval '?' %prec tUNARY    { $$ = new og::address_of_node(LINE, $1); }
     | '~' expr                 { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr            { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr            { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr            { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr            { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr            { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr            { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr            { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr            { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr            { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr            { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr            { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tOR expr            { $$ = new cdk::or_node(LINE, $1, $3); }
     | expr tAND expr           { $$ = new cdk::and_node(LINE, $1, $3); }
     | '[' expr ']'             { $$ = new og::memory_reservation_node(LINE, $2); }
     | '(' expr ')'             { $$ = $2; }
     | lval                     { $$ = new cdk::rvalue_node(LINE, $1); }  //FIXME
     | lval '=' expr            { $$ = new cdk::assignment_node(LINE, $1, $3); }
     ;

lval : tIDENTIFIER              { $$ = new cdk::variable_node(LINE, $1); }
     ;

%%
