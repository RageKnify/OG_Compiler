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
  int                        i;	/* integer value */
  double                     d;
  std::string                *s;	/* symbol name or string literal */
  cdk::basic_node            *node;	/* node pointer */
  cdk::sequence_node         *sequence;
  cdk::expression_node       *expression; /* expression nodes */
  cdk::lvalue_node           *lvalue;
  cdk::basic_type            *type;
  std::vector<std::string*>  *identifiers;
  og::block_node             *block_node;
};

%token <i> tINTEGER
%token <d> tREAL
%token <s> tIDENTIFIER tSTRING
%token tAUTO tINT_TYPE tREAL_TYPE tSTRING_TYPE tPTR
%token tFOR tDO tBREAK tCONTINUE
%token tPUBLIC tPRIVATE tREQUIRE tPROCEDURE tRETURN tSIZEOF tNULLPTR
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

%type <node> stmt vardec funcdec
%type <block_node> block
%type <sequence> stmts exprs vardecs decs
%type <expression> expr
%type <lvalue> lval
%type <type> type
%type <identifiers> identifiers

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

vardec :          type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), NULL); }
       |          type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), $4); }
       | tPUBLIC  type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  $2, new std::vector<std::string*>({$3}), NULL); }
       | tPUBLIC  type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  $2, new std::vector<std::string*>({$3}), $5); }
       | tREQUIRE type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tREQUIRE, $2, new std::vector<std::string*>({$3}), NULL); }
       | tREQUIRE type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tREQUIRE, $2, new std::vector<std::string*>({$3}), $5); }
       |          tAUTO identifiers '=' exprs  { $$ = new og::variable_declaration_node(LINE, tPRIVATE, NULL, $2, new og::tuple_node(LINE, $4)); }
       | tPUBLIC  tAUTO identifiers '=' exprs  { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  NULL, $3, new og::tuple_node(LINE, $5)); }
       ;

funcdec :          type       tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tPRIVATE, $1, $2); }
        |          tPROCEDURE tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2); }
        |          tAUTO      tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tPRIVATE, NULL, $2); }
        |          type       tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tPRIVATE, $1, $2, $4); }
        |          tPROCEDURE tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2, $4); }
        |          tAUTO      tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tPRIVATE, NULL, $2, $4); }
        | tPUBLIC  type       tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tPUBLIC, $2, $3); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tPUBLIC, NULL, $3); }
        | tPUBLIC  type       tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tPUBLIC, $2, $3, $5); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $5); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tPUBLIC, NULL, $3, $5); }
        | tREQUIRE type       tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tREQUIRE, $2, $3); }
        | tREQUIRE tPROCEDURE tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tREQUIRE, new cdk::primitive_type(0, cdk::TYPE_VOID), $3); }
        | tREQUIRE tAUTO      tIDENTIFIER '(' ')' { $$ = new og::function_declaration_node(LINE, tREQUIRE, NULL, $3); }
        | tREQUIRE type       tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tREQUIRE, $2, $3, $5); }
        | tREQUIRE tPROCEDURE tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tREQUIRE, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $5); }
        | tREQUIRE tAUTO      tIDENTIFIER '(' vardecs ')' { $$ = new og::function_declaration_node(LINE, tREQUIRE, NULL, $3, $5); }
        |          type       tIDENTIFIER '(' ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, $1, $2, $5); }
        |          tPROCEDURE tIDENTIFIER '(' ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2, $5); }
        |          tAUTO      tIDENTIFIER '(' ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, NULL, $2, $5); }
        |          type       tIDENTIFIER '(' vardecs ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, $1, $2, $4, $6); }
        |          tPROCEDURE tIDENTIFIER '(' vardecs ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2, $4, $6); }
        |          tAUTO      tIDENTIFIER '(' vardecs ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, NULL, $2, $4, $6); }
        | tPUBLIC  type       tIDENTIFIER '(' ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, $2, $3, $6); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $6); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, NULL, $3, $6); }
        | tPUBLIC  type       tIDENTIFIER '(' vardecs ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, $2, $3, $5, $7); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' vardecs ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $5, $7); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' vardecs ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, NULL, $3, $5, $7); }
        ;

exprs : expr           { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

vardecs : vardec                { $$ = new cdk::sequence_node(LINE, $1); }
        | vardecs ',' vardec    { $$ = new cdk::sequence_node(LINE, $3, $1); }
        ;

type : tINT_TYPE           { $$ = new cdk::primitive_type(4, cdk::TYPE_INT); }
                                    // { $$ = cdk::make_primitive_type(4, cdk::TYPE_INT); }
     | tREAL_TYPE          { $$ = new cdk::primitive_type(8, cdk::TYPE_DOUBLE); }
                                    // { $$ = cdk::make_primitive_type(8, cdk::TYPE_DOUBLE); }
     | tSTRING_TYPE        { $$ = new cdk::primitive_type(4, cdk::TYPE_STRING); }
                                    // { $$ = cdk::make_primitive_type(4, cdk::TYPE_STRING); }
     | tPTR '<' type '>'   { $$ = new cdk::reference_type(4, std::shared_ptr<cdk::basic_type>($3)); }
                                    // { $$ = cdk::make_reference_type(4, $3); }
     | tPTR '<' tAUTO '>'  { $$ = new cdk::reference_type(4, cdk::make_primitive_type(0, cdk::TYPE_VOID)); }
                                    // { $$ = cdk::make_reference_type(4, cdk::make_primitive_type(0, cdk::TYPE_VOID)); }
     ;

identifiers : identifiers ',' tIDENTIFIER { $1->push_back($3); $$ = $1; }
            | tIDENTIFIER                 { $$ = new std::vector<std::string*>({$1}); }
            ;

block : '{' decs stmts '}'     { $$ = new og::block_node(LINE, $2, $3); }
      | '{' decs '}'           { $$ = new og::block_node(LINE, $2, NULL); }
      | '{' stmts '}'          { $$ = new og::block_node(LINE, NULL, $2); }
      ;

decs : vardec        { $$ = new cdk::sequence_node(LINE, $1); }
     | funcdec       { $$ = new cdk::sequence_node(LINE, $1); }
     | decs vardec   { $$ = new cdk::sequence_node(LINE, $2, $1); }
     | decs funcdec   { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

stmts : stmt         { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt   { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

stmt : expr ';'                         { $$ = new og::evaluation_node(LINE, $1); }
     | tPRINT expr ';'                  { $$ = new og::print_node(LINE, $2); }
     | tREAD ';'                        { $$ = new og::read_node(LINE); }
     | tFOR exprs ';' exprs ';' exprs tDO stmt { $$ = new og::for_node(LINE, $2, $4, $6, $8); }
     | tIF '(' expr ')' stmt %prec tIFX { $$ = new og::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt { $$ = new og::if_else_node(LINE, $3, $5, $7); }
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
