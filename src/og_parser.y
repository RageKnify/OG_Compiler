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

%nonassoc tIFX tFOR
%nonassoc tTHEN tDO
%nonassoc tELIF tELSE

%right '='
%nonassoc ','
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY '['
// %nonassoc tPRIMARY

%type <s> string
%type <node> stmt vardec funcdec argdec localdec ifcontent dec
%type <block_node> block
%type <sequence> stmts exprs argdecs decs file
%type <expression> expr
%type <lvalue> lval
%type <type> type
%type <identifiers> identifiers

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : dec      { $$ = new cdk::sequence_node(LINE, $1); }
     | file dec { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

dec : vardec ';'    { $$ = $1; }
    | funcdec       { $$ = $1; }
    ;

vardec :          type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), NULL); }
       |          type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), $4); }
       | tPUBLIC  type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  $2, new std::vector<std::string*>({$3}), NULL); }
       | tPUBLIC  type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  $2, new std::vector<std::string*>({$3}), $5); }
       | tREQUIRE type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tREQUIRE, $2, new std::vector<std::string*>({$3}), NULL); }
       | tREQUIRE type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tREQUIRE, $2, new std::vector<std::string*>({$3}), $5); }
       |          tAUTO identifiers '=' exprs  { $$ = new og::variable_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $2, new og::tuple_node(LINE, $4)); }
       | tPUBLIC  tAUTO identifiers '=' exprs  { $$ = new og::variable_declaration_node(LINE, tPUBLIC,  new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3, new og::tuple_node(LINE, $5)); }
       ;

funcdec :          type       tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tPRIVATE, $1, $2); }
        |          tPROCEDURE tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2); }
        |          tAUTO      tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $2); }
        |          type       tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tPRIVATE, $1, $2, $4); }
        |          tPROCEDURE tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2, $4); }
        |          tAUTO      tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $2, $4); }
        | tPUBLIC  type       tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tPUBLIC, $2, $3); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3); }
        | tPUBLIC  type       tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tPUBLIC, $2, $3, $5); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $5); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3, $5); }
        | tREQUIRE type       tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tREQUIRE, $2, $3); }
        | tREQUIRE tPROCEDURE tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tREQUIRE, new cdk::primitive_type(0, cdk::TYPE_VOID), $3); }
        | tREQUIRE tAUTO      tIDENTIFIER '(' ')'               { $$ = new og::function_declaration_node(LINE, tREQUIRE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3); }
        | tREQUIRE type       tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tREQUIRE, $2, $3, $5); }
        | tREQUIRE tPROCEDURE tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tREQUIRE, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $5); }
        | tREQUIRE tAUTO      tIDENTIFIER '(' argdecs ')'       { $$ = new og::function_declaration_node(LINE, tREQUIRE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3, $5); }
        |          type       tIDENTIFIER '(' ')' block         { $$ = new og::function_definition_node(LINE, tPRIVATE, $1, $2, $5); }
        |          tPROCEDURE tIDENTIFIER '(' ')' block         { $$ = new og::function_definition_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2, $5); }
        |          tAUTO      tIDENTIFIER '(' ')' block         { $$ = new og::function_definition_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $2, $5); }
        |          type       tIDENTIFIER '(' argdecs ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, $1, $2, $4, $6); }
        |          tPROCEDURE tIDENTIFIER '(' argdecs ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_VOID), $2, $4, $6); }
        |          tAUTO      tIDENTIFIER '(' argdecs ')' block { $$ = new og::function_definition_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $2, $4, $6); }
        | tPUBLIC  type       tIDENTIFIER '(' ')' block         { $$ = new og::function_definition_node(LINE, tPUBLIC, $2, $3, $6); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' ')' block         { $$ = new og::function_definition_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $6); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' ')' block         { $$ = new og::function_definition_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3, $6); }
        | tPUBLIC  type       tIDENTIFIER '(' argdecs ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, $2, $3, $5, $7); }
        | tPUBLIC  tPROCEDURE tIDENTIFIER '(' argdecs ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_VOID), $3, $5, $7); }
        | tPUBLIC  tAUTO      tIDENTIFIER '(' argdecs ')' block { $$ = new og::function_definition_node(LINE, tPUBLIC, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $3, $5, $7); }
        ;

exprs : expr           { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

argdec : type  tIDENTIFIER      { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), NULL); }
       ;

argdecs : argdec                { $$ = new cdk::sequence_node(LINE, $1); }
        | argdecs ',' argdec    { $$ = new cdk::sequence_node(LINE, $3, $1); }
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

decs : localdec        { $$ = new cdk::sequence_node(LINE, $1); }
     | decs localdec   { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

localdec : type  tIDENTIFIER            { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), NULL); }
         | type  tIDENTIFIER '=' expr   { $$ = new og::variable_declaration_node(LINE, tPRIVATE, $1, new std::vector<std::string*>({$2}), $4); }
         | tAUTO identifiers '=' exprs  { $$ = new og::variable_declaration_node(LINE, tPRIVATE, new cdk::primitive_type(0, cdk::TYPE_UNSPEC), $2, new og::tuple_node(LINE, $4)); }
         ;

stmts : stmt         { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt   { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

stmt : expr ';'                                 { $$ = new og::evaluation_node(LINE, $1); }
     | tPRINT exprs ';'                         { $$ = new og::print_node(LINE, $2); }
     | tPRINTLN exprs ';'                       { $$ = new og::print_node(LINE, $2, true); }
     | tFOR decs ';' exprs ';' exprs tDO stmt   { $$ = new og::for_node(LINE, $2, $4, $6, $8); }
     | tFOR exprs ';' exprs ';' exprs tDO stmt  { $$ = new og::for_node(LINE, $2, $4, $6, $8); }
     | tIF ifcontent                            { $$ = $2; }
     | tBREAK                                   { $$ = new og::break_node(LINE); }
     | tCONTINUE                                { $$ = new og::continue_node(LINE); }
     | tRETURN ';'                              { $$ = new og::return_node(LINE, NULL); }
     | tRETURN expr  ';'                        { $$ = new og::return_node(LINE, $2); }
     | tRETURN exprs ';'                        { $$ = new og::return_node(LINE, new og::tuple_node(LINE, $2)); }
     ;

ifcontent : expr tTHEN stmt %prec tIFX          { $$ = new og::if_node(LINE, $1, $3); }
          | expr tTHEN stmt tELIF ifcontent     { $$ = new og::if_else_node(LINE, $1, $3, $5); }
          | expr tTHEN stmt tELSE stmt          { $$ = new og::if_else_node(LINE, $1, $3, $5); }
          ;

expr : tINTEGER                     { $$ = new cdk::integer_node(LINE, $1); }
     | string                       { $$ = new cdk::string_node(LINE, $1); }
     | '-' expr %prec tUNARY        { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUNARY        { $$ = new og::identity_node(LINE, $2); }
     | lval '?' %prec tUNARY        { $$ = new og::address_of_node(LINE, $1); }
     | '~' expr                     { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr                { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr                { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr                { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr                { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr                { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr                { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr                { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr                { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr                { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr                { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tOR expr                { $$ = new cdk::or_node(LINE, $1, $3); }
     | expr tAND expr               { $$ = new cdk::and_node(LINE, $1, $3); }
     | '[' expr ']'                 { $$ = new og::memory_reservation_node(LINE, $2); }
     | '(' expr ')'                 { $$ = $2; }
     | tSIZEOF '(' expr ')'         { $$ = new og::sizeof_node(LINE, $3); }
     | tSIZEOF '(' exprs ')'        { $$ = new og::sizeof_node(LINE, new og::tuple_node(LINE, $3)); }
     | tREAD                        { $$ = new og::read_node(LINE); }
     | lval                         { $$ = new cdk::rvalue_node(LINE, $1); }  //FIXME
     | lval '=' expr                { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | tIDENTIFIER '(' ')'          { $$ = new og::function_call_node(LINE, $1); }
     | tIDENTIFIER '(' exprs ')'    { $$ = new og::function_call_node(LINE, $1, $3); }
     ;

string : string tSTRING         { $1->append(*$2); $$ = $1; delete $2; }
       | tSTRING                { $$ = $1; }
       ;

lval : tIDENTIFIER              { $$ = new cdk::variable_node(LINE, $1); }
     | expr '[' expr ']'        { $$ = new og::pointer_index_node(LINE, $1, $3); }
     ;

%%
