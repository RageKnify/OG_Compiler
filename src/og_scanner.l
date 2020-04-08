%option c++ prefix="og_scanner_" outfile="og_scanner.cpp"
%option stack noyywrap yylineno 8bit
%{
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "og_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
%x X_STRING
%%

"--".*                 ; /* ignore comments */

">="                   return tGE;
"<="                   return tLE;
"=="                   return tEQ;
"!="                   return tNE;

"auto"                 return tAUTO;
"int"                  return tINT_TYPE;
"real"                 return tREAL_TYPE;
"string"               return tSTRING_TYPE;
"ptr"                  return tPTR;

"for"                  return tFOR;
"do"                   return tDO;
"break"                return tBREAK;
"continue"             return tCONTINUE;

"public"               return tPUBLIC;
"require"              return tREQUIRE;
"procedure"            return tPROCEDURE;
"return"               return tRETURN;

"sizeof"               return tSIZEOF;
"nullptr"              return tNULLPTR;

"input"                return tREAD;
"write"                return tPRINT;
"writeln"              return tPRINTLN;

"if"                   return tIF;
"then"                 return tTHEN;
"elif"                 return tELIF;
"else"                 return tELSE;

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"           yy_pop_state(); return tSTRING;
<X_STRING>\\\"         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");

[0-9]+                 {
    try {
        yylval.i = std::stoi(yytext, nullptr, 10);
        return tINTEGER;
    } catch(const std::out_of_range&) {
        yyerror("integer overflow");
    }
}

0x[0-9a-fA-F]+         {
    try {
        yylval.i = std::stoi(yytext, nullptr, 16);
        return tINTEGER;
    } catch(const std::out_of_range&) {
        yyerror("integer overflow");
    }
}

[-()<>=+*/%;{}.@?[\]]  return *yytext;

[ \t\n\r]+             ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
