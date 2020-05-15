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
EXP [eE]-?[0-9]+

%x X_STRING X_STRING_IGNORE X_COMMENT
%%

"//".*                 ; /* ignore comments */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>.|\n        ; /* ignore comments */

">="                   return tGE;
"<="                   return tLE;
"=="                   return tEQ;
"!="                   return tNE;

"||"                   return tOR;
"&&"                   return tAND;

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
<X_STRING>\\\\         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");
<X_STRING>\\n          *yylval.s += '\n';
<X_STRING>\\t          *yylval.s += '\t';
<X_STRING>\\r          *yylval.s += '\r';
<X_STRING>\\([a-fA-F0-9]{2})     |
<X_STRING>\\[a-fA-F0-9]          {
                                     char c = std::stoi(yytext+1, NULL, 16);
                                     if (c == 0) {
                                         yy_push_state(X_STRING_IGNORE);
                                     } else {
                                         *yylval.s += c;
                                     }
                                 }
<X_STRING>\\.          *yylval.s += yytext + 1;

<X_STRING_IGNORE>\"           yy_pop_state(); yyless(0);
<X_STRING_IGNORE>.|\\\"       ;
<X_STRING_IGNORE>\n           yyerror("newline in string");

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

[0-9]*\.[0-9]+{EXP}?   |
[0-9]{EXP}             |
[0-9]\.{EXP}?          {
    try {
        yylval.d = std::stod(yytext);
        return tREAL;
    } catch (const std::out_of_range&) {
      yyerror("double precision float oveflow");
    }
}

[-()<>=+*/%;,{}.@?[\]]  return *yytext;

[ \t\n\r]+             ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
