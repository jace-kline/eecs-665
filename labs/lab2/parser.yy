/* Definitions and Declarations */
%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define api.namespace {LAB2}
%define api.parser.class {Parser}
/* %define parser_class_name {Parser} */
%output "parser.cc"
%token-table

%code requires{
    namespace LAB2 {
        class Manager;
        class Scanner;
    }

#ifndef YY_NULLPTR
#define YY_NULLPTR 0
#endif
}

%parse-param { Scanner &scanner }
%parse-param { Manager &manager }

%code {
    #include <iostream>
    #include <string>
    #include "string.h"
    #include <fstream>
    #include <cstdlib>
    #include "calc.hpp"

    #undef yylex
    #define yylex scanner.yylex
}

/* %define parser.assert */

%union {
    bool boolval;
    int intval;
}

%token END 0
%token LPAR
%token RPAR
%token PLUS
%token MINUS
%token DIV
%token MULT
%token <intval> INTLIT

%type <boolval> Expr
%type <intval> E
%type <intval> T
%type <intval> F
%type <intval> G
%type <intval> P

%%
/* CF-Productions and actions */
Expr : E END    { 
        std::cout << "Expression value: " << $1 << "\n";
        $$ = true;
    }
E : E PLUS T    { $$ = $1 + $3; }
  | T           { $$ = $1; }
T : T MINUS F   { $$ = $1 - $3; }
  | F           { $$ = $1; }
F : F DIV G     { $$ = $1 / $3; }
  | G           { $$ = $1; }
G : G MULT P    { $$ = $1 * $3; }
  | P           { $$ = $1; }
P : INTLIT      { $$ = $1; }
  | LPAR E RPAR { $$ = $2; }

%%
/* Program stub -> code placed at end of parser.cc file */

/* We must implement the error method for the Parser class */
void LAB2::Parser::error(const std::string& err_msg) {
    std::cerr << "Parse error: " << err_msg << std::endl;
}