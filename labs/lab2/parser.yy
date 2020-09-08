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

%token END 0
%token LPAR
%token RPAR
%token PLUS
%token MINUS
%token DIV
%token MULT
%token INTLIT

%%
/* CF-Productions and actions */
P: INTLIT

%%
/* Program stub -> code placed at end of parser.cc file */