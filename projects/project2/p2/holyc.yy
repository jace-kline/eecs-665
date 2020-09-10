%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define api.namespace {holyc}
 /*
 If your bison install has trouble with the 
 line %define api.parser.class {Parser} try
 using the older %define parser_class_name {Parser}
 instead
 */
%define api.parser.class {Parser}
%define parse.assert
%define parse.error verbose
%output "parser.cc"
%token-table

%code requires{
	#include "helper.hpp"
	#include "tokens.hpp"
	namespace holyc {
		class Scanner;
	}

//The following definition is required when 
// we don't use the %locations directive (which we won't)
# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

//End "requires" code
}

%parse-param { holyc::Scanner &scanner }

%code{
   // C std code for utility functions
   #include <iostream>
   #include <cstdlib>
   #include <fstream>

   // Our code for interoperation between scanner/parser
   #include "scanner.hpp"

   // Include our ParseTree class
   // #include "parsetree.hpp"

  //Request tokens from our scanner member, not 
  // from a global function
  #undef yylex
  #define yylex scanner.yylex
}

/*
The %union directive is a way to specify the 
set of possible types that might be used as
translation attributes that a symbol might take.
For this project, only terminals have types (we'll
have translation attributes for non-terminals in the next
project)
*/
%union {
   holyc::Token* transToken;
}

%token                   END	   0 "end file"
%token	<transToken>     AND
%token	<transToken>     AT
%token	<transToken>     ASSIGN
%token	<transToken>     BOOL
%token	<transToken>     BOOLPTR
%token	<transToken>     CARAT
%token	<transToken>     CHAR
%token	<transCharToken> CHARLIT
%token	<transToken>     CHARPTR
%token	<transToken>     COMMA
%token	<transToken>     CROSS
%token	<transToken>     CROSSCROSS
%token	<transToken>     DASH
%token	<transToken>     DASHDASH
%token	<transToken>     ELSE
%token	<transToken>     EQUALS
%token	<transToken>     FALSE
%token	<transToken>     FROMCONSOLE
%token	<transIDToken>   ID
%token	<transToken>     IF
%token	<transToken>     INT
%token	<transIntToken>  INTLITERAL
%token	<transToken>     INTPTR
%token	<transToken>     GREATER
%token	<transToken>     GREATEREQ
%token	<transToken>     LBRACE
%token	<transToken>     LCURLY
%token	<transToken>     LESS
%token	<transToken>     LESSEQ
%token	<transToken>     LPAREN
%token	<transToken>     NOT
%token	<transToken>     NOTEQUALS
%token	<transToken>     NULLPTR
%token	<transToken>     OR
%token	<transToken>     RBRACE
%token	<transToken>     RCURLY
%token	<transToken>     RETURN
%token	<transToken>     RPAREN
%token	<transToken>     SEMICOLON
%token	<transToken>     SLASH
%token	<transToken>     STAR
%token	<transStrToken>  STRLITERAL
%token	<transToken>     TOCONSOLE
%token	<transToken>     TRUE
%token	<transToken>     VOID
%token	<transToken>     WHILE

/* NOTE: Make sure to add precedence and associativity 
 * declarations
*/

%%

/* TODO: add productions for the other nonterminals in the 
   grammar and make sure that all of the productions of the 
   given nonterminals are complete
*/
program 	: globals
{
	outputTokenStrs(tokenStrList($1), std::cout);
	return 0;
}

globals 	: globals decl 
{
	$$ = $1 + $2;
}
			| /* epsilon */
{
}

decl 		: varDecl SEMICOLON
{
	$$ = $1 + $2;
}
			| fnDecl
{
	$$ = $1;
}

varDecl 	: type id
{
	$$ = $1 + $2;
}

type 		: INT
{
	$$ = $1;
}
			| INTPTR
{
	$$ = $1;
}
		  	| BOOL
{
	$$ = $1;
}
		  	| BOOLPTR
{
	$$ = $1;
}
		  	| CHAR
{
	$$ = $1;
}
		  	| CHARPTR
{
	$$ = $1;
}
		  	| VOID
{
	$$ = $1;
}

fnDecl          : type id formals fnBody
{
	$$ = $1 + $2 + $3 + $4;
}


formals         : LPAREN RPAREN
{
	$$ = $1 + $2;
}

                | LPAREN formalsList RPAREN
{
	$$ = $1 + $2 + $3;
}


formalsList     : formalDecl
{
	$$ = $1;
}

                | formalDecl COMMA formalsList
{
	$$ = $1 + $2 + $3;
}


formalDecl      : type id
{
	$$ = $1 + $2;
}

fnBody          : LCURLY stmtList RCURLY
{
	$$ = $1 + $2 + $3;
}

stmtList        : stmtList stmt
{
	$$ = $1 + $2;
}

                | /* epsilon */
{
}


stmt            : varDecl SEMICOLON
{
	$$ = $1 + $2;
}

                | assignExp SEMICOLON
{
	$$ = $1 + $2;
}

                | lval DASHDASH SEMICOLON
{
	$$ = $1 + $2 + $3;
}

                | lval CROSSCROSS SEMICOLON
{
	$$ = $1 + $2 + $3;
}

                | FROMCONSOLE lval SEMICOLON
{
	$$ = $1 + $2 + $3;
}

                | TOCONSOLE exp SEMICOLON
{
	$$ = $1 + $2 + $3;
}

                | IF LPAREN exp RPAREN LCURLY stmtList RCURLY
{
	$$ = $1 + $2 + $3 + $4 + $5 + $6 + $7;
}

                | IF LPAREN exp RPAREN LCURLY stmtList RCURLY ELSE LCURLY stmtList RCURLY
{
	$$ = $1 + $2 + $3 + $4 + $5 + $6 + $7 + $8 + $9 + $10 + $11;
}

                | WHILE LPAREN exp RPAREN LCURLY stmtList RCURLY
{
	$$ = $1 + $2 + $3 + $4 + $5 + $6 + $7;
}

                | RETURN exp SEMICOLON
{
	$$ = $1 + $2 + $3;
}

                | RETURN SEMICOLON
{
	$$ = $1 + $2;
}

                | fncall SEMICOLON
{
	$$ = $1 + $2;
}

fncall          :  id LPAREN RPAREN   // fn call with no args
{
	$$ = $1 + $2 + $3;
}

                | id LPAREN actualsList RPAREN  // with args
{
	$$ = $1 + $2 + $3 + $4;
}


actualsList     : exp
{
	$$ = $1
}

                | actualsList COMMA exp
{
	$$ = $1 + $2 + $3;
}


exp             : NOT exp
{
	$$ = $1 + $2;
}

				| assignExp
{
	$$ = $1;
}

				| orExp
{
	$$ = $1;
}


assignExp       : lval ASSIGN exp
{
	$$ = $1 + $2 + $3;
}


orExp           : orExp OR andExp
{
	$$ = $1 + $2 + $3;
}

                | andExp
{
	$$ = $1;
}


andExp          : andExp AND cmpExp
{
	$$ = $1 + $2 + $3;
}

                | cmpExp
{
	$$ = $1;
}


cmpExp          : arithExp cmpOp arithExp
{
	$$ = $1 + $2 + $3;
}

                | arithExp
{
	$$ = $1;
}


arithExp        : arithExp arithOp prodExp
{
	$$ = $1 + $2 + $3;
}

                | prodExp
{
	$$ = $1;
}


prodExp         : prodExp prodOp termExp
{
	$$ = $1 + $2 + $3;
}

                | termExp
{
	$$ = $1;
}


termExp	        : DASH term
{
	$$ = $1 + $2;
}

		        | term
{
	$$ = $1;
}


cmpOp           : EQUALS
{
	$$ = $1;
}

                | NOTEQUALS
{
	$$ = $1;
}

                | GREATER
{
	$$ = $1;
}

                | GREATEREQ
{
	$$ = $1;
}

                | LESS
{
	$$ = $1;
}

                | LESSEQ
{
	$$ = $1;
}


arithOp         : CROSS
{
	$$ = $1;
}

                | DASH
{
	$$ = $1;
}


prodOp          : STAR
{
	$$ = $1;
}

                | SLASH
{
	$$ = $1;
}


term            : lval
{
	$$ = $1;
}

	            | INTLITERAL
{
	$$ = $1;
}

                | STRLITERAL
{
	$$ = $1;
}

                | CHARLIT
{
	$$ = $1;
}

                | TRUE
{
	$$ = $1;
}

                | FALSE
{
	$$ = $1;
}

                | NULLPTR
{
	$$ = $1;
}

                | LPAREN exp RPAREN
{
	$$ = $1 + $2 + $3;
}

                | fncall
{
	$$ = $1;
}


lval             : id
{
	$$ = $1;
}

                | id LBRACE exp RBRACE
{
	$$ = $1 + $2 + $3 + $4;
}

                | AT id
{
	$$ = $1 + $2;
}

                | CARAT id
{
	$$ = $1 + $2;
}


id			: ID
{
	$$ = $1;
}
	
%%

void holyc::Parser::error(const std::string& err_message){
   /* For project grading, only report "syntax error"
      if a program has bad syntax. However, you will
      probably want better output for debugging. Thus,
      this error function prints a verbose message to 
      stdout, but only prints "syntax error" to stderr
   */
	std::cout << err_message << std::endl;
	std::cerr << "syntax error" << std::endl;
}
