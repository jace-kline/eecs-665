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
	#include <list>
	// #include "helper.hpp"
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
	holyc::CharLitToken* transCharToken;
	holyc::StrToken* transStrToken;
	holyc::IDToken* transIDToken;
	holyc::IntLitToken* transIntToken;
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
// %type <transList> globals
// %type <transList> decl
// %type <transList> varDecl
// %type <transList> type
// %type <transList> fnDecl
// %type <transList> formals
// %type <transList> formalsList
// %type <transList> formalDecl
// %type <transList> fnBody
// %type <transList> stmtList
// %type <transList> stmt
// %type <transList> fncall
// %type <transList> actualsList
// %type <transList> exp
// %type <transList> assignExp
// %type <transList> orExp
// %type <transList> andExp
// %type <transList> cmpExp
// %type <transList> arithExp
// %type <transList> prodExp
// %type <transList> termExp
// %type <transList> cmpOp
// %type <transList> arithOp
// %type <transList> prodOp
// %type <transList> term
// %type <transList> lval
// %type <transList> id

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

}

globals 	: globals decl 
{
}
			| /* epsilon */
{
}

decl 		: varDecl SEMICOLON
{

}
			| fnDecl
{

}

varDecl 	: type id
{

}

type 		: INT
{

}
			| INTPTR
{

}
		  	| BOOL
{

}
		  	| BOOLPTR
{

}
		  	| CHAR
{

}
		  	| CHARPTR
{

}
		  	| VOID
{

}

fnDecl          : type id formals fnBody
{

}


formals         : LPAREN RPAREN
{

}

                | LPAREN formalsList RPAREN
{

}


formalsList     : formalDecl
{

}

                | formalDecl COMMA formalsList
{

}


formalDecl      : type id
{

}

fnBody          : LCURLY stmtList RCURLY
{

}

stmtList        : stmtList stmt
{

}

                | /* epsilon */
{
}


stmt            : varDecl SEMICOLON
{

}

                | assignExp SEMICOLON
{

}

                | lval DASHDASH SEMICOLON
{

}

                | lval CROSSCROSS SEMICOLON
{

}

                | FROMCONSOLE lval SEMICOLON
{

}

                | TOCONSOLE exp SEMICOLON
{

}

                | IF LPAREN exp RPAREN LCURLY stmtList RCURLY
{

}

                | IF LPAREN exp RPAREN LCURLY stmtList RCURLY ELSE LCURLY stmtList RCURLY
{

}

                | WHILE LPAREN exp RPAREN LCURLY stmtList RCURLY
{

}

                | RETURN exp SEMICOLON
{

}

                | RETURN SEMICOLON
{

}

                | fncall SEMICOLON
{

}

fncall          :  id LPAREN RPAREN   // fn call with no args
{

}

                | id LPAREN actualsList RPAREN  // with args
{

}


actualsList     : exp
{

}

                | actualsList COMMA exp
{

}


exp             : NOT exp
{

}

				| assignExp
{

}

				| orExp
{

}


assignExp       : lval ASSIGN exp
{

}


orExp           : orExp OR andExp
{

}

                | andExp
{

}


andExp          : andExp AND cmpExp
{

}

                | cmpExp
{

}


cmpExp          : arithExp cmpOp arithExp
{

}

                | arithExp
{

}


arithExp        : arithExp arithOp prodExp
{

}

                | prodExp
{

}


prodExp         : prodExp prodOp termExp
{

}

                | termExp
{

}


termExp	        : DASH term
{

}

		        | term
{

}


cmpOp           : EQUALS
{

}

                | NOTEQUALS
{

}

                | GREATER
{

}

                | GREATEREQ
{

}

                | LESS
{

}

                | LESSEQ
{

}


arithOp         : CROSS
{

}

                | DASH
{

}


prodOp          : STAR
{

}

                | SLASH
{

}


term            : lval
{

}

	            | INTLITERAL
{

}

                | STRLITERAL
{

}

                | CHARLIT
{

}

                | TRUE
{

}

                | FALSE
{

}

                | NULLPTR
{

}

                | LPAREN exp RPAREN
{

}

                | fncall
{

}


lval             : id
{

}

                | id LBRACE exp RBRACE
{

}

                | AT id
{

}

                | CARAT id
{

}


id			: ID
{

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
