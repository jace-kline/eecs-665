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
	#include "unparser.hpp"
	// #include "tokens.hpp"
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
%parse-param { std::ostream *unparseOutStream }
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
	UnparseNode* unparsePtr;
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
%type <unparsePtr> globals
%type <unparsePtr> decl
%type <unparsePtr> varDecl
%type <unparsePtr> type
%type <unparsePtr> fnDecl
%type <unparsePtr> formals
%type <unparsePtr> formalsList
%type <unparsePtr> formalDecl
%type <unparsePtr> fnBody
%type <unparsePtr> stmtList
%type <unparsePtr> stmt
%type <unparsePtr> fncall
%type <unparsePtr> actualsList
%type <unparsePtr> exp
%type <unparsePtr> assignExp
%type <unparsePtr> orExp
%type <unparsePtr> andExp
%type <unparsePtr> cmpExp
%type <unparsePtr> arithExp
%type <unparsePtr> prodExp
%type <unparsePtr> termExp
%type <unparsePtr> cmpOp
%type <unparsePtr> arithOp
%type <unparsePtr> prodOp
%type <unparsePtr> term
%type <unparsePtr> lval
%type <unparsePtr> id

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
	holyc::Token* eof_token = new holyc::Token(scanner.getLine(), scanner.getCol(), holyc::Parser::token::END);
	UnparseNode unparsed = *$1 + *eof_token;
	if(unparseOutStream != NULL) writeUnparsed(unparsed, *unparseOutStream);
	// std::cout << unparsed.str << std::endl;
}

globals 	: globals decl
{
	$$ = new UnparseNode(*$1 + *$2);
}
			| /* epsilon */
{
	$$ = new UnparseNode("");
}

decl 		: varDecl SEMICOLON
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2));
}
			| fnDecl
{
	$$ = new UnparseNode(*$1);
}

varDecl 	: type id
{
	$$ = new UnparseNode(*$1 + *$2);
}

type 		: INT
{
	$$ = new UnparseNode(*$1);
}
			| INTPTR
{
	$$ = new UnparseNode(*$1);
}
		  	| BOOL
{
	$$ = new UnparseNode(*$1);
}
		  	| BOOLPTR
{
	$$ = new UnparseNode(*$1);
}
		  	| CHAR
{
	$$ = new UnparseNode(*$1);
}
		  	| CHARPTR
{
	$$ = new UnparseNode(*$1);
}
		  	| VOID
{
	$$ = new UnparseNode(*$1);
}

fnDecl          : type id formals fnBody
{
	$$ = new UnparseNode(*$1 + *$2 + *$3 + *$4);
}


formals         : LPAREN RPAREN
{
	$$ = new UnparseNode(UnparseNode(*$1) + UnparseNode(*$2));
}

                | LPAREN formalsList RPAREN
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2 + UnparseNode(*$2));
}


formalsList     : formalDecl
{
	$$ = new UnparseNode(*$1);
}

                | formalDecl COMMA formalsList
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3);
}


formalDecl      : type id
{
	$$ = new UnparseNode(*$1 + *$2);
}

fnBody          : LCURLY stmtList RCURLY
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2 + UnparseNode(*$3));
}

stmtList        : stmtList stmt
{
	$$ = new UnparseNode(*$1 + *$2);
}

                | /* epsilon */
{
	$$ = new UnparseNode("");
}


stmt            : varDecl SEMICOLON
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2));
}

                | assignExp SEMICOLON
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2));
}

                | lval DASHDASH SEMICOLON
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + UnparseNode(*$3));
}

                | lval CROSSCROSS SEMICOLON
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + UnparseNode(*$3));
}

                | FROMCONSOLE lval SEMICOLON
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2 + UnparseNode(*$3));
}

                | TOCONSOLE exp SEMICOLON
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2 + UnparseNode(*$3));
}

                | IF LPAREN exp RPAREN LCURLY stmtList RCURLY
{
	$$ = new UnparseNode(UnparseNode(*$1) + UnparseNode(*$2) + *$3 + UnparseNode(*$4) + UnparseNode(*$5) + *$6 + UnparseNode(*$7));
}

                | IF LPAREN exp RPAREN LCURLY stmtList RCURLY ELSE LCURLY stmtList RCURLY
{
	$$ = new UnparseNode(UnparseNode(*$1) + UnparseNode(*$2) + *$3 + UnparseNode(*$4) + UnparseNode(*$5) + *$6 + UnparseNode(*$7) + UnparseNode(*$8) + UnparseNode(*$9) + *$10 + UnparseNode(*$11));
}

                | WHILE LPAREN exp RPAREN LCURLY stmtList RCURLY
{
	$$ = new UnparseNode(UnparseNode(*$1) + UnparseNode(*$2) + *$3 + UnparseNode(*$4) + UnparseNode(*$5) + *$6 + UnparseNode(*$7));
}

                | RETURN exp SEMICOLON
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2 + UnparseNode(*$3));
}

                | RETURN SEMICOLON
{
	$$ = new UnparseNode(UnparseNode(*$1) + UnparseNode(*$2));
}

                | fncall SEMICOLON
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2));
}

fncall          :  id LPAREN RPAREN   // fn call with no args
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + UnparseNode(*$3));
}

                | id LPAREN actualsList RPAREN  // with args
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3 + UnparseNode(*$4));
}


actualsList     : exp
{
	$$ = $1;
}

                | actualsList COMMA exp
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3);
}


exp             : NOT exp
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2);
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
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3);
}


orExp           : orExp OR andExp
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3);
}

                | andExp
{
	$$ = $1;
}


andExp          : andExp AND cmpExp
{
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3);
}

                | cmpExp
{
	$$ = $1;
}


cmpExp          : arithExp cmpOp arithExp
{
	$$ = new UnparseNode(*$1 + *$2 + *$3);
}

                | arithExp
{
	$$ = $1;
}


arithExp        : arithExp arithOp prodExp
{
	$$ = new UnparseNode(*$1 + *$2 + *$3);
}

                | prodExp
{
	$$ = $1;
}


prodExp         : prodExp prodOp termExp
{
	$$ = new UnparseNode(*$1 + *$2 + *$3);
}

                | termExp
{
	$$ = $1;
}


termExp	        : DASH term
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2);
}

		        | term
{
	$$ = $1;
}


cmpOp           : EQUALS
{
	$$ = new UnparseNode(*$1);
}

                | NOTEQUALS
{
	$$ = new UnparseNode(*$1);
}

                | GREATER
{
	$$ = new UnparseNode(*$1);
}

                | GREATEREQ
{
	$$ = new UnparseNode(*$1);
}

                | LESS
{
	$$ = new UnparseNode(*$1);
}

                | LESSEQ
{
	$$ = new UnparseNode(*$1);
}


arithOp         : CROSS
{
	$$ = new UnparseNode(*$1);
}

                | DASH
{
	$$ = new UnparseNode(*$1);
}


prodOp          : STAR
{
	$$ = new UnparseNode(*$1);
}

                | SLASH
{
	$$ = new UnparseNode(*$1);
}


term            : lval
{
	$$ = $1;
}

	            | INTLITERAL
{
	$$ = new UnparseNode(*$1);
}

                | STRLITERAL
{
	$$ = new UnparseNode(*$1);
}

                | CHARLIT
{
	$$ = new UnparseNode(*$1);
}

                | TRUE
{
	$$ = new UnparseNode(*$1);
}

                | FALSE
{
	$$ = new UnparseNode(*$1);
}

                | NULLPTR
{
	$$ = new UnparseNode(*$1);
}

                | LPAREN exp RPAREN
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2 + UnparseNode(*$3));
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
	$$ = new UnparseNode(*$1 + UnparseNode(*$2) + *$3 + UnparseNode(*$4));
}

                | AT id
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2);
}

                | CARAT id
{
	$$ = new UnparseNode(UnparseNode(*$1) + *$2);
}


id			: ID
{
	$$ = new UnparseNode(*$1);
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
