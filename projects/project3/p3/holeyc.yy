%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define api.namespace{holeyc}
%define api.parser.class {Parser}
%define parse.error verbose
%output "parser.cc"
%token-table

%code requires{
	#include <list>
	#include "tokens.hpp"
	#include "ast.hpp"
	namespace holeyc {
		class Scanner;
	}

//The following definition is required when 
// we don't have the %locations directive
# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

//End "requires" code
}

%parse-param { holeyc::Scanner &scanner }
%parse-param { holeyc::ProgramNode** root }

%code{
   // C std code for utility functions
   #include <iostream>
   #include <cstdlib>
   #include <fstream>

   // Our code for interoperation between scanner/parser
   #include "scanner.hpp"
   #include "ast.hpp"
   #include "tokens.hpp"

  //Request tokens from our scanner member, not 
  // from a global function
  #undef yylex
  #define yylex scanner.yylex
}


/*
The %union directive is a way to specify the 
set of possible types that might be used as
translation attributes in the syntax-directed
translations. 

TODO: You will have to add to this list to 
create new translation value types
*/
%union {
   holeyc::Token *                     transToken;
   holeyc::IDToken *                   transIDToken;
   holeyc::StrToken *				   transStrToken;
   holeyc::CharLitToken *			   transCharToken;
   holeyc::IntLitToken *			   transIntToken;
   holeyc::ProgramNode *               transProgram;
   std::list<holeyc::DeclNode *> *     transDeclList;
   holeyc::DeclNode *                  transDecl;
   holeyc::VarDeclNode *               transVarDecl;
   holeyc::TypeNode *                  transType;
   holeyc::IDNode *                    transID;
   holeyc::FnDeclNode *				   transFnDecl;
   holeyc::FormalsNode *			   transFormals;
   std::list<holeyc::FormalDeclNode *> *  transFormalList;
   holeyc::FormalDeclNode *			   transFormal;
   holeyc::FnBodyNode *				   transFnBody;
   holeyc::StmtNode *				   transStmt;
   std::list<holeyc::StmtNode *> *	   transStmtList;		
   holeyc::ExpNode *				   transExp;
   holeyc::AssignExpNode *			   transAssignExp;
   holeyc::FnCallNode *				   transFnCall;
   std::list<holeyc::ExpNode *> *	   transExpList;
   holeyc::LValNode *				   transLVal;
}

%define parse.assert

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


/* Nonterminals
*  TODO: You will need to add more nonterminals
*  to this list as you add productions to the grammar
*  below (along with indicating the appropriate translation
*  attribute type). Note that the specifier in brackets
*  indicates the type of the translation attribute using
*  the names defined in the %union directive above
*/
/*    (attribute type)    (nonterminal)    */
%type <transProgram>    program
%type <transDeclList>   globals
%type <transDecl>       decl
%type <transVarDecl>    varDecl
%type <transType>       type
%type <transID>         id
%type <transFnDecl>		fnDecl
%type <transFormals>	formals
%type <transFormalList>	formalsList
%type <transFormal>		formalDecl
%type <transFnBody>		fnBody
%type <transStmtList>	stmtList
%type <transStmt>		stmt
%type <transExp>		exp
%type <transExp>		term
%type <transAssignExp>	assignExp
%type <transFnCall>		callExp
%type <transExpList>	actualsList
%type <transLVal>		lval

%right ASSIGN
%left OR
%left AND
%nonassoc LESS GREATER LESSEQ GREATEREQ EQUALS NOTEQUALS
%left DASH CROSS
%left STAR SLASH
%left NOT 
%left AT CARAT

%%

program 	: globals
		  {
		  $$ = new ProgramNode($1);
		  *root = $$;
		  }

globals 	: globals decl 
		  {
		  $$ = $1;
		  DeclNode * aGlobalDecl = $2;
		  $1->push_back(aGlobalDecl);
		  }
		| /* epsilon */
		  {
		  std::list<DeclNode *> * startingGlobals;
		  startingGlobals = new std::list<DeclNode *>();
	 	  $$ = startingGlobals;
		  }
		; 

decl 		: varDecl SEMICOLON
		  {
			$$ = $1;
		  }
		| fnDecl 
		  { 
			$$ = $1;
		  }

varDecl 	: type id
		  { 
			size_t typeLine = $1->line();
			size_t typeCol = $1->col();
			$$ = new VarDeclNode(typeLine, typeCol, $1, $2);
		  }

type 		: INT
		  {
		  	$$ = new TypeNode($1->line(), $1->col(), INT);
		  }
		| INTPTR
		  {
			  $$ = new TypeNode($1->line(), $1->col(), INTPTR);
		  }
		| BOOL
		  { 
			  $$ = new TypeNode($1->line(), $1->col(), BOOL);
		  }
		| BOOLPTR
		  { 
			  $$ = new TypeNode($1->line(), $1->col(), BOOLPTR);
		  }
		| CHAR
		  { 
			  $$ = new TypeNode($1->line(), $1->col(), CHAR);
		  }
		| CHARPTR
		  { 
			  $$ = new TypeNode($1->line(), $1->col(), CHARPTR);
		  }
		| VOID
		  { 
			  $$ = new TypeNode($1->line(), $1->col(), VOID);
		  }

fnDecl 		: type id formals fnBody
		  {
			  size_t l = $1->line();

			  $$ = new FnDeclNode($1->line(), 
			  					  $1->col(),
								  $1,
								  $2,
								  $3,
								  $4);
		  }

formals 	: LPAREN RPAREN
		  { 
			  $$ = new FormalsNode($1->line(),$1->col(),new std::list<FormalDeclNode *>());
		  }
		| LPAREN formalsList RPAREN
		  { 
			  $$ = new FormalsNode($1->line(),$1->col(),$2);
		  }


formalsList	: formalDecl
		  { 
			  std::list<FormalDeclNode *> *lptr = new std::list<FormalDeclNode *>();
			  lptr->push_back($1);
			  $$ = lptr;
		  }
		| formalDecl COMMA formalsList 
		  { 
			  $3->push_front($1);
			  $$ = $3;
		  }

formalDecl 	: type id
		  { 
			  $$ = new FormalDeclNode($1->line(),$1->col(),$1,$2);
		  }

fnBody		: LCURLY stmtList RCURLY
		  { 
			  $$ = new FnBodyNode($1->line(),$1->col(),$2);
		  }

stmtList 	: /* epsilon */
		  { 
			  $$ = new std::list<StmtNode *>();
		  }
		| stmtList stmt
		  {
			  $1->push_back($2);
			  $$ = $1;
		  }

stmt		: varDecl SEMICOLON
		  {
			  $$ = new VarDeclStmtNode($1->line(), $1->col(), $1);
		  }
		| assignExp SEMICOLON
		  {
			  $$ = new AssignStmtNode($1->line(), $1->col(), $1);
		  }
		| lval DASHDASH SEMICOLON
		  {
			  $$ = new LValStmtNode($1->line(), $1->col(), $1, DEC);
		  }
		| lval CROSSCROSS SEMICOLON
		  {
			  $$ = new LValStmtNode($1->line(), $1->col(), $1, INC);
		  }
		| FROMCONSOLE lval SEMICOLON
		  { 
			  $$ = new LValStmtNode($1->line(), $1->col(), $2, FROMCONSOLE);
		  }
		| TOCONSOLE exp SEMICOLON
		  {
			  $$ = new ExpStmtNode($1->line(), $1->col(), $2, TOCONSOLE);
		  }
		| IF LPAREN exp RPAREN LCURLY stmtList RCURLY
		  {
			  $$ = new CondStmtNode($1->line(), $1->col(), $3, $6, nullptr, IF);
		  }
		| IF LPAREN exp RPAREN LCURLY stmtList RCURLY ELSE LCURLY stmtList RCURLY
		  {
			  $$ = new CondStmtNode($1->line(), $1->col(), $3, $6, $10, IFELSE);
		  }
		| WHILE LPAREN exp RPAREN LCURLY stmtList RCURLY
		  {
			  $$ = new CondStmtNode($1->line(), $1->col(), $3, $6, nullptr, WHILE);
		  }
		| RETURN exp SEMICOLON
		  { 
			  $$ = new ExpStmtNode($1->line(), $1->col(), $2, RETURN);
		  }
		| RETURN SEMICOLON
		  {
			  $$ = new ExpStmtNode($1->line(), $1->col(), nullptr, RETURN);
		  }
		| callExp SEMICOLON
		  {
			  $$ = new FnCallStmtNode($1->line(),$1->col(),$1);
		  }

exp		: assignExp 
		  {
			  $$ = $1;
		  }
		| exp DASH exp
		  { 
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,DASH_BIN);
		  }
		| exp CROSS exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,CROSS);
		  }
		| exp STAR exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,STAR);
		  }
		| exp SLASH exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,SLASH);
		  }
		| exp AND exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,AND);
		  }
		| exp OR exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,OR);
		  }
		| exp EQUALS exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,EQUALS);
		  }
		| exp NOTEQUALS exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,NOTEQUALS);
		  }
		| exp GREATER exp
		  { 
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,GREATER);
		  }
		| exp GREATEREQ exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,GREATEREQ);
		  }
		| exp LESS exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,LESS);
		  }
		| exp LESSEQ exp
		  {
			  $$ = new BinOpExpNode($1->line(),$1->col(),$1,$3,LESSEQ);
		  }
		| NOT exp
		  {
			  $$ = new UnOpExpNode($1->line(),$1->col(),$2,NOT);
		  }
		| DASH term
		  {
			  $$ = new UnOpExpNode($1->line(),$1->col(),$2,DASH_UN);
		  }
		| term 
		  {
			  $$ = $1;
		  }

assignExp	: lval ASSIGN exp
		  { 
			  $$ = new AssignExpNode($1->line(),$1->col(),$1,$3);
		  }

callExp		: id LPAREN RPAREN
		  {
			  $$ = new FnCallNode($1->line(),$1->col(),$1,new std::list<ExpNode *>());
		  }
		| id LPAREN actualsList RPAREN
		  {
			  $$ = new FnCallNode($1->line(),$1->col(),$1,$3);
		  }

actualsList	: exp
		  { 
			  std::list<ExpNode *> * l = new std::list<ExpNode *>();
			  l->push_back($1);
			  $$ = l; 
		  }
		| actualsList COMMA exp
		  {
			  $1->push_back($3);
			  $$ = $1;
		  }

term 		: lval
		  {
			$$ = $1;
		  }
		| callExp
		  {
			  $$ = $1;
		  }
		| NULLPTR
		  {
			  $$ = new TermPrimitiveNode($1->line(),$1->col(),NULLPTR);
		  }
		| INTLITERAL 
		  {
			  $$ = new IntLitNode($1);
		  }
		| STRLITERAL 
		  {
			  $$ = new StrLitNode($1);
		  }
		| CHARLIT 
		  {
			  $$ = new CharLitNode($1);
		  }
		| TRUE
		  {
			  $$ = new TermPrimitiveNode($1->line(),$1->col(),TRUE);
		  }
		| FALSE
		  {
			  $$ = new TermPrimitiveNode($1->line(),$1->col(),FALSE);
		  }
		| LPAREN exp RPAREN
		  {
			  $$ = new TermGrpNode($1->line(),$1->col(),$2);
		  }

lval		: id
		  {
			  $$ = $1;
		  }
		| id LBRACE exp RBRACE
		  {
			  $$ = new LValIndexNode($1->line(),$1->col(),$1,$3);
		  }
		| AT id
		  {
			  $$ = new LValUnOpNode($1->line(),$1->col(),$2,AT);
		  }
		| CARAT id
		  {
			  $$ = new LValUnOpNode($1->line(),$1->col(),$2,CARAT);
		  }

id		: ID
		  {
		  	$$ = new IDNode($1);
		  }
	
%%

void holeyc::Parser::error(const std::string& msg){
	std::cout << msg << std::endl;
	std::cerr << "syntax error\nNo AST built" << std::endl;
}
