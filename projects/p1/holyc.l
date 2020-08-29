%{
#include <string>
#include <limits.h>

/* Get our custom yyFlexScanner subclass */
#include "scanner.hpp"
#include "tokens.hpp"
// include our tracker file
#include "tracker.hpp"
#undef  YY_DECL
#define YY_DECL int holyc::Scanner::yylex( holyc::Parser::semantic_type * const lval )

/* define yyterminate as this instead of NULL */
#define yyterminate() return( TokenKind::END )

/* Exclude unistd.h for Visual Studio compatability. */
#define YY_NO_UNISTD_H

/* typedef to make the reference to token kinds shorter */
using TokenKind = holyc::Parser::token;

// declare a StrTracker object from our tracker file as a global variable
// invokes default constructor
StrTracker str_tracker;



%}

%option debug
%option nodefault
%option yyclass="holyc::Scanner"
%option noyywrap
%option c++

/* when we encounter a single ("), go to STR state */
%x STR

/* when we encounter a single ('), go to CHAR state */
%x CHAR
%x ESC_STR
%x ESC_CHAR

/* INITIAL state definitions */
DIGIT [0-9]
LETTER [a-zA-Z]
WHITESPACE [ \t]
INT {DIGIT}+
ID ({LETTER}|"_")({LETTER}|"_"|{DIGIT})*

/* CHAR state patterns */
VALID_ESC_CHAR [nt\'\\\"]

/* STR state patterns */
INVALID_STR_CHAR [\n\'\\\]



COMMENT [#](.)*$
INT {DIGIT}+



%%
%{          /** Code executed at the beginning of yylex **/
            yylval = lval;
%}

<INITIAL>"+"		{
		// Keep track of the "kind" of token we matched. In this case,
		// it's a CROSS (aka the plus symbol). The full list of token
		// kinds is declared around line 197 of grammar.hh. The testing
		// functionScanner::outputTokens also indicates what each token
		// represents (declared in scanner.cpp line 9)
		int tokenKind = TokenKind::CROSS;

		//Instatiate a token. There are 4 classes of tokens defined based on
		// - a literal character (CharLitToken) 
		// - a literal string (StringLitToken)
		// - a literal integer (IntLitToken)
		// - an identifier (IDToken)
		// - a base Token (any other type)
		// The token object is a handy place to keep information about the
		// token (i.e. token metadata) such as the line and column on which
		// the token character started (which can be used later in debug 
		// messages, etc). 
		Token * token = new Token(lineNum, colNum, tokenKind);

		//Assign the token to a instance variable of the scanner.
		// (When Flex processes a .l file in C++ mode, it creates 
		// a class called Scanner, which is a subclass of yyFlexScanner)
		// By assigning a value to this field, we put the token
		// metadata in a place where scanner's client program can find it.
		this->yylval->tokenValue = token;

		//Since we consumed 1 character, increase the column number by 1
		colNum++;

		//Let the scanner's client program know that a CROSS token
		// has been matched
		return tokenKind;
		}

<INITIAL>"-" {
		//Essentially the same thing as CROSS, but without the verbose
		// comments so it doesn't look so scary.
		int tokenKind = TokenKind::DASH;
		this->yylval->tokenValue = new Token(lineNum, colNum, tokenKind);
		colNum++;
		return tokenKind;
		}

<INITIAL>"@"

<INITIAL>"^"

<INITIAL>"["

<INITIAL>"]"

<INITIAL>"{"

<INITIAL>"}"

<INITIAL>"("

<INITIAL>")"

<INITIAL>";"

<INITIAL>","

<INITIAL>"--"

<INITIAL>"++"

<INITIAL>"*"

<INITIAL>"/"

<INITIAL>"!"

<INITIAL>"=="

<INITIAL>"!="

<INITIAL>"<"

<INITIAL>">"

<INITIAL>"&"

<INITIAL>"<="

<INITIAL>">="

<INITIAL>"="

<INITIAL>"&&"

<INITIAL>"||"

<INITIAL>int

<INITIAL>intptr

<INITIAL>bool

<INITIAL>Boolptr

<INITIAL>char

<INITIAL>charptr

<INITIAL>void

<INITIAL>if

<INITIAL>else

<INITIAL>while

<INITIAL>return

<INITIAL>true

<INITIAL>false

<INITIAL>FROMCONSOLE

<INITIAL>TOCONSOLE

<INITIAL>{INT}	{
		//TODO: The following computation of the integer
		// value does not check for overflow. It should do so.
		int intVal;
		try {
			intVal = std::stoi(yytext);
		}
		catch(std::out_of_range& e) {
			// error -> msg="Integer literal too large; using max value"
			
			intVal = MAX_INT;
		}


		//Note that the IntLitToken does not take a TokenKind argument
		// since it's implicitly known that the token represents an
		// INTLITERAL		
		yylval->tokenValue = new IntLitToken(lineNum, colNum, intVal);

		//TODO: update the colNum count according to the number of
		// characters consumed.
		colNum += yyleng;

		//As before, alert the client program that an INTLITERAL 
		// token was matched
		return TokenKind::INTLITERAL;
		}

<INITIAL>\n		{
		//Update the line number, reset the columnNumber
		lineNum++;
		colNum = 0;
	}

<INITIAL>{WHITESPACE} {
		colNum++;
	}

<INITIAL>.   {
		//TODO: Add the rest of the rules above this point so that legal characters
		// are properly accounted for. 
		std::string msg = "Illegal character ";
		msg += yytext;
		error(lineNum,colNum,msg);
		colNum += yyleng;
    }

<INITIAL>\" {
	// move to the string state
	BEGIN(STR);

	// reset the string and set the column number to start of string in case of error; increment colNum
	str_tracker.reset(colNum++);
}

<STR>\" {
	// move back to the initial state
	BEGIN(INITIAL);

	// create a new StringLitToken with str_tracker.str as the string

	// increment colNum
	colNum++;
}

<STR>{NEWLINE}	{
	// move back to initial state
	BEGIN(INITIAL);

	// error -> msg="Unterminated string literal ignored"

	// increment line, reset colNum
	lineNum++;
	colNum = 1;
}

<STR>.	{

}

<INITIAL>\' {
	BEGIN(CHAR);
}

<CHAR>{BAD_CHAR}

<CHAR>$	{

}

<CHAR>
%%
