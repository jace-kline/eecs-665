%{
#include <string>
#include <limits.h>

/* Get our custom yyFlexScanner subclass */
#include "scanner.hpp"
#include "tokens.hpp"
// include our tracker file that contains the StrTracker
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

/* when we encounter a single (") from INITIAL, go to STR state -> initiates use of 'str_tracker' object */
%x STR

/* RegEx aliases */
DIGIT [0-9]
LETTER [a-zA-Z]
WHITESPACE [ \t]
BACKSLASH \\
END (\n)|($)
SINGLEQUOTE [']
DOUBLEQUOTE \"
ID ({LETTER}|"_")({LETTER}|"_"|{DIGIT})*
COMMENT [#]((.)*)({END})
INTLIT {DIGIT}+

VALID_STRLIT_ESC_CHAR [nt]|{SINGLEQUOTE}|{BACKSLASH}|{DOUBLEQUOTE}
VALID_CHARLIT_ESC_CHAR [nt]|{BACKSLASH}


%%
%{          /** Code executed at the beginning of yylex **/
            yylval = lval;
%}

<INITIAL>"+" { return makeBareToken(TokenKind::CROSS); }

<INITIAL>"-" { return makeBareToken(TokenKind::DASH); }

<INITIAL>"@" { return makeBareToken(TokenKind::AT); }

<INITIAL>"^" { return makeBareToken(TokenKind::CARAT); }

<INITIAL>"[" { return makeBareToken(TokenKind::LBRACE); }

<INITIAL>"]" { return makeBareToken(TokenKind::RBRACE); }

<INITIAL>"{" { return makeBareToken(TokenKind::LCURLY); }

<INITIAL>"}" { return makeBareToken(TokenKind::RCURLY); }

<INITIAL>"(" { return makeBareToken(TokenKind::LPAREN); }

<INITIAL>")" { return makeBareToken(TokenKind::RPAREN); }

<INITIAL>";" { return makeBareToken(TokenKind::SEMICOLON); }

<INITIAL>"," { return makeBareToken(TokenKind::COMMA); }

<INITIAL>"--" { return makeBareToken(TokenKind::DASHDASH); }

<INITIAL>"++" { return makeBareToken(TokenKind::CROSSCROSS); }

<INITIAL>"*" { return makeBareToken(TokenKind::STAR); }

<INITIAL>"/" { return makeBareToken(TokenKind::SLASH); }

<INITIAL>"!" { return makeBareToken(TokenKind::CARAT); }

<INITIAL>"==" { return makeBareToken(TokenKind::EQUALS); }

<INITIAL>"!=" { return makeBareToken(TokenKind::NOTEQUALS); }

<INITIAL>"<" { return makeBareToken(TokenKind::LESS); }

<INITIAL>">" { return makeBareToken(TokenKind::GREATER); }

<INITIAL>"&" { return makeBareToken(TokenKind::AMP); }

<INITIAL>"<=" { return makeBareToken(TokenKind::LESSEQ); }

<INITIAL>">=" { return makeBareToken(TokenKind::GREATEREQ); }

<INITIAL>"=" { return makeBareToken(TokenKind::ASSIGN); }

<INITIAL>"&&" { return makeBareToken(TokenKind::AND); }

<INITIAL>"||" { return makeBareToken(TokenKind::OR); }

<INITIAL>"int" { return makeBareToken(TokenKind::INT); }

<INITIAL>"intptr" { return makeBareToken(TokenKind::INTPTR); }

<INITIAL>"bool" { return makeBareToken(TokenKind::BOOL); }

<INITIAL>"boolptr" { return makeBareToken(TokenKind::BOOLPTR); }

<INITIAL>"char" { return makeBareToken(TokenKind::CHAR); }

<INITIAL>"charptr" { return makeBareToken(TokenKind::CHARPTR); }

<INITIAL>"void" { return makeBareToken(TokenKind::VOID); }

<INITIAL>"if" { return makeBareToken(TokenKind::IF); }

<INITIAL>"else" { return makeBareToken(TokenKind::ELSE); }

<INITIAL>"while" { return makeBareToken(TokenKind::WHILE); }

<INITIAL>"return" { return makeBareToken(TokenKind::RETURN); }

<INITIAL>"true" { return makeBareToken(TokenKind::TRUE); }

<INITIAL>"false" { return makeBareToken(TokenKind::FALSE); }

<INITIAL>"FROMCONSOLE" { return makeBareToken(TokenKind::FROMCONSOLE); }

<INITIAL>"TOCONSOLE" { return makeBareToken(TokenKind::TOCONSOLE); }

<INITIAL>{ID} { return makeIDToken(); }

<INITIAL>{INTLIT}	{
		// Check for int overflow
		int intVal;
		try {
			intVal = std::stoi(yytext);
		}
		catch(std::out_of_range& e) {
			// error -> msg="Integer literal too large; using max value"
			errIntOverflow(lineNum, colNum);
			intVal = INT_MAX;
		}

		return makeIntLitToken(intVal);
}

<INITIAL>({SINGLEQUOTE})({BACKSLASH})({VALID_CHARLIT_ESC_CHAR}) { return makeCharLitToken(std::string(yytext)); }

<INITIAL>({SINGLEQUOTE})({BACKSLASH})({END}) { 
	errChrEscEmpty(lineNum, colNum);
	colNum = 1;
	lineNum++;
}

<INITIAL>({SINGLEQUOTE})({END}) { 
	errChrEmpty(lineNum, colNum);
	colNum = 1;
	lineNum++;
}

<INITIAL>({SINGLEQUOTE})({BACKSLASH})(.) {
	errChrEsc(lineNum, colNum);
	colNum += static_cast<size_t>(yyleng);
}

<INITIAL>({SINGLEQUOTE})(.) { return makeCharLitToken(std::string(yytext)); }

<INITIAL>{DOUBLEQUOTE} {
	// move to the string state (STR)
	BEGIN(STR);

	// initiate the string tracker
	str_tracker.set(colNum);
}

<STR>{DOUBLEQUOTE} {
	// move back to the initial state (INITIAL)
	BEGIN(INITIAL);

	// Update column with the characters consumed in the STR state
	colNum += str_tracker.str.length() + 2; // String's length + each double quote character

	// if no bad escape characters, create a new StrToken with str_tracker.str as the string
	if(!str_tracker.bad_esc) { return makeStrToken(str_tracker.col_start, str_tracker.str); }
	else { errStrEsc(lineNum, str_tracker.col_start); }
}

<STR>{END}	{
	// move back to initial state
	BEGIN(INITIAL);

	// 2 errors possible depending on the bad_esc bit in the str_tracker
	if(str_tracker.bad_esc) { errStrEscAndUnterm(lineNum, str_tracker.col_start); }
	else { errStrUnterm(lineNum, str_tracker.col_start); }

	// increment line, reset colNum
	lineNum++;
	colNum = 1;
}

<INITIAL>({DOUBLEQUOTE})({WHITESPACE}*)({END}) {
	errStrUnterm(lineNum, str_tracker.col_start);

	// increment line, reset colNum
	lineNum++;
	colNum = 1;
}

<STR>({BACKSLASH})({VALID_STRLIT_ESC_CHAR}) { str_tracker.append(yytext); }

<STR>({BACKSLASH})(.) {
	str_tracker.bad_esc = true; // will result in an error
	str_tracker.append(yytext); //we still append to keep track of the string length
	// will this affect the correct column length?
}

<STR>(.) { str_tracker.append(yytext); }

<INITIAL>{END}|{COMMENT} { lineNum++; colNum = 1; }

<INITIAL>{WHITESPACE}* { colNum += static_cast<size_t>(yyleng); }

<INITIAL>(.) {
		errIllegal(lineNum,colNum,std::string(yytext));
		colNum += static_cast<size_t>(yyleng);
	}


%%
