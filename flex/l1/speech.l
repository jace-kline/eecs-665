%option noyywrap
%option c++
%option yyclass="MyScanner"
%{
using namespace std;

#undef YY_DECL
#define YY_DECL int MyScanner::yylex( Token ** lval)

#include "scanner.hpp"
#include "token.hpp"
#include "grammar.hh"
%}
%%
		/*
		Man man Woman woman
		*/

[Dd]og { 
	Token * res = new Token(yytext, lineNum, colNum);
	colNum += strlen(yytext);
	
	*lval = res;

	return noun; 
	}
[Mm]an { 
	colNum += strlen(yytext);
	return noun; 
	}
([wW]o)?man { 
	colNum += strlen(yytext);
	return noun; 
	}
bites { 
	colNum += strlen(yytext);
	return verb; }
kicks { 
	colNum += strlen(yytext);
	return verb; }
loves { 
	colNum += strlen(yytext);
	return verb; 
	}
lo+ng { 
	cout << "matched long with " << yytext << "\n";
	colNum += strlen(yytext);
	return adjective; 
	}
tall { 
	colNum += strlen(yytext);
	return adjective; 
	}
short { 
	colNum += strlen(yytext);
	return adjective; 
	}
[ \t] { 
	colNum += strlen(yytext);
	}
\n	{
	lineNum++;
	colNum = 1;
	}

.   { cout << "Bad character "
	 << yytext << " at " << this->lineNum << "," 
	<< this->colNum << endl;
	exit(1); 
	}
%%

