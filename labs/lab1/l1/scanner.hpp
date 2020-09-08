#ifndef EECS665_SCANNER
#define EECS665_SCANNER

#include "token.hpp"

#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

class MyScanner : public yyFlexLexer{
	public:
		MyScanner(std::istream * in) : yyFlexLexer(in){
			lineNum = 1;
			colNum = 1;
		}
		virtual int yylex(Token ** lval);
	private:
		int colNum = 1;
		int lineNum = 1;
};

#endif
