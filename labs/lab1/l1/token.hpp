#ifndef EECS665_TOKEN
#define EECS665_TOKEN

#include <string>

class Token {
public:
	Token(std::string in, int line, int col)
	: lexeme(in), myLine(line), myCol(col){}
	std::string getLexeme(){ return lexeme; }
	int getLine(){ return myLine; }
private:
	std::string lexeme;
	int myLine;
	int myCol;
};

#endif
