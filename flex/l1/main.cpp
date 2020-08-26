#include <fstream>
#include <iostream>
#include "scanner.hpp"
#include "grammar.hh"

int adjective = 1;
int noun = 2;
int verb = 3;

int main(){
	std::ifstream * inFile = new std::ifstream("input.txt");
	if (!inFile->good()){
		std::cout << "No such file input.txt\n";
	}
	MyScanner * lexer = new MyScanner(inFile);

	Token * myToken;

	int nounCount = 0;
	int verbCount = 0;
	int adjCount = 0;
	while (true){
		int tokenNum = lexer->yylex(&myToken);
		//if (myToken != nullptr){
		//	std::cout << "matched" << myToken->getLexeme()
		//		<< "at line " << myToken->getLine();
		//} 
		if (tokenNum == 0){
			break;
		}
		if (tokenNum == noun){
			nounCount++;
		}
		if (tokenNum == verb){
			verbCount++;
		}
		if (tokenNum == adjective){
			adjCount++;
		}
	}
	std::cout << nounCount << " " << verbCount << " " << adjCount;

	//NOT GONNA DO IT delete lexer
}
