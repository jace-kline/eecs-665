#include "ast.hpp"
#include "symbol_table.hpp"
#include "errors.hpp"
#include <iostream>

static std::ostream * err = &std::cerr;
static std::ostream * out = &std::cout;

namespace holeyc{

//TODO here is a subset of the nodes needed to do nameAnalysis, 
// you should add the rest to allow for a complete treatment
// of any AST

// bool ASTNode::nameAnalysis(SymbolTable * symTab){
// 	throw new ToDoError("This function should have"
// 		"been overriden in the subclass!");
// }

bool ProgramNode::nameAnalysis(SymbolTable * symTab){
	bool res = true;
	for (auto global : *myGlobals){
		res = global->nameAnalysis(symTab) && res;
	}
	return res;
}

bool VarDeclNode::nameAnalysis(SymbolTable * symTab){
	std::string name = myID->getName();
	Type t = getType(myType);
	SemSymbol * sym = new SemSymbol(VAR, t);
	QueryResult res = symTab->add(name, sym);
	switch (res) {
		case SUCCESS: {
			*out << name << " (" << myType->show() << ")\n";
			return true;
		}
		case INVALID_MULTIPLE: {}
		case INVALID_TYPE: {
			*err << line() << "," << col() << ": Invalid type in declaration\n";
		}
		case MULTIPLE_DECL: {
			*err << line() << "," << col() << ": Multiply declared identifier\n";
		}
	}
	return false;
}

bool FnDeclNode::nameAnalysis(SymbolTable * symTab){
	bool nameAnalysisOk = true;
	throw new ToDoError("[DELETE ME] I'm an fnDecl."
		" you should add and make current a new"	
		" scope table for my body"
	);
	return nameAnalysisOk;
}
}
