#include "ast.hpp"
#include "symbol_table.hpp"
#include "errors.hpp"
#include <iostream>


namespace holeyc{

static Type getType(TypeNode * node) {
	std::string s = node->show();
	if(s == "void") return VOID;
	if(s == "int") return INT;
	if(s == "intptr") return INTPTR;
	if(s == "char") return CHAR;
	if(s == "charptr") return CHARPTR;
	if(s == "bool") return BOOL;
	if(s == "boolptr") return BOOLPTR;
	return VOID;
}

static QueryResult addSymbol(SemSymbol * symbol, IDNode * idNode, SymbolTable * symTab) {
	return symTab->add(idNode->getName(), symbol);
}

static void printErr(IDNode * node, std::string msg) {
	Report::fatal(node->line(), node->col(), msg);
}

static bool stmtListAnalysis(std::list<StmtNode *> * list, SymbolTable * symTab) {
	bool valid = true;
	for (auto node : *list) {
		valid = node->nameAnalysis(symTab) && valid;
	}
	return valid;
}

static bool expListAnalysis(std::list<ExpNode *> * list, SymbolTable * symTab) {
	bool valid = true;
	for (auto node : *list) {
		valid = node->nameAnalysis(symTab) && valid;
	}
	return valid;
}

//TODO here is a subset of the nodes needed to do nameAnalysis, 
// you should add the rest to allow for a complete treatment
// of any AST

bool ASTNode::nameAnalysis(SymbolTable * symTab){
	return true;
}

bool ProgramNode::nameAnalysis(SymbolTable * symTab){
	symTab->createScope();
	bool res = true;
	for (auto global : *myGlobals){
		res = global->nameAnalysis(symTab) && res;
	}
	symTab->popScope();
	return res;
}

bool VarDeclNode::nameAnalysis(SymbolTable * symTab){ 
	Type t = getType(myType);
	VarSymbol * symbol = new VarSymbol(t);
	QueryResult res = addSymbol(symbol, myID, symTab);
	myID->setDecl();
	switch (res) {
		case SUCCESS: {
			return true;
		}
		case INVALID_MULTIPLE: {
			printErr(myID, "Invalid type in declaration");
			printErr(myID, "Multiply declared identifier");
			break;
		}
		case INVALID_TYPE: {
			printErr(myID, "Invalid type in declaration");
			break;
		}
		case MULTIPLE_DECL: {
			printErr(myID, "Multiply declared identifier");
			break;
		}
		default: break;
	}
	return false;
}

bool FnDeclNode::nameAnalysis(SymbolTable * symTab){
	bool success = true;
	Type t = getType(myRetType);
	FnSymbol * symbol = new FnSymbol(t);
	QueryResult res = addSymbol(symbol, myID, symTab);
	myID->setDecl();

	// display error messages for top-level function name declaration
	switch (res) {
		case INVALID_MULTIPLE: {
			printErr(myID, "Invalid type in declaration");
			printErr(myID, "Multiply declared identifier");
			break;
		}
		case INVALID_TYPE: {
			printErr(myID, "Invalid type in declaration");
			break;
		}
		case MULTIPLE_DECL: {
			printErr(myID, "Multiply declared identifier");
			break;
		}
		default: break;
	}

	// Create a new scope context at top of stack
	symTab->createScope();
	for (FormalDeclNode * formal : *myFormals) {
		success = formal->nameAnalysis(symTab) && success;
		symbol->addArgType(getType(formal->getTypeNode()));
	}

	// Run nameAnalysis over the function body statements
	success = stmtListAnalysis(myBody, symTab) && success;

	// pop the scope
	symTab->popScope();
	return success;
}

bool IDNode::nameAnalysis(SymbolTable * symTab) {
	SemSymbol * symbol = symTab->reference(name);
	if(symbol != nullptr) {
		mySymbol = symbol; 
		return true;
	}
	printErr(this, "Undeclared identifier");
	return false;
}

bool RefNode::nameAnalysis(SymbolTable * symTab) {
	return myID->nameAnalysis(symTab);
}

bool DerefNode::nameAnalysis(SymbolTable * symTab) {
	return myID->nameAnalysis(symTab);
}

bool IndexNode::nameAnalysis(SymbolTable * symTab) {
	return (myBase->nameAnalysis(symTab) && myOffset->nameAnalysis(symTab));
}

bool AssignStmtNode::nameAnalysis(SymbolTable * symTab) {
	return myExp->nameAnalysis(symTab);
}

bool FromConsoleStmtNode::nameAnalysis(SymbolTable * symTab) {
	return myDst->nameAnalysis(symTab);
}

bool ToConsoleStmtNode::nameAnalysis(SymbolTable * symTab) {
	return mySrc->nameAnalysis(symTab);
}

bool PostDecStmtNode::nameAnalysis(SymbolTable * symTab) {
	return myLVal->nameAnalysis(symTab);
}

bool PostIncStmtNode::nameAnalysis(SymbolTable * symTab) {
	return myLVal->nameAnalysis(symTab);
}

bool IfStmtNode::nameAnalysis(SymbolTable * symTab) {
	bool valid = true;
	valid = myCond->nameAnalysis(symTab) && valid;

	symTab->createScope();
	valid = stmtListAnalysis(myBody, symTab) && valid;
	symTab->popScope();
	return valid;
}

bool IfElseStmtNode::nameAnalysis(SymbolTable * symTab) {
	bool valid = true;
	valid = myCond->nameAnalysis(symTab) && valid;

	// name analysis on true branch
	symTab->createScope();
	valid = stmtListAnalysis(myBodyTrue, symTab) && valid;
	symTab->popScope();

	// name analysis on false branch
	symTab->createScope();
	valid = stmtListAnalysis(myBodyFalse, symTab) && valid;
	symTab->popScope();

	return valid;
}

bool WhileStmtNode::nameAnalysis(SymbolTable * symTab) {
	bool valid = true;
	valid = myCond->nameAnalysis(symTab) && valid;

	symTab->createScope();
	valid = stmtListAnalysis(myBody, symTab) && valid;
	symTab->popScope();
	return valid;
}

bool ReturnStmtNode::nameAnalysis(SymbolTable * symTab) {
	return myExp->nameAnalysis(symTab);
}

bool CallExpNode::nameAnalysis(SymbolTable * symTab) {
	return (myID->nameAnalysis(symTab) 
			&& expListAnalysis(myArgs, symTab));
}

bool BinaryExpNode::nameAnalysis(SymbolTable * symTab) {
	return (myExp1->nameAnalysis(symTab) 
			&& myExp2->nameAnalysis(symTab));
}

bool UnaryExpNode::nameAnalysis(SymbolTable * symTab) {
	return myExp->nameAnalysis(symTab);
}

bool AssignExpNode::nameAnalysis(SymbolTable * symTab) {
	return (myDst->nameAnalysis(symTab) 
			&& mySrc->nameAnalysis(symTab));
}

bool CallStmtNode::nameAnalysis(SymbolTable * symTab) {
	return myCallExp->nameAnalysis(symTab);
}

}
