#include "ast.hpp"
#include "symbol_table.hpp"
#include "errors.hpp"
#include <iostream>

static std::ostream * initial_err = &std::cerr;
static std::ostream * initial_out = &std::cout;
static std::ostream * err = initial_err;
static std::ostream * out = initial_out;

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

static SemSymbol * mkSymbol(Kind k, TypeNode * typeNode, IDNode * idNode) {
	std::string name = idNode->getName();
	Type t = getType(typeNode);
	return new SemSymbol(k, t);
}

static QueryResult mkAddSymbol(Kind k, TypeNode * typeNode, IDNode * idNode, SymbolTable * symTab) {
	return symTab->add(idNode->getName(), mkSymbol(k, typeNode, idNode));
}

static void printErr(IDNode * node, std::string msg) {
	*err << node->line() << "," << node->col() << ": " << msg << "\n";
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
	bool res = true;
	for (auto global : *myGlobals){
		res = global->nameAnalysis(symTab) && res;
	}
	return res;
}

bool VarDeclNode::nameAnalysis(SymbolTable * symTab){ 
	QueryResult res = mkAddSymbol(VAR, myType, myID, symTab);
	switch (res) {
		case SUCCESS: {
			*out << myID->getName() << " (" << myType->show() << ")\n";
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
	QueryResult retRes = mkAddSymbol(FN, myRetType, myID, symTab);

	// display error messages for top-level function name declaration
	switch (retRes) {
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

	// Iterate through each formal paramater and perform name analysis
	// Add the type annotation for each parameter to a list
	std::list<std::string> annotations;

	std::stringstream out_, err_;
	out = &out_;
	err = &err_;
	for (FormalDeclNode * formal : *myFormals) {
		// write output to a stream buffer for delayed display
		success = formal->nameAnalysis(symTab) && success;
		// push type annotation string to back of list
		annotations.push_back(formal->getTypeNode()->show());
	}

	// Reassign the out and err streams to initial locations
	out = initial_out;
	err = initial_err;

	// If success thus far, show the type annotation of this function declaration
	if(success) {
		*out << myID->getName() << " (";
		int i = annotations.size();
		for (std::string s : annotations) {
			*out << s;
			i--;
			if(i > 0) *out << ",";
		}
		*out << " -> " << myRetType->show() << ")\n";
	}

	// get the strings from the string stream objects
	// output them to the out and err streams
	*err << err_.str();
	*out << out_.str();

	// Run nameAnalysis over the function body statements
	success = stmtListAnalysis(myBody, symTab) && success;

	// pop the scope
	symTab->popScope();
	return success;
}

bool IDNode::nameAnalysis(SymbolTable * symTab) {
	QueryResult res = symTab->reference(name);
	switch(res) {
		case SUCCESS: return true;
		case UNDECLARED: printErr(this, "Undeclared identifier"); break;
		default: break;
	}
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
