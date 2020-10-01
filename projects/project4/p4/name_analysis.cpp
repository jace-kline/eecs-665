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
}

static SemSymbol * mkSymbol(Kind k, TypeNode * typeNode, IDNode * idNode) {
	std::string name = idNode->getName();
	Type t = getType(typeNode);
	return new SemSymbol(k, t);
}

static QueryResult mkAddSymbol(Kind k, TypeNode * typeNode, IDNode * idNode, SymbolTable * symTab) {
	return symTab->add(idNode->getName(), mkSymbol(k, typeNode, idNode));
}

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
	QueryResult res = mkAddSymbol(VAR, myType, myID, symTab);
	switch (res) {
		case SUCCESS: {
			*out << myID->getName() << " (" << myType->show() << ")\n";
			return true;
		}
		case INVALID_MULTIPLE: {}
		case INVALID_TYPE: {
			*err << line() << "," << col() << ": Invalid type in declaration\n";
			if(res == INVALID_TYPE) break;
		}
		case MULTIPLE_DECL: {
			*err << line() << "," << col() << ": Multiply declared identifier\n";
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
		case INVALID_MULTIPLE: {}
		case INVALID_TYPE: {
			success = false;
			*err << line() << "," << col() << ": Invalid type in declaration\n";
			if(retRes == INVALID_TYPE) break;
		}
		case MULTIPLE_DECL: {
			success = false;
			*err << line() << "," << col() << ": Multiply declared identifier\n";
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
	for(StmtNode * stmt : *myBody) {
		success = stmt->nameAnalysis(symTab) && success;
	}
	return success;
}



}
