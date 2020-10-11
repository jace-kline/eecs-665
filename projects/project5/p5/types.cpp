#include <list>
#include <sstream>

#include "types.hpp"
#include "ast.hpp"

namespace holeyc{

std::string typeStr(Type t) {
	switch(t) {
		case INT: return "int";
		case INTPTR: return "intptr";
		case BOOL: return "bool";
		case BOOLPTR: return "boolptr";
		case CHAR: return "char";
		case CHARPTR: return "charptr";
		default: return "void";
	}
	return "void";
}

Type baseType(Type t) {
	if(t == BOOL || t == BOOLPTR) return BOOL;
	else if(t == INT || t == INTPTR) return INT;
	else if(t == CHAR || t == CHARPTR) return CHAR;
	else return t;
}

Type refType(Type t) {
	if(t == BOOL) return BOOLPTR;
	else if(t == INT) return INTPTR;
	else if(t == CHAR) return CHARPTR;
	else return ERRTYPE;
}

Type derefType(Type t) {
	if(t == BOOLPTR) return BOOL;
	else if(t == INTPTR) return INT;
	else if(t == CHARPTR) return CHAR;
	else return ERRTYPE;
}

bool isPtrType(Type t) {
	return (t == BOOLPTR || t == INTPTR || t == CHARPTR || t == GENERICPTR);
}

bool isBaseType(Type t) {
	return (t == BOOL || t == INT || t == CHAR);
}

} //End namespace
