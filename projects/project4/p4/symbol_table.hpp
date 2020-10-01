#ifndef HOLEYC_SYMBOL_TABLE_HPP
#define HOLEYC_SYMBOL_TABLE_HPP
#include <string>
#include <unordered_map>
#include <list>
#include "ast.hpp"

//Use an alias template so that we can use
// "HashMap" and it means "std::unordered_map"
template <typename K, typename V>
using HashMap = std::unordered_map<K, V>;

using namespace std;

namespace holeyc{

//A semantic symbol, which represents a single
// variable, function, etc. Semantic symbols 
// exist for the lifetime of a scope in the 
// symbol table.

// define a Kind type
enum Kind {VAR, FN};

// define a Type type
enum Type {INT, INTPTR, BOOL, BOOLPTR, CHAR, CHARPTR, VOID};

Type getType(TypeNode * node);

class SemSymbol {
	protected:
		Kind kind;
		Type type;
	public:
		SemSymbol(Kind k, Type t)
		: kind(k), type(t) {}
		Kind getKind() const { return kind; }
		Type getType() const { return type; }
};


//A single scope. The symbol table is broken down into a 
// chain of scope tables, and each scope table holds 
// semantic symbols for a single scope. For example,
// the globals scope will be represented by a ScopeTable,
// and the contents of each function can be represented by
// a ScopeTable.

enum QueryResult {SUCCESS, FAIL, MULTIPLE_DECL, UNDECLARED, INVALID_TYPE, INVALID_MULTIPLE};

class ScopeTable {
	public:
		ScopeTable();
		//TODO: add functions for looking up symbols
		// and/or returning information to indicate
		// that the symbol does not exist within the
		// current scope.

		// add an entry (declaration) to this scope
		QueryResult add(std::string id, SemSymbol * symbol);

		// returns the associated ID string's symbol ptr or nullptr if not found
		SemSymbol * lookup(std::string id);
	private:
		HashMap<std::string, SemSymbol *> * symbols;
};

class SymbolTable{
	public:
		SymbolTable();
		//TODO: add functions to create a new ScopeTable
		// when a new scope is entered, drop a ScopeTable
		// when a scope is exited, etc.

		// return true if there are no scopes on the stack
		bool noScopes() const;

		// create a new scope, push it to stack, and return the pointer to it
		ScopeTable * createScope();

		// pop a scope from the stack and return the pointer to it
		// return nullptr if empty stack
		ScopeTable * popScope();

		// get top scope pointer (current scope)
		// return nullptr if empty stack
		ScopeTable * peekScope();

		// wrapper around the ScopeTable implementation
		// adds pair to the scope at the front (top) of stack
		QueryResult add(std::string id, SemSymbol * symbol);

		// Takes an id name and attempts to find that id in the symbol table
		// If the name is found, then SUCCESS
		// If not found, then UNDECLARED result
		QueryResult reference(std::string id);

	private:
		std::list<ScopeTable *> * scopeTableChain;
};

	
}

#endif
