#include "symbol_table.hpp"
namespace holeyc{

ScopeTable::ScopeTable(){
	symbols = new HashMap<std::string, SemSymbol *>();
}

QueryResult ScopeTable::add(std::string id, SemSymbol * symbol) {
	SemSymbol * look = lookup(id);
	bool invalidType = (symbol->getKind() == VAR && symbol->getType() == VOID);
	bool multipleDecl = (look != nullptr);
	if(invalidType && !multipleDecl) return INVALID_TYPE;
	else if(!invalidType && multipleDecl) return MULTIPLE_DECL;
	else if(invalidType && multipleDecl) return INVALID_MULTIPLE;
	else {
		symbols->insert({id, symbol});
		return SUCCESS;
	}
}

SemSymbol * ScopeTable::lookup(std::string id) {
	auto search = symbols->find(id);
	return (search != symbols->end()) ? search->second : nullptr;
}

SymbolTable::SymbolTable(){
	//TODO: implement the list of hashtables approach
	// to building a symbol table:
	// Upon entry to a scope a new scope table will be 
	// entered into the front of the chain and upon exit the 
	// latest scope table will be removed from the front of 
	// the chain.
	scopeTableChain = new std::list<ScopeTable *>();
}

bool SymbolTable::noScopes() const {
	return scopeTableChain->empty();
}

ScopeTable * SymbolTable::createScope() {
	ScopeTable * scope = new ScopeTable();
	scopeTableChain->push_front(scope);
	return scope;
}

ScopeTable * SymbolTable::popScope() {
	if(!noScopes()) {
		ScopeTable * scope = scopeTableChain->front();
		scopeTableChain->pop_front();
		return scope;
	}
	return nullptr;
}

ScopeTable * SymbolTable::peekScope() {
	if(!noScopes()) return scopeTableChain->front();
	return nullptr;
}

QueryResult SymbolTable::add(std::string id, SemSymbol * symbol) {
	if(!noScopes()) {
		return peekScope()->add(id, symbol);
	}
	return FAIL;
}

QueryResult SymbolTable::reference(std::string id) {
	for (ScopeTable * scope : *scopeTableChain) {
		SemSymbol * symbol = scope->lookup(id);
		if(symbol != nullptr) {
			return SUCCESS;
		}
	}
	return UNDECLARED;
}

}
