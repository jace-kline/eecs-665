#include "3ac.hpp"
#include "vector"
#include "type_analysis.hpp"

namespace holeyc {

Procedure * IRProgram::makeProc(std::string name){
	Procedure * proc = new Procedure(this, name);
	procs->push_back(proc);
	return proc;
}

std::list<Procedure *> * IRProgram::getProcs(){
	return procs;
}

const DataType * IRProgram::nodeType(ASTNode * node){
	return ta->nodeType(node);
}

OpdWidth IRProgram::opWidth(ASTNode * node){
	return Opd::width(nodeType(node));
}

OpdWidth IRProgram::opDerefWidth(ASTNode * node){
	const DataType * derefType = PtrType::derefType(nodeType(node));
	return Opd::width(derefType);
}

Label * IRProgram::makeLabel(){
	Label * label = new Label("lbl_" + std::to_string(max_label++));
	return label;
}

SymOpd * IRProgram::getGlobal(SemSymbol * sym){
	if (globals.find(sym) != globals.end()){
		return globals[sym];
	} 
	return nullptr;
}

std::map<SemSymbol *, SymOpd *>& IRProgram::getGlobals() { return globals; }

void IRProgram::gatherGlobal(SemSymbol * sym){
	SymOpd * res = new SymOpd(sym, sym->getDataType());
	globals[sym] = res;
}

Opd * IRProgram::makeString(std::string val){
	std::string name = "str_" + std::to_string(str_idx++);
	AuxOpd * opd = new AuxOpd(name, PtrType::produce(BasicType::produce(CHAR), 1));
	strings[opd] = val;
	return opd;
}

std::string IRProgram::toString(bool verbose){
	std::string res = "";
	res += "[BEGIN GLOBALS]\n";
	for (auto entry : globals){
		res += entry.second->getName() + "\n"; 
	}
	for (auto entry : strings){
		res += entry.first->locString();
		res += " " + entry.second; 
		res += "\n";
	}

	res += "[END GLOBALS]\n";
	
	for (Procedure * proc : *procs){
		res += proc->toString(verbose);
	}
	return res;
}

std::set<Opd *> IRProgram::globalSyms(){
	std::set<Opd *> result;
	for (auto gItr : globals){
		result.insert(gItr.second);
	}
	return result;
}

}
