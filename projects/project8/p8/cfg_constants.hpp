#ifndef HOLEYC_CFG_CONSTANTS
#define HOLEYC_CFG_CONSTANTS

#include <map>
#include <optional>
#include "cfg.hpp"
#include "3ac.hpp"

namespace holeyc{

enum ConstantValType {INTVAL, CHARVAL, BOOLVAL, TOPVAL};

/**
* This class represents the possible value that an Opd might take on at
* at some program point. Note that if the Opd might take on more than 
* one possible value at a program point, it is simply represented as
* TOPVAL (regardless of what the values are). This use of TOPVAL causes the 
* algorithm to reach saturation much faster. 
**/
class ConstantVal{
public:
	ConstantVal() {
		type = TOPVAL;
		intVal = 0;
		boolVal = false;
		charVal = 0;
	}
	ConstantVal(int v) {
		type = INTVAL;
		intVal = v;
	}
	ConstantVal(bool v) {
		type = BOOLVAL;
		boolVal = v;
	}
	ConstantVal(char v) {
		type = CHARVAL;
		charVal = v;
	}
	// ConstantVal(const ConstantVal& other) {
	// 	type = other.type;
	// 	if(type == INTVAL) { intVal = other.intVal; }
	// 	else if(type == CHARVAL) { charVal = other.charVal; }
	// 	else if(type == BOOLVAL) { boolVal = other.boolVal; }
	// }
	Opd * toLitOpd() {
		Opd * opd = nullptr;
		if(type == INTVAL) { opd = new LitOpd(std::to_string(intVal), BasicType::produce(INT));}
		else if(type == BOOLVAL) { opd = new LitOpd(boolVal ? "1" : "0", BasicType::produce(BOOL));}
		else if(type == CHARVAL) {
			int i = charVal;
			opd = new LitOpd(std::to_string(i), BasicType::produce(CHAR));
		}
		return opd;
	}
	ConstantValType type;

	int intVal;
	char charVal;
	bool boolVal;
	void setInt(int val){ intVal = val; type = INTVAL; } 
	void setBool(bool val){ boolVal = val; type = BOOLVAL; } 
	void setChar(char val){ charVal = val; type = CHARVAL; } 
	void setTop(){ type = TOPVAL; } 


	void merge(ConstantVal other){
		if (other.type == TOPVAL) {
			setTop(); 
		} else if (other.type == INTVAL) { 
			if (intVal != other.intVal){ setTop(); }
		} else if (other.type == CHARVAL) { 
			if (charVal != other.charVal){ setTop(); }
		} else if (other.type == BOOLVAL) { 
			if (intVal != other.intVal){ setTop(); }
		}
	}

	bool operator==(ConstantVal other) {
		if(type != other.type) { 
			return false; 
		} else {
			if(type != TOPVAL) {
				if (type == INTVAL) { return intVal == other.intVal; }
				else if (type == BOOLVAL) { return boolVal == other.boolVal; }
				else { return charVal == other.charVal; }
			}
		}
	}

	bool operator!=(ConstantVal other) {
		return !(*this == other);
	}
};

class ConstantsFacts{
public:
	ConstantsFacts(){}
	ConstantsFacts clone(){
		ConstantsFacts facts;
		return facts;
	}
	void gen(Opd * opd, ConstantVal v){
		auto itr = vals.find(opd);
		if (itr != vals.end()){
			kill(opd);
		}
		vals[opd] = v;
	}
	void kill(Opd * opd){
		vals.erase(opd);
	}
	void killGlobalFacts(ControlFlowGraph * cfg) {
		std::map<SemSymbol *, SymOpd *> gbls_map = cfg->getProc()->getProg()->getGlobals();
		for(auto pair : gbls_map) {
			Opd * opd = pair.second;
			auto it = vals.find(opd);
			if(it != vals.end()) {
				kill(opd);
			}
		}
	}
	ConstantVal * lookupVal(Opd * opd) {
		auto itr = vals.find(opd);
		if (itr != vals.end()) {
			return new ConstantVal(itr->second);
		}
		return nullptr;
	}
	// generate a new ConstantsFacts object by merging two others
	static ConstantsFacts mergeFacts(ConstantsFacts l, ConstantsFacts r) {
		for(auto pair : l.vals) {
			Opd * curOpd = pair.first;
			ConstantVal curVal = pair.second;
			auto itr = r.vals.find(curOpd);
			// if opd in left clashes with opd in right,
			// merge the values and add back to map
			if (itr != r.vals.end()) {
				curVal.merge(itr->second);
				l.vals.erase(curOpd);
				l.vals[curOpd] = curVal;
			}
		}
		for(auto pair : r.vals) {
			Opd * curOpd = pair.first;
			ConstantVal curVal = pair.second;
			auto itr = l.vals.find(curOpd);
			// if opd in right does not appear in left,
			// add (opd, val) to facts
			if (itr == l.vals.end()) {
				l.vals[curOpd] = curVal;
			}
		}
		return l;
	}
	//TODO: You'll probably want to add some extra convenience functions
	// to, for example, merge two constantsFacts objects, or determine if
	// a given set of values changes a fact set to determine if you've reached
	// saturation.
private:
	std::map<Opd *, ConstantVal> vals;
};

class ConstantsAnalysis{
public:
	static bool run(ControlFlowGraph * cfg){
		ConstantsAnalysis ca;
		return ca.runGraph(cfg);
	}
private:
	ConstantsAnalysis() : effectful(false){}
	bool runGraph(ControlFlowGraph * cfg); 
	bool runBlock(ControlFlowGraph * cfg, BasicBlock * block); 

	std::map<BasicBlock *, ConstantsFacts> outFacts;
	std::map<BasicBlock *, ConstantsFacts> inFacts;
	bool effectful;
};

AssignQuad * tryBinaryFold(BinOpQuad * q, ConstantsFacts& facts);
AssignQuad * tryUnaryFold(UnaryOpQuad * q, ConstantsFacts& facts);

}

#endif
