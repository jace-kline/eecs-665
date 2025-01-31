#ifndef HOLEYC_3AC_HPP
#define HOLEYC_3AC_HPP

#include <assert.h>
#include "list"
#include "map"
#include "err.hpp"
#include "symbol_table.hpp"

namespace holeyc{

class TypeAnalysis;
class Procedure;
class IRProgram;

std::string quadRegByte0(std::string reg);

class Label{
public:
	Label(std::string nameIn){
		this->name = nameIn;
	}
	std::string toString(){
		return name == "main" ? "main" : "lbl_" + this->name;
	}
	std::string getName(){
		return name;
	}
private:
	std::string name;
};

enum OpdWidth{
	ADDR, QUADWORD, BYTE
};

class Opd{
public:
	Opd(OpdWidth widthIn, const DataType * typeIn) : myWidth(widthIn), myType(typeIn), isGlobal(false), isString(false), derefOpd(nullptr) {}
	virtual std::string valString() = 0;
	virtual std::string locString() = 0;
	virtual OpdWidth getWidth(){ return myWidth; }
	virtual void genLoad(std::ostream& out, std::string dstReg) = 0;
	virtual void genStore(std::ostream& out, std::string srcReg) = 0;
	virtual std::string opdX64Repr() = 0;
	bool getIsGlobal() { return isGlobal; }
	void setIsGlobal() { isGlobal = true; }
	bool getIsString() { return isString; }
	void setIsString() { isString = true; }
	const DataType * getDataType() { return myType; }
	Opd * getDerefOpd() { return derefOpd; }
	void setDeref(Opd * opd) { derefOpd = opd; }
	static OpdWidth width(const DataType * type){
		if (const BasicType * basic = type->asBasic()){
			if (basic->isChar()){ return BYTE; }
			if (basic->isBool()){ return BYTE; }
			return QUADWORD;
		} else if (const PtrType * ptr = type->asPtr()){
			return ADDR;
		}
		assert(false);
	}
private:
	OpdWidth myWidth;
	const DataType * myType;
	bool isGlobal;
	bool isString;
	Opd * derefOpd;
};

class SymOpd : public Opd{
public:
	virtual std::string valString() override{
		return "[" + mySym->getName() + "]";
	}
	virtual std::string locString() override{
		return mySym->getName();
	}
	std::string getName(){
		return mySym->getName();
	}
	const SemSymbol * getSym(){ return mySym; }
	virtual void genLoad(std::ostream& out, std::string loc)
		override;
	virtual void genStore(std::ostream& out, std::string loc)
		override;
	virtual void setMemoryLoc(std::string loc){
		myLoc = loc;
	}
	virtual std::string getMemoryLoc(){
		return myLoc;
	}
	std::string opdX64Repr() { 
		return getIsGlobal() ? "(" + getMemoryLoc() + ")" : getMemoryLoc(); 
	}
private:
	//Private Constructor
	SymOpd(SemSymbol * sym, OpdWidth width)
	: Opd(width, sym->getDataType()), mySym(sym) {} 
	SemSymbol * mySym;
	friend class Procedure;
	friend class IRProgram;
	std::string myLoc;
};

class LitOpd : public Opd{
public:
	LitOpd(std::string valIn, OpdWidth width, const DataType * dt)
	: Opd(width, dt), val(valIn){ }
	virtual std::string valString() override{
		return val;
	}
	virtual std::string locString() override{
		throw InternalError("Tried to get location of a constant");
	}
	virtual void genLoad(std::ostream& out, std::string loc)
		override;
	virtual void genStore(std::ostream& out, std::string loc)
		override;
	std::string opdX64Repr() { return "$" + val; }
private:
	std::string val;
};

class AuxOpd : public Opd{
public:
	AuxOpd(std::string nameIn, OpdWidth width, const DataType * dt) 
	: Opd(width, dt), name(nameIn) { }
	virtual std::string valString() override{
		return "[" + getName() + "]";
	}
	virtual std::string locString() override{
		return getName();
	}
	std::string getName(){
		return name;
	}
	virtual void genLoad(std::ostream& out, std::string loc)
		override;
	virtual void genStore(std::ostream& out, std::string loc)
		override;
	virtual void setMemoryLoc(std::string loc){
		myLoc = loc;
	}
	virtual std::string getMemoryLoc(){
		return myLoc;
	}
	std::string opdX64Repr() { 
		return getMemoryLoc(); 
	}
private:
	std::string name;
	std::string myLoc = "UNINIT";
};

enum BinOp {
	ADD, SUB, DIV, MULT, OR, AND, EQ, NEQ, LT, GT, LTE, GTE
};

bool isCmpOp(BinOp op);
std::string binOpToX64(BinOp op);

enum UnaryOp{
	NEG, NOT
};

std::string unOpToX64(UnaryOp op);

enum Intrinsic {
	OUTPUT, INPUT
};

class Quad{
public:
	Quad();
	void addLabel(Label * label);
	virtual std::string repr() = 0;
	std::string commentStr();
	virtual std::string toString(bool verbose=false);
	void setComment(std::string commentIn);
	virtual void codegenX64(std::ostream& out) = 0;
	void codegenLabels(std::ostream& out);
private:
	std::string myComment;
	std::list<Label *> labels;
};

class BinOpQuad : public Quad{
public:
	BinOpQuad(Opd * dstIn, BinOp opIn, Opd * src1In, Opd * src2In);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Opd * dst;
	BinOp op;
	Opd * src1;
	Opd * src2;
};

class UnaryOpQuad : public Quad {
public:
	UnaryOpQuad(Opd * dstIn, UnaryOp opIn, Opd * srcIn);
	std::string repr() override ;
	void codegenX64(std::ostream& out) override;
private:
	Opd * dst;
	UnaryOp op;
	Opd * src;
};

class AssignQuad : public Quad{
	
public:
	AssignQuad(Opd * dstIn, Opd * srcIn)
	: dst(dstIn), src(srcIn)
	{ }
	std::string repr() override;
	void codegenX64(std::ostream& out) override;

private:
	Opd * dst;
	Opd * src;
};

class LocQuad : public Quad {
public:
	LocQuad(Opd * srcIn, Opd * tgtIn, bool srcLocIn)
	: src(srcIn), tgt(tgtIn), srcLoc(srcLocIn){ }
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Opd * src;
	Opd * tgt;
	bool srcLoc;
};

class JmpQuad : public Quad {
public:
	JmpQuad(Label * tgtIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Label * tgt;
};

class JmpIfQuad : public Quad {
public:
	JmpIfQuad(Opd * cndIn, Label * tgtIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Opd * cnd;
	Label * tgt;
};

class NopQuad : public Quad {
public:
	NopQuad();
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
};

class IntrinsicQuad : public Quad {
public:
	IntrinsicQuad(Intrinsic intrinsic, Opd * arg);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Opd * myArg;
	Intrinsic myIntrinsic;
};

class CallQuad : public Quad{
public:
	CallQuad(SemSymbol * calleeIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	SemSymbol * callee;
};

class EnterQuad : public Quad{
public:
	EnterQuad(Procedure * proc);
	virtual std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Procedure * myProc;
};

class LeaveQuad : public Quad{
public:
	LeaveQuad(Procedure * proc);
	virtual std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Procedure * myProc;
};

std::string indexToReg(size_t index);

class SetArgQuad : public Quad{
public:
	SetArgQuad(size_t indexIn, Opd * opdIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	size_t index;
	Opd * opd;
};

class GetArgQuad : public Quad{
public:
	GetArgQuad(size_t indexIn, Opd * opdIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	size_t index;
	Opd * opd;
};

class SetRetQuad : public Quad{
public:
	SetRetQuad(Opd * opdIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Opd * opd;
};

class GetRetQuad : public Quad{
public:
	GetRetQuad(Opd * opdIn);
	std::string repr() override;
	void codegenX64(std::ostream& out) override;
private:
	Opd * opd;
};

class Procedure{
public:
	Procedure(IRProgram * prog, std::string name);
	void addQuad(Quad * quad);
	Quad * popQuad();
	IRProgram * getProg();
	std::list<SymOpd *> getFormals() { return formals; }
	holeyc::Label * makeLabel();

	void gatherLocal(SemSymbol * sym);
	void gatherFormal(SemSymbol * sym);
	SymOpd * getSymOpd(SemSymbol * sym);
	AuxOpd * makeTmp(OpdWidth width, const DataType * dt);

	std::string toString(bool verbose=false); 
	std::string getName();

	holeyc::Label * getLeaveLabel();

	void toX64(std::ostream& out);
	size_t localsSize() const;
	size_t numTemps() const;
	int getAllocBytes() const { return allocBytes; }

	std::list<Quad *> * getQuads(){
		return bodyQuads;
	}
private:
	void allocLocals();

	EnterQuad * enter;
	Quad * leave;
	Label * leaveLabel;

	IRProgram * myProg;
	std::map<SemSymbol *, SymOpd *> locals;
	std::list<AuxOpd *> temps; 
	std::list<SymOpd *> formals; 
	std::list<Quad *> * bodyQuads;
	std::string myName;
	size_t maxTmp;

	// bytes to reserved for formals, locals, temps
	// ensure 16-byte aligned
	int allocBytes;
};

class IRProgram{
public:
	IRProgram(TypeAnalysis * taIn) : ta(taIn){}
	Procedure * makeProc(std::string name);
	std::list<Procedure *> getProcs();
	Label * makeLabel();
	Opd * makeString(std::string val);
	void gatherGlobal(SemSymbol * sym);
	SymOpd * getGlobal(SemSymbol * sym);
	OpdWidth opWidth(ASTNode * node);

	std::string toString(bool verbose=false);

	void toX64(std::ostream& out);
private:
	TypeAnalysis * ta;
	size_t max_label = 0;
	size_t str_idx = 0;
	std::list<Procedure *> procs; 
	HashMap<AuxOpd *, std::string> strings;
	std::map<SemSymbol *, SymOpd *> globals;

	void datagenX64(std::ostream& out);
	void allocGlobals();
};

}

#endif 
