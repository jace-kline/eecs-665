#ifndef HOLEYC_AST_HPP
#define HOLEYC_AST_HPP

#include <ostream>
#include <sstream>
#include <string.h>
#include <list>
#include "errors.hpp"
#include "tokens.hpp"
#include "types.hpp"
#include "symbol_table.hpp"

namespace holeyc {

class TypeAnalysis;

class Opd;

class SymbolTable;
class SemSymbol;

class DerefNode;
class RefNode;
class DeclNode;
class VarDeclNode;
class StmtNode;
class AssignExpNode;
class FormalDeclNode;
class TypeNode;
class StructTypeNode;
class ExpNode;
class LValNode;
class IDNode;

class ASTNode{
public:
	ASTNode(size_t lineIn, size_t colIn)
	: l(lineIn), c(colIn){ }
	virtual void unparse(std::ostream&, int) = 0;
	size_t line() const { return this->l; }
	size_t col() const { return this->c; }
	std::string pos(){
		return "[" + std::to_string(line()) + ","
			+ std::to_string(col()) + "]";
	}
	// by default, should eval to true
	virtual bool nameAnalysis(SymbolTable *);
	// by default, return NoType instance
	virtual DataType * typeAnalysis(TypeAnalysis *);
private:
	size_t l;
	size_t c;
};

class ProgramNode : public ASTNode{
public:
	ProgramNode(std::list<DeclNode *> * globalsIn)
	: ASTNode(1,1), myGlobals(globalsIn){}
	void unparse(std::ostream&, int) override;
	virtual bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	std::list<DeclNode *> * myGlobals;
};

class ExpNode : public ASTNode{
public:
	ExpNode(size_t lIn, size_t cIn) : ASTNode(lIn, cIn){ }
	virtual void unparseNested(std::ostream& out);
	virtual void unparse(std::ostream& out, int indent) override = 0;
	virtual bool nameAnalysis(SymbolTable *) override {return true;}
	virtual DataType * typeAnalysis(TypeAnalysis *) override = 0;
};

class LValNode : public ExpNode{
public:
	LValNode(size_t lIn, size_t cIn) : ExpNode(lIn, cIn){}
	void unparse(std::ostream& out, int indent) override = 0;
	void unparseNested(std::ostream& out) override;
	virtual bool nameAnalysis(SymbolTable *) override = 0;
	virtual DataType * typeAnalysis(TypeAnalysis *) override = 0;
};

class IDNode : public LValNode{
public:
	IDNode(size_t lIn, size_t cIn, std::string nameIn)
	: LValNode(lIn, cIn), name(nameIn), isDecl(false), mySymbol(nullptr) {}
	std::string getName(){ return name; }
	SemSymbol * getSymbol() { return mySymbol; }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
	void linkSymbol(SemSymbol * symbol) {
		mySymbol = symbol;
	}
	void setDecl() {
		isDecl = true;
	}
private:
	std::string name;
	// if this is a declaration, we don't print type annotation
	bool isDecl;
	// semantic symbol to point to (after name analysis)
	SemSymbol * mySymbol;
};

class RefNode : public LValNode{
public:
	RefNode(size_t l, size_t c, IDNode * id)
	: LValNode(l, c), myID(id){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	IDNode * myID;
};

class DerefNode : public LValNode{
public:
	DerefNode(size_t l, size_t c, IDNode * id)
	: LValNode(l, c), myID(id){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	IDNode * myID;
};

class IndexNode : public LValNode{
public:
	IndexNode(size_t l, size_t c, IDNode * id, ExpNode * offset)
	: LValNode(l, c), myBase(id), myOffset(offset){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	IDNode * myBase;
	ExpNode * myOffset;
};


class TypeNode : public ASTNode{
public:
	TypeNode(size_t l, size_t c) : ASTNode(l, c){ }
	void unparse(std::ostream&, int) override = 0;
	virtual std::string show() = 0;
	virtual bool nameAnalysis(SymbolTable *) override {return true;}
};

class CharTypeNode : public TypeNode{
public:
	CharTypeNode(size_t lIn, size_t cIn, bool isPtrIn)
	: TypeNode(lIn, cIn), isPtr(isPtrIn){}
	void unparse(std::ostream& out, int indent) override;
	std::string show() override { return (isPtr ? "charptr" : "char"); }
private:
	bool isPtr;
};


class StmtNode : public ASTNode{
public:
	StmtNode(size_t lIn, size_t cIn) : ASTNode(lIn, cIn){ }
	virtual void unparse(std::ostream& out, int indent) override = 0;
	virtual bool nameAnalysis(SymbolTable *) override = 0;
	virtual DataType * typeAnalysis(TypeAnalysis *) override = 0;
};

class DeclNode : public StmtNode{
public:
	DeclNode(size_t l, size_t c) : StmtNode(l, c){ }
	void unparse(std::ostream& out, int indent) override =0;
	virtual bool nameAnalysis(SymbolTable *) override = 0;
	virtual DataType * typeAnalysis(TypeAnalysis *) override;
};

class VarDeclNode : public DeclNode{
public:
	VarDeclNode(size_t lIn, size_t cIn, TypeNode * typeIn, IDNode * IDIn)
	: DeclNode(lIn, cIn), myType(typeIn), myID(IDIn){ }
	void unparse(std::ostream& out, int indent) override;
	IDNode * ID(){ return myID; }
	TypeNode * getTypeNode(){ return myType; }
	bool nameAnalysis(SymbolTable *) override;
private:
	TypeNode * myType;
	IDNode * myID;
};

class FormalDeclNode : public VarDeclNode{
public:
	FormalDeclNode(size_t lIn, size_t cIn, TypeNode * type, IDNode * id) 
	: VarDeclNode(lIn, cIn, type, id){ }
	void unparse(std::ostream& out, int indent) override;
};

class FnDeclNode : public DeclNode{
public:
	FnDeclNode(size_t lIn, size_t cIn, 
	  TypeNode * retTypeIn, IDNode * idIn,
	  std::list<FormalDeclNode *> * formalsIn,
	  std::list<StmtNode *> * bodyIn)
	: DeclNode(lIn, cIn), 
	  myID(idIn), myRetType(retTypeIn),
	  myFormals(formalsIn), myBody(bodyIn){ }
	IDNode * ID() const { return myID; }
	std::list<FormalDeclNode *> * getFormals() const{
		return myFormals;
	}
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	IDNode * myID;
	TypeNode * myRetType;
	std::list<FormalDeclNode *> * myFormals;
	std::list<StmtNode *> * myBody;
};

class AssignStmtNode : public StmtNode{
public:
	AssignStmtNode(size_t l, size_t c, AssignExpNode * expIn)
	: StmtNode(l, c), myExp(expIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	AssignExpNode * myExp;
};

class FromConsoleStmtNode : public StmtNode{
public:
	FromConsoleStmtNode(size_t l, size_t c, LValNode * dstIn)
	: StmtNode(l, c), myDst(dstIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	LValNode * myDst;
};

class ToConsoleStmtNode : public StmtNode{
public:
	ToConsoleStmtNode(size_t l, size_t c, ExpNode * srcIn)
	: StmtNode(l, c), mySrc(srcIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	ExpNode * mySrc;
};

class PostDecStmtNode : public StmtNode{
public:
	PostDecStmtNode(size_t l, size_t c, LValNode * lvalIn)
	: StmtNode(l, c), myLVal(lvalIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	LValNode * myLVal;
};

class PostIncStmtNode : public StmtNode{
public:
	PostIncStmtNode(size_t l, size_t c, LValNode * lvalIn)
	: StmtNode(l, c), myLVal(lvalIn){ }
	void unparse(std::ostream& out, int indent) override;
	virtual bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	LValNode * myLVal;
};

class IfStmtNode : public StmtNode{
public:
	IfStmtNode(size_t l, size_t c, ExpNode * condIn,
	  std::list<StmtNode *> * bodyIn)
	: StmtNode(l, c), myCond(condIn), myBody(bodyIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	ExpNode * myCond;
	std::list<StmtNode *> * myBody;
};

class IfElseStmtNode : public StmtNode{
public:
	IfElseStmtNode(size_t l, size_t c, ExpNode * condIn, 
	  std::list<StmtNode *> * bodyTrueIn,
	  std::list<StmtNode *> * bodyFalseIn)
	: StmtNode(l, c), myCond(condIn),
	  myBodyTrue(bodyTrueIn), myBodyFalse(bodyFalseIn) { }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	ExpNode * myCond;
	std::list<StmtNode *> * myBodyTrue;
	std::list<StmtNode *> * myBodyFalse;
};

class WhileStmtNode : public StmtNode{
public:
	WhileStmtNode(size_t l, size_t c, ExpNode * condIn, 
	  std::list<StmtNode *> * bodyIn)
	: StmtNode(l, c), myCond(condIn), myBody(bodyIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	ExpNode * myCond;
	std::list<StmtNode *> * myBody;
};

class ReturnStmtNode : public StmtNode{
public:
	ReturnStmtNode(size_t l, size_t c, ExpNode * exp)
	: StmtNode(l, c), myExp(exp){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	ExpNode * myExp;
};

class CallExpNode : public ExpNode{
public:
	CallExpNode(size_t l, size_t c, IDNode * id,
	  std::list<ExpNode *> * argsIn)
	: ExpNode(l, c), myID(id), myArgs(argsIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	IDNode * myID;
	std::list<ExpNode *> * myArgs;
};

class BinaryExpNode : public ExpNode{
public:
	BinaryExpNode(size_t lIn, size_t cIn, ExpNode * lhs, ExpNode * rhs)
	: ExpNode(lIn, cIn), myExp1(lhs), myExp2(rhs) { }
	bool nameAnalysis(SymbolTable *) override;
	virtual DataType * typeAnalysis(TypeAnalysis *) override = 0;
protected:
	DataType * typeAnalysisHelper(TypeAnalysis *, Type argtype, Type rettype);
	ExpNode * myExp1;
	ExpNode * myExp2;
};

class PlusNode : public BinaryExpNode{
public:
	PlusNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class MinusNode : public BinaryExpNode{
public:
	MinusNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class TimesNode : public BinaryExpNode{
public:
	TimesNode(size_t l, size_t c, ExpNode * e1In, ExpNode * e2In)
	: BinaryExpNode(l, c, e1In, e2In){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class DivideNode : public BinaryExpNode{
public:
	DivideNode(size_t lIn, size_t cIn, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(lIn, cIn, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class AndNode : public BinaryExpNode{
public:
	AndNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class OrNode : public BinaryExpNode{
public:
	OrNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class EqualsNode : public BinaryExpNode{
public:
	EqualsNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class NotEqualsNode : public BinaryExpNode{
public:
	NotEqualsNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class LessNode : public BinaryExpNode{
public:
	LessNode(size_t lineIn, size_t colIn, 
		ExpNode * exp1, ExpNode * exp2)
	: BinaryExpNode(lineIn, colIn, exp1, exp2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class LessEqNode : public BinaryExpNode{
public:
	LessEqNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class GreaterNode : public BinaryExpNode{
public:
	GreaterNode(size_t lineIn, size_t colIn, 
		ExpNode * exp1, ExpNode * exp2)
	: BinaryExpNode(lineIn, colIn, exp1, exp2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class GreaterEqNode : public BinaryExpNode{
public:
	GreaterEqNode(size_t l, size_t c, ExpNode * e1, ExpNode * e2)
	: BinaryExpNode(l, c, e1, e2){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class UnaryExpNode : public ExpNode {
public:
	UnaryExpNode(size_t lIn, size_t cIn, ExpNode * expIn) 
	: ExpNode(lIn, cIn){
		this->myExp = expIn;
	}
	virtual void unparse(std::ostream& out, int indent) override = 0;
	bool nameAnalysis(SymbolTable *) override;
	virtual DataType * typeAnalysis(TypeAnalysis *) override = 0;
protected:
	ExpNode * myExp;
};

class NegNode : public UnaryExpNode{
public:
	NegNode(size_t l, size_t c, ExpNode * exp)
	: UnaryExpNode(l, c, exp){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class NotNode : public UnaryExpNode{
public:
	NotNode(size_t lIn, size_t cIn, ExpNode * exp)
	: UnaryExpNode(lIn, cIn, exp){ }
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class VoidTypeNode : public TypeNode{
public:
	VoidTypeNode(size_t l, size_t c) : TypeNode(l, c){}
	void unparse(std::ostream& out, int indent) override;
	std::string show() override { return "void"; }
};

class IntTypeNode : public TypeNode{
public:
	IntTypeNode(size_t l, size_t c, bool ptrIn): TypeNode(l, c), isPtr(ptrIn){}
	void unparse(std::ostream& out, int indent) override;
	std::string show() override { return (isPtr ? "intptr" : "int"); }
private:
	const bool isPtr;
};

class BoolTypeNode : public TypeNode{
public:
	BoolTypeNode(size_t l, size_t c, bool ptrIn): TypeNode(l, c), isPtr(ptrIn) { }
	void unparse(std::ostream& out, int indent) override;
	std::string show() override { return (isPtr ? "boolptr" : "bool"); }
private:
	const bool isPtr;
};

class AssignExpNode : public ExpNode{
public:
	AssignExpNode(size_t l, size_t c, LValNode * dstIn, ExpNode * srcIn)
	: ExpNode(l, c), myDst(dstIn), mySrc(srcIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	LValNode * myDst;
	ExpNode * mySrc;
};

class IntLitNode : public ExpNode{
public:
	IntLitNode(size_t l, size_t c, const int numIn)
	: ExpNode(l, c), myNum(numIn){ }
	virtual void unparseNested(std::ostream& out) override{
		unparse(out, 0);
	}
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	const int myNum;
};

class StrLitNode : public ExpNode{
public:
	StrLitNode(size_t l, size_t c, const std::string strIn)
	: ExpNode(l, c), myStr(strIn){ }
	virtual void unparseNested(std::ostream& out) override{
		unparse(out, 0);
	}
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	 const std::string myStr;
};

class CharLitNode : public ExpNode{
public:
	CharLitNode(size_t l, size_t c, const char valIn)
	: ExpNode(l, c), myVal(valIn){ }
	virtual void unparseNested(std::ostream& out) override{
		unparse(out, 0);
	}
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	 const char myVal;
};

class NullPtrNode : public ExpNode{
public:
	NullPtrNode(size_t l, size_t c): ExpNode(l, c){ }
	virtual void unparseNested(std::ostream& out) override{
		unparse(out, 0);
	}
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class TrueNode : public ExpNode{
public:
	TrueNode(size_t l, size_t c): ExpNode(l, c){ }
	virtual void unparseNested(std::ostream& out) override{
		unparse(out, 0);
	}
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class FalseNode : public ExpNode{
public:
	FalseNode(size_t l, size_t c): ExpNode(l, c){ }
	virtual void unparseNested(std::ostream& out) override{
		unparse(out, 0);
	}
	void unparse(std::ostream& out, int indent) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
};

class CallStmtNode : public StmtNode{
public:
	CallStmtNode(size_t l, size_t c, CallExpNode * expIn)
	: StmtNode(l, c), myCallExp(expIn){ }
	void unparse(std::ostream& out, int indent) override;
	bool nameAnalysis(SymbolTable *) override;
	DataType * typeAnalysis(TypeAnalysis *) override;
private:
	CallExpNode * myCallExp;
};

} //End namespace holeyc

#endif

