#ifndef HOLEYC_AST_HPP
#define HOLEYC_AST_HPP

#include <ostream>
#include <list>
#include "tokens.hpp"

// **********************************************************************
// ASTnode class (base class for all other kinds of nodes)
// **********************************************************************

namespace holeyc{

/* You may find it useful to forward declare AST subclasses
   here so that you can use a class before it's full definition
*/
class DeclListNode;
class DeclNode;
class FormalDeclNode;
class StmtNode;
class TypeNode;
class LValNode;
class IDNode;
class AssignExpNode;
class FnCallNode;
class ExpNode;
class VarDeclNode;

class ASTNode{
public:
	ASTNode(size_t lineIn, size_t colIn)
	: l(lineIn), c(colIn){
	}
	virtual void unparse(std::ostream& out, int indent) = 0;
	size_t line(){ return l; }
	size_t col() { return c; }

	/**
	* Return a string specifying the position this node begins.
	* For some node a position doesn't really make sense (i.e.
	* ProgramNode) but for the rest it's the position in the 
	* input file that represents that node
	**/
	std::string pos(){
		return "[" + std::to_string(line()) + ","
		+ std::to_string(col()) + "]";
	}

private:
	size_t l; /// The line at which the node starts in the input file
	size_t c; /// The column at which the node starts in the input file
};

/** 
* \class ProgramNode
* Class that contains the entire abstract syntax tree for a program.
* Note the list of declarations encompasses all global declarations
* which includes (obviously) all global variables and struct declarations
* and (perhaps less obviously), all function declarations
**/
class ProgramNode : public ASTNode{
public:
	ProgramNode(std::list<DeclNode *> * globalsIn) 
	: ASTNode(1, 1), globals_(globalsIn){
	}
	void unparse(std::ostream& out, int indent) override;
private:
	std::list<DeclNode * > * globals_;
};


class StmtNode : public ASTNode{
public:
	StmtNode(size_t line, size_t col) 
	: ASTNode(line, col) {
	}
	virtual void unparse(std::ostream& out, int indent) = 0;
};

class FnBodyNode : public ASTNode{
public:
	FnBodyNode(size_t line, size_t col, std::list<StmtNode *> * stmtList) 
	: ASTNode(line, col), stmtList_(stmtList) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	std::list<StmtNode *> * stmtList_;
};

class FormalsNode : public ASTNode{
	public:
	FormalsNode(size_t line, size_t col, std::list<FormalDeclNode *> * formalsList) 
	: ASTNode(line, col), formalsList_(formalsList) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	std::list<FormalDeclNode *> * formalsList_;
};

class VarDeclStmtNode : public StmtNode{
public:
	VarDeclStmtNode(size_t line, size_t col, VarDeclNode* varDecl) 
	: StmtNode(line, col), varDecl_(varDecl) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	VarDeclNode* varDecl_;
};

class AssignStmtNode : public StmtNode{
public:
	AssignStmtNode(size_t line, size_t col, AssignExpNode* assignExp) 
	: StmtNode(line, col), assignExp_(assignExp) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	AssignExpNode* assignExp_;
};

enum LValStmtType {INC, DEC, FROMCONSOLE};

class LValStmtNode : public StmtNode{
public:
	LValStmtNode(size_t line, size_t col, LValNode* lval, LValStmtType t) 
	: StmtNode(line, col), lval_(lval), t_(t) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	LValNode* lval_;
	LValStmtType t_;
};


enum ExpStmtType { TOCONSOLE, RETURN };

class ExpStmtNode : public StmtNode{
public:
	ExpStmtNode(size_t line, size_t col, ExpNode* exp, ExpStmtType t) 
	: StmtNode(line, col), exp_(exp), t_(t) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	ExpNode* exp_;
	ExpStmtType t_;
};


enum CondStmtType {IF, IFELSE, WHILE};

class CondStmtNode : public StmtNode {
public:
	CondStmtNode(size_t line, size_t col, 
				ExpNode* exp, 
				std::list<StmtNode *> * stmtList1,
				std::list<StmtNode *> * stmtList2,
				CondStmtType t) 
	: StmtNode(line, col), exp_(exp), stmtList1_(stmtList1), stmtList2_(stmtList2), t_(t){
	}
	void unparse(std::ostream& out, int indent) override;
private:
	ExpNode* exp_;
	std::list<StmtNode *> * stmtList1_;
	std::list<StmtNode *> * stmtList2_;
	CondStmtType t_;
};

class FnCallStmtNode : public StmtNode{
public:
	FnCallStmtNode(size_t line, size_t col, FnCallNode* fncall) 
	: StmtNode(line, col), fncall_(fncall) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	FnCallNode* fncall_;
};

/**  \class ExpNode
* Superclass for expression nodes (i.e. nodes that can be used as
* part of an expression).  Nodes that are part of an expression
* should inherit from this abstract superclass.
**/
class ExpNode : public ASTNode{
public:
	ExpNode(size_t line, size_t col) 
	: ASTNode(line, col){
	}
	virtual void unparse(std::ostream& out, int indent) = 0;
};

class FnCallNode : public ExpNode {
public:
	FnCallNode(size_t line, size_t col, IDNode* id, std::list<ExpNode *> * argsList)
	: ExpNode(line,col), id_(id), argsList_(argsList){
	}
	void unparse(std::ostream& out, int indent) override;
private:
	IDNode* id_;
	std::list<ExpNode *> * argsList_;
};

class AssignExpNode : public ExpNode{
public:
	AssignExpNode(size_t line, size_t col, LValNode* lval, ExpNode* exp) 
	: ExpNode(line, col), lval_(lval), exp_(exp) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	LValNode* lval_;
	ExpNode* exp_;
};

enum BinOpType {DASH_BIN, CROSS, STAR, SLASH, AND, OR, EQUALS, NOTEQUALS, GREATER, GREATEREQ, LESS, LESSEQ};

class BinOpExpNode : public ExpNode{
public:
	BinOpExpNode(size_t line, size_t col, ExpNode* left, ExpNode* right, BinOpType op) 
	: ExpNode(line, col), left_(left), right_(right), op_(op) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	ExpNode* left_;
	ExpNode* right_;
	BinOpType op_;
};


enum UnOpType {NOT, DASH_UN};

class UnOpExpNode : public ExpNode{
public:
	UnOpExpNode(size_t line, size_t col, ExpNode* exp, UnOpType op) 
	: ExpNode(line, col), exp_(exp), op_(op) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	ExpNode* exp_;
	UnOpType op_;
};


class LValNode : public ExpNode {
protected:
	LValNode(size_t line, size_t col) 
	: ExpNode(line, col) {
	}
public:
	virtual void unparse(std::ostream& out, int indent) = 0;
};


enum LValUnOpType {AT, CARAT};

class LValUnOpNode : public LValNode {
public:
	LValUnOpNode(size_t line, size_t col, IDNode* id, LValUnOpType t)
	: LValNode(line, col), id_(id), t_(t){
	}
	void unparse(std::ostream& out, int indent) override;
private:
	IDNode* id_;
	LValUnOpType t_;
};

class LValIndexNode : public LValNode {
public:
	LValIndexNode(size_t line, size_t col, IDNode* id, ExpNode* exp)
	: LValNode(line, col), id_(id), exp_(exp){
	}
	void unparse(std::ostream& out, int indent) override;
private:
	IDNode* id_;
	ExpNode* exp_;
};

enum TermPrimitiveType {TRUE, FALSE, NULLPTR};

class TermPrimitiveNode : public ExpNode{
public:
	TermPrimitiveNode(size_t line, size_t col, TermPrimitiveType t) 
	: ExpNode(line, col), t_(t) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	TermPrimitiveType t_;
};


class TermGrpNode : public ExpNode {
	public:
	TermGrpNode(size_t line, size_t col, ExpNode* exp)
	: ExpNode(line, col), exp_(exp){
	}
	void unparse(std::ostream& out, int indent) override;
private:
	ExpNode* exp_;
};

class IntLitNode : public ExpNode {
public:
	IntLitNode(IntLitToken * token) 
	: ExpNode(token->line(), token->col()), val_(token->num()) {
		val_ = token->num();
	}
	void unparse(std::ostream& out, int indent) override;
private:
	int val_;
};

class CharLitNode : public ExpNode {
public:
	CharLitNode(CharLitToken * token) 
	: ExpNode(token->line(), token->col()), val_(token->val()) {
		val_ = token->val();
	}
	void unparse(std::ostream& out, int indent) override;
private:
	char val_;
};

class StrLitNode : public ExpNode{
public:
	StrLitNode(StrToken * token) 
	: ExpNode(token->line(), token->col()), val_(token->str()) {
		val_ = token->str();
	}
	void unparse(std::ostream& out, int indent) override;
private:
	std::string val_;
};

// Decl Nodes

/** \class DeclNode
* Superclass for declarations (i.e. nodes that can be used to 
* declare a struct, function, variable, etc).  This base class will 
**/
class DeclNode : public ASTNode{
public:
	DeclNode(size_t line, size_t col) 
	: ASTNode(line, col) {
	}
	virtual void unparse(std::ostream& out, int indent) = 0;
};

class VarDeclNode : public DeclNode{
public:
	VarDeclNode(size_t l, size_t c, TypeNode * type, IDNode * id) 
	: DeclNode(l, c), type_(type), id_(id){
	}
	virtual void unparse(std::ostream& out, int indent);
protected:
	TypeNode * type_;
	IDNode * id_;
};

class FnDeclNode : public DeclNode{
public:
	FnDeclNode(size_t l, size_t c, 
			   TypeNode * type, 
			   IDNode * id, 
			   FormalsNode * formals, 
			   FnBodyNode * fnBody)
	: DeclNode(l,c), type_(type), id_(id), formals_(formals), fnBody_(fnBody) {
	}
	void unparse(std::ostream& out, int indent);
private:
	TypeNode * type_;
	IDNode * id_;
	FormalsNode * formals_;
	FnBodyNode * fnBody_;
};

class FormalDeclNode : public VarDeclNode{
public:
	FormalDeclNode(size_t l, size_t c, TypeNode* type, IDNode* id)
	: VarDeclNode(l,c,type,id) {
	}
	void unparse(std::ostream& out, int indent) override;
};


/**  \class TypeNode
* Superclass of nodes that indicate a data type. For example, in 
* the declaration "int a", the int part is the type node (a is an IDNode
* and the whole thing is a DeclNode).
**/

enum TypeNodeType {INT, INTPTR, BOOL, BOOLPTR, CHAR, CHARPTR, VOID};

class TypeNode : public ASTNode{
public:
	TypeNode(size_t lineIn, size_t colIn, TypeNodeType t) 
	: ASTNode(lineIn, colIn), t_(t) {
	}
	void unparse(std::ostream& out, int indent) override;
private:
	TypeNodeType t_;
};

/** An identifier. Note that IDNodes subclass
 * ExpNode because they can be used as part of an expression. 
**/
class IDNode : public LValNode {
public:
	IDNode(IDToken * token) 
	: LValNode(token->line(), token->col()), str_(token->value()){
		str_ = token->value();
	}
	void unparse(std::ostream& out, int indent) override;
private:
	/** The name of the identifier **/
	std::string str_;
};


} //End namespace holeyc

#endif
