#include "ast.hpp"

namespace holeyc{

/*
doIndent is declared static, which means that it can 
only be called in this file (its symbol is not exported).
*/
static void doIndent(std::ostream& out, int indent){
	for (int k = 0 ; k < indent; k++){ out << "\t"; }
}

/*
In this code, the intention is that functions are grouped 
into files by purpose, rather than by class.
If you're used to having all of the functions of a class 
defined in the same file, this style may be a bit disorienting,
though it is legal. Thus, we can have
ProgramNode::unparse, which is the unparse method of ProgramNodes
defined in the same file as DeclNode::unparse, the unparse method
of DeclNodes.
*/


void ProgramNode::unparse(std::ostream& out, int indent){
	/* Oh, hey it's a for-each loop in C++!
	   The loop iterates over each element in a collection
	   without that gross i++ nonsense. 
	 */
	for (auto global : *globals_){
		/* The auto keyword tells the compiler
		   to (try to) figure out what the
		   type of a variable should be from 
		   context. here, since we're iterating
		   over a list of DeclNode *s, it's 
		   pretty clear that global is of 
		   type DeclNode *.
		*/
		global->unparse(out, indent);
	}
}

void FnBodyNode::unparse(std::ostream& out, int indent){
	out << "{\n";
	for(auto stmt : *stmtList_) {
		stmt->unparse(out,indent+1);
		out << "\n";
	}
	doIndent(out,indent);
	out << "}\n";
}

void FormalsNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << "(";
	int i = formalsList_->size();
	for(auto formal : *formalsList_) {
		formal->unparse(out,0);
		i--;
		if(i > 0) out << ", ";
	}
	out << ")";

}

void VarDeclStmtNode::unparse(std::ostream& out, int indent){
	this->varDecl_->unparse(out, indent);
}

void AssignStmtNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	this->assignExp_->unparse(out, 0);
	out << ";";
}

void LValStmtNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	switch(t_) {
		case INC: {
			this->lval_->unparse(out, 0);
			out << "++";
			break;
		}
		case DEC: {
			this->lval_->unparse(out, 0);
			out << "--";
			break;
		}
		case FROMCONSOLE: {
			out << "FROMCONSOLE ";
			this->lval_->unparse(out, 0);
			break;
		}
	}
	out << ";";
}

void ExpStmtNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	switch(t_) {
		case TOCONSOLE: {
			out << "TOCONSOLE ";
			this->exp_->unparse(out, 0);
			break;
		}
		case RETURN: {
			out << "return";
			if(this->exp_ != nullptr) {
				this->exp_->unparse(out, 0);
			}
			break;
		}
	}
	out << ";";
}

void CondStmtNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	out << (t_ == WHILE ? "while(" : "if(");
	this->exp_->unparse(out,0);
	out << "){\n";
	for (auto stmt : *stmtList1_) {
		stmt->unparse(out, indent+1);
		out << "\n";
	}
	doIndent(out,indent);
	out << "}\n";
	if(t_ == IFELSE) {
		doIndent(out, indent);
		out << "else {\n";
		for(auto stmt : *stmtList2_) {
			stmt->unparse(out,indent+1);
			out << "\n";
		}
		doIndent(out, indent);
		out << "}\n";
	}
}

void FnCallStmtNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	this->fncall_->unparse(out,0);
	out << ";";
}

void FnCallNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	this->id_->unparse(out,0);
	out << "(";
	int i = argsList_->size();
	for(auto arg : *argsList_) {
		arg->unparse(out,0);
		i--;
		if(i > 0) out << ", ";
	}
	out << ")";
}

void AssignExpNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	this->lval_->unparse(out,0);
	out << " = (";
	this->exp_->unparse(out,0);
	out << ")";
}

void BinOpExpNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << "(";
	this->left_->unparse(out,0);
	out << " ";
	switch(op_) {
		case DASH_BIN : out << "-"; break;
		case CROSS : out << "+"; break;
		case STAR : out << "*"; break;
		case SLASH : out << "/"; break;
		case AND : out << "&&"; break;
		case OR : out << "||"; break;
		case EQUALS : out << "=="; break;
		case NOTEQUALS : out << "!="; break;
		case GREATER : out << ">"; break;
		case GREATEREQ : out << ">="; break;
		case LESS : out << "<"; break;
		case LESSEQ : out << "<="; break;
	}
	this->right_->unparse(out,0);
	out << ")";
}

void UnOpExpNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << "(";
	switch(op_) {
		case NOT: out << "!"; break;
		case DASH_UN: out << "-"; break;
	}
	this->exp_->unparse(out,0);
	out << ")";
}

void LValUnOpNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << (t_ == AT ? "@" : "^");
	this->id_->unparse(out, 0);
}

void LValIndexNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << this->id_ << "[";
	this->exp_->unparse(out, 0);
	out << "]";
}

void TermPrimitiveNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	switch(t_) {
		case TRUE: out << "true"; break;
		case FALSE: out << "false"; break;
		case NULLPTR: out << "NULLPTR"; break;
	}
}

void TermGrpNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << "(";
	this->exp_->unparse(out,0);
	out << ")";
}

void IntLitNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << this->val_;
}

void CharLitNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << this->val_;
}

void StrLitNode::unparse(std::ostream& out, int indent){
	doIndent(out,indent);
	out << this->val_;
}

void VarDeclNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	this->type_->unparse(out, 0);
	out << " ";
	this->id_->unparse(out, 0);
	out << ";";
}

void FnDeclNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	this->type_->unparse(out, 0);
	out << " ";
	this->id_->unparse(out, 0);
	out << "(";
	this->formals_->unparse(out,0);
	out << ")";
	this->fnBody_->unparse(out,indent);
}

void FormalDeclNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	this->type_->unparse(out, 0);
	out << " ";
	this->id_->unparse(out, 0);
}

// ID Node

void IDNode::unparse(std::ostream& out, int indent){
	out << this->str_;
}

// Type Nodes

void TypeNode::unparse(std::ostream& out, int indent){
	doIndent(out, indent);
	switch(t_)
	{
		case INT: out << "int"; break;
		case INTPTR: out << "intptr"; break;
		case BOOL: out << "bool"; break;
		case BOOLPTR: out << "boolptr"; break;
		case CHAR: out << "char"; break;
		case CHARPTR: out << "charptr"; break;
		case VOID: out << "void"; break;
	}
}


} // End namespace holeyc
