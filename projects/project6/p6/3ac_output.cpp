#include "ast.hpp"

namespace holeyc{

IRProgram * ProgramNode::to3AC(TypeAnalysis * ta){
	IRProgram * prog = new IRProgram(ta);
	for (auto global : *myGlobals){
		global->to3AC(prog);
	}
	return prog;
}

void FnDeclNode::to3AC(IRProgram * prog){
	// Create new procedure object and add to program
	Procedure * proc = prog->makeProc(myID->getName());
	
	// Enter -> handled by constructor

	// Process formal parameters...
	// Each formal parameter should transformed
	// and stored as a SymOpd object in the formals list
	for (auto formal : *myFormals) {
		formal->to3AC(proc);
	}

	// For each formal, we need to construct a getarg quad
	size_t i = 1;
	for (SymOpd * opd : proc->getFormals()) {
		proc->addQuad(new GetArgQuad(i, opd));
		i++;
	}

	// recurse on all body stmts (add locals if needed)
	for (auto stmt : *myBody) {
		stmt->to3AC(proc);
	}

	// Leave -> handled by constructor
}

void FnDeclNode::to3AC(Procedure * proc){
	// pass - already handled above
}

void FormalDeclNode::to3AC(IRProgram * prog){
	// pass
}

void FormalDeclNode::to3AC(Procedure * proc){
	// create SymOpd and add to formals list
	proc->gatherFormal(myID->getSymbol());
}

Opd * IntLitNode::flatten(Procedure * proc){
	return new LitOpd(std::to_string(myNum), QUADWORD);
}

Opd * StrLitNode::flatten(Procedure * proc){
	Opd * res = proc->getProg()->makeString(myStr);
	return res;
}

Opd * CharLitNode::flatten(Procedure * proc){
	return new LitOpd(std::to_string(myVal), BYTE);
}

Opd * NullPtrNode::flatten(Procedure * proc){
	return new LitOpd("0", ADDR);
}

Opd * TrueNode::flatten(Procedure * prog){
	return new LitOpd("1", QUADWORD);
}

Opd * FalseNode::flatten(Procedure * prog){
	return new LitOpd("0", QUADWORD);
}

Opd * AssignExpNode::flatten(Procedure * proc){
	Opd * dstOpd = myDst->flatten(proc);
	Opd * srcOpd = mySrc->flatten(proc);
	proc->addQuad(new AssignQuad(dstOpd, srcOpd));
	return dstOpd;
}

Opd * LValNode::flatten(Procedure * proc){
	// abstract -- no implementation
	return nullptr;
}

Opd * DerefNode::flatten(Procedure * proc){
	TODO(Implement me)
}

Opd * RefNode::flatten(Procedure * proc){
	TODO(Implement me)
}

Opd * CallExpNode::flatten(Procedure * proc){
	// for each arg, flatten and then setarg
	size_t i = 1;
	for(auto arg : *myArgs) {
		Opd * argOpd = arg->flatten(proc);
		proc->addQuad(new SetArgQuad(i, argOpd));
		i++;
	}

	// call quad
	proc->addQuad(new CallQuad(myID->getSymbol()));

	return nullptr;
}

Opd * UnaryExpNode::flattenHelper(Procedure * proc, UnaryOp op) {
	Opd * expOpd = myExp->flatten(proc);
	Opd * tmpOpd = proc->makeTmp(expOpd->getWidth());
	proc->addQuad(new UnaryOpQuad(tmpOpd, op, expOpd));
	return tmpOpd;
}

Opd * NegNode::flatten(Procedure * proc){
	return flattenHelper(proc, NEG);
}

Opd * NotNode::flatten(Procedure * proc){
	return flattenHelper(proc, NOT);
}

Opd * BinaryExpNode::flattenHelper(Procedure * proc, BinOp op) {
	Opd * exp1Opd = myExp1->flatten(proc);
	Opd * exp2Opd = myExp2->flatten(proc);
	Opd * tmpOpd = proc->makeTmp(exp1Opd->getWidth());
	proc->addQuad(new BinOpQuad(tmpOpd, op, exp1Opd, exp2Opd));
	return tmpOpd;
}

Opd * PlusNode::flatten(Procedure * proc){
	return flattenHelper(proc, ADD);
}

Opd * MinusNode::flatten(Procedure * proc){
	return flattenHelper(proc, SUB);
}

Opd * TimesNode::flatten(Procedure * proc){
	return flattenHelper(proc, MULT);
}

Opd * DivideNode::flatten(Procedure * proc){
	return flattenHelper(proc, DIV);
}

Opd * AndNode::flatten(Procedure * proc){
	return flattenHelper(proc, AND);
}

Opd * OrNode::flatten(Procedure * proc){
	return flattenHelper(proc, OR);
}

Opd * EqualsNode::flatten(Procedure * proc){
	return flattenHelper(proc, EQ);
}

Opd * NotEqualsNode::flatten(Procedure * proc){
	return flattenHelper(proc, NEQ);
}

Opd * LessNode::flatten(Procedure * proc){
	return flattenHelper(proc, LT);
}

Opd * GreaterNode::flatten(Procedure * proc){
	return flattenHelper(proc, GT);
}

Opd * LessEqNode::flatten(Procedure * proc){
	return flattenHelper(proc, LTE);
}

Opd * GreaterEqNode::flatten(Procedure * proc){
	return flattenHelper(proc, GTE);
}

void AssignStmtNode::to3AC(Procedure * proc){
	myExp->flatten(proc);
}

void PostIncStmtNode::to3AC(Procedure * proc){
	Opd * dstOpd = myLVal->flatten(proc);
	Opd * litOpd = new LitOpd("1", QUADWORD);
	proc->addQuad(new BinOpQuad(dstOpd, ADD, dstOpd, litOpd));
}

void PostDecStmtNode::to3AC(Procedure * proc){
	Opd * dstOpd = myLVal->flatten(proc);
	Opd * litOpd = new LitOpd("1", QUADWORD);
	proc->addQuad(new BinOpQuad(dstOpd, SUB, dstOpd, litOpd));
}

void FromConsoleStmtNode::to3AC(Procedure * proc){
	Opd * dstOpd = myDst->flatten(proc);
	proc->addQuad(new IntrinsicQuad(INPUT, dstOpd));
}

void ToConsoleStmtNode::to3AC(Procedure * proc){
	Opd * srcOpd = mySrc->flatten(proc);
	proc->addQuad(new IntrinsicQuad(OUTPUT, srcOpd));
}

void IfStmtNode::to3AC(Procedure * proc){
	Label * exitIfLbl = proc->makeLabel();
	Opd * cndOpd = myCond->flatten(proc);
	proc->addQuad(new JmpIfQuad(cndOpd, exitIfLbl));
	for (auto stmt : *myBody) {
		stmt->to3AC(proc);
	}
	Quad * exitQuad = new NopQuad();
	exitQuad->addLabel(exitIfLbl);
	proc->addQuad(exitQuad);
}

void IfElseStmtNode::to3AC(Procedure * proc){
	Label * elseLbl = proc->makeLabel();
	Label * exitIfElseLbl = proc->makeLabel();
	Opd * cndOpd = myCond->flatten(proc);
	proc->addQuad(new JmpIfQuad(cndOpd, elseLbl));

	// true branch
	for (auto stmt : *myBodyTrue) {
		stmt->to3AC(proc);
	}
	// if true branch taken, skip false branch
	proc->addQuad(new JmpQuad(exitIfElseLbl));
	
	// false branch
	for (auto stmt : *myBodyFalse) {
		stmt->to3AC(proc);
	}

	// exit else branch
	Quad * exitQuad = new NopQuad();
	exitQuad->addLabel(exitIfElseLbl);
	proc->addQuad(exitQuad);
}

void WhileStmtNode::to3AC(Procedure * proc){
	Label * loopLbl = proc->makeLabel();
	Label * exitWhileLbl = proc->makeLabel();

	Opd * cndOpd = myCond->flatten(proc);
	Quad * cndQuad = new JmpIfQuad(cndOpd, exitWhileLbl);
	cndQuad->addLabel(loopLbl);
	proc->addQuad(cndQuad);

	for (auto stmt : *myBody) {
		stmt->to3AC(proc);
	}
	proc->addQuad(new JmpQuad(loopLbl));

	Quad * exitQuad = new NopQuad();
	exitQuad->addLabel(exitWhileLbl);
	proc->addQuad(exitQuad);
}

void CallStmtNode::to3AC(Procedure * proc){
	myCallExp->flatten(proc);
}

void ReturnStmtNode::to3AC(Procedure * proc){
	// set ret (if applicable)
	if (myExp != nullptr) {
		Opd * retOpd = myExp->flatten(proc);
		proc->addQuad(new SetRetQuad(retOpd));
	}
	// leave the function
	proc->addQuad(proc->getLeave());
}

void VarDeclNode::to3AC(Procedure * proc){
	SemSymbol * sym = ID()->getSymbol();
	if (sym == nullptr){
		throw new InternalError("null sym");
	}
	proc->gatherLocal(sym);
}

void VarDeclNode::to3AC(IRProgram * prog){
	SemSymbol * sym = ID()->getSymbol();
	if (sym == nullptr){
		throw new InternalError("null sym");
	}
	
	prog->gatherGlobal(sym);
}

//We only get to this node if we are in a stmt
// context (DeclNodes protect descent)
// Only encountered inside a local scope
// Since the variable should have already been declared,
// we simply need to look up the corresponding Opd *
Opd * IDNode::flatten(Procedure * proc){
	return proc->getSymOpd(mySymbol);
}


}
