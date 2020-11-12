#include <ostream>
#include "3ac.hpp"
#include "err.hpp"

namespace holeyc{

void IRProgram::allocGlobals(){
	// iterate over globals
	for (auto pair : globals) {
		SymOpd * opd = pair.second;
		std::string s = "gbl_" + opd->getSym()->getName();
		opd->setMemoryLoc(s);
		opd->setIsGlobal();
	}

	// iterate over strings
	int i = 0;
	for (auto pair : strings) {
		AuxOpd * opd = pair.first;
		opd->setMemoryLoc("str_" + std::to_string(i));
		opd->setIsString();
		i++;
	}
}

void IRProgram::datagenX64(std::ostream& out){
	out << ".data\n";
	for (auto pair : globals) {
		SymOpd * opd = pair.second;
		out << opd->getMemoryLoc()
			<< ": " 
			<< (opd->getWidth() == BYTE ? ".byte" : ".quad")
			<< " 0\n";
	}
	
	for (auto pair : strings) {
		out << pair.first->getMemoryLoc()
			<< ": .asciz "
			<< pair.second
			<< "\n.align 8\n";
	}	
}

void IRProgram::toX64(std::ostream& out){
	allocGlobals();
	datagenX64(out);
	out << "\n.globl main\n.text\n\n";
	for (Procedure * proc : procs) {
		proc->toX64(out);
		out << "\n";
	}
}

void Procedure::allocLocals(){
	int n = formals.size();
	int m = locals.size();
	int t = temps.size();
	int offset;

	// iterate over formals
	int i = 1;
	for (SymOpd * opd : formals) {
		offset = (i <= 6) ? (-16 - (8 * i)) : (8 * (n - i));
		opd->setMemoryLoc(std::to_string(offset) + "(%rbp)");
		i++;
	}

	// we only subtract up to 6 args in the AR
	int n_ = n < 7 ? n : 6;

	// iterate over locals
	i = 1;
	for (auto pair : locals) {
		offset = -16 - (8 * n_) - (8 * i);
		pair.second->setMemoryLoc(std::to_string(offset) + "(%rbp)");
		i++;
	}

	// iterate over temps
	i = 1;
	for (AuxOpd * opd : temps) {
		offset = -16 - (8 * n_) - (8 * m) - (8 * i);
		opd->setMemoryLoc(std::to_string(offset) + "(%rbp)");
		i++;
	}

	// set procedure total storage offset
	// 16-bit align
	int align = ((8 * n) + (8 * m) + (8 * t)) % 16;
	allocBytes = align + (8 * n_) + (8 * m) + (8 * t);
}

void Procedure::toX64(std::ostream& out){
	// Allocate all locals
	// i.e. assign each local opd an offset from %rbp
	allocLocals();

	std::string bytes = std::to_string(allocBytes);

	enter->codegenLabels(out);
	enter->codegenX64(out);

	// function body
	for (auto quad : *bodyQuads){
		quad->codegenLabels(out);
		quad->codegenX64(out);
	}

	leave->codegenLabels(out);
	leave->codegenX64(out);
}

void Quad::codegenLabels(std::ostream& out){
	if (labels.empty()){ return; }

	size_t numLabels = labels.size();
	size_t labelIdx = 0;
	for ( Label * label : labels){
		out << label->toString() << ": ";
		if (labelIdx != numLabels - 1){ out << "\n"; }
		labelIdx++;
	}
}

void BinOpQuad::codegenX64(std::ostream& out){
	src1->genLoad(out, "%rax");
	src2->genLoad(out, "%rbx");
	if(op == MULT || op == DIV) {
		// rax (* | /) rbx
		// result stored in rdx:rax
		// for mult, we assume that overflow to rdx does not occur
		// for div, we assume only integer quotient (no remainder)
		out << binOpToX64(op) << " %rbx\n";
	} else if(isCmpOp(op)) {
		out << "cmpq %rbx, %rax\n"
			<< binOpToX64(op) << " " << quadRegByte0("%rax") << "\n";
	} else {
		out << binOpToX64(op) << " %rbx, %rax\n";
	}
	dst->genStore(out, "%rax");
}

void UnaryOpQuad::codegenX64(std::ostream& out){
	src->genLoad(out, "%rax");
	if(op == NOT) {
		out << "cmpq $0, %rax\n"
			<< "setz %al\n";
	} else if (op == NEG) {
		out << "negq %rax\n";
	}
	dst->genStore(out, "%rax");
}

void AssignQuad::codegenX64(std::ostream& out){
	src->genLoad(out, "%rax");
	dst->genStore(out, "%rax");

	// if the destination operand dereferences another,
	// we must assign back to that as well
	// Opd * derefOpd = dst->getDerefOpd();
	// if(derefOpd != nullptr) {
	// 	derefOpd->genLoad(out, "%rbx");
	// 	out << "movq %rax, (%rbx)\n";		
	// }

}

void LocQuad::codegenX64(std::ostream& out){
	// (Optional)
	if (srcLoc) { // reference
		out << "leaq " << src->opdX64Repr() << ", %rax\n";
		tgt->genStore(out, "%rax");
	} else { // dereference
		out << "movq " << src->opdX64Repr() << ", %rax\n";
		if(tgt->getWidth() == BYTE) {
			// clear the entire %rbx register to 0
			// then move one byte from address into lowest byte of %rbx
			out << "movq $0, %rbx\n"
				<< "movb (%rax), " << quadRegByte0("%rbx") << "\n";
		} else {
			out << "movq (%rax), %rbx\n";
		}
		// keep track of the opd that we are dereferencing
		tgt->setDeref(src);

		tgt->genStore(out, "%rbx");
	}
}

void JmpQuad::codegenX64(std::ostream& out){
	out << "jmp " << tgt->toString() << "\n";
}

void JmpIfQuad::codegenX64(std::ostream& out){
	out << "movq $0, %rax\n";
	cnd->genLoad(out, "%rax");
	out << "cmp $0, %rax\n";
	out << "je " << tgt->toString() << "\n";
}

void NopQuad::codegenX64(std::ostream& out){
	out << "nop\n";
}

void IntrinsicQuad::codegenX64(std::ostream& out){
	switch(myIntrinsic){
	case OUTPUT:
		myArg->genLoad(out, "%rdi");
		if (myArg->getWidth() == QUADWORD){
			out << "callq printInt\n";
		} else if (myArg->getWidth() == BYTE){
			const DataType * dt = myArg->getDataType();
			if(dt->isBool()) {
				out << "callq printBool\n";
			} else if(dt->isChar()) {
				out << "callq printChar\n";
			}
		} else {
			//If the argument is an ADDR,
			// assume it's a string
			out << "callq printString\n";
		}
		break;
	case INPUT:
		// store input val to %rax (via return)
		if (myArg->getWidth() == QUADWORD){
			out << "callq getInt\n";
		} else if (myArg->getWidth() == BYTE){
			// Must differentiate between char and bool
			const DataType * dt = myArg->getDataType();
			if(dt->isBool()) {
				out << "callq getBool\n";
			} else if(dt->isChar()) {
				out << "callq getChar\n";
			}
		} else {
			throw new InternalError("Attempted to read into a pointer");
		}

		// store value from %rax into destination
		myArg->genStore(out, "%rax");
	}
}

void CallQuad::codegenX64(std::ostream& out){
	out << "call " << callee->toString() << "\n";

	// get callee's number of args
	int args = callee->getDataType()->asFn()->getFormalTypes()->size();

	// if more than 6 args, must add back to stack pointer
	// for args passed on the stack
	if(args > 6) {
		int addback = 8 * (args - 6);
		out << "addq $" << addback << ", %rsp\n";
	}
}

void EnterQuad::codegenX64(std::ostream& out){
	// Procedure prologue
	out << "pushq %rbp\n"
		<< "movq %rsp, %rbp\n"
		<< "addq $16, %rbp\n" 
		<< "subq $" << myProc->getAllocBytes() << ", %rsp\n";
}

void LeaveQuad::codegenX64(std::ostream& out){
	// Procedure epilogue
	out << "addq $" << myProc->getAllocBytes() << ", %rsp\n"
		<< "popq %rbp\n"
		<< "ret\n";
}

std::string indexToReg(size_t index) {
	switch (index) {
		case 1: return "%rdi";
		case 2: return "%rsi";
		case 3: return "%rdx";
		case 4: return "%rcx";
		case 5: return "%r8";
		case 6: return "%r9";
		default: break;
	}
	throw new InternalError("Index out of range of target registers");
}

void SetArgQuad::codegenX64(std::ostream& out){
	if(index <= 6) {
		opd->genLoad(out, indexToReg(index));
	} else {
		//
		opd->genLoad(out, "%rax");
		// push arg to stack
		out << "pushq %rax\n";
	}
}

void GetArgQuad::codegenX64(std::ostream& out){
	// If arg is passed in register, copy value to
	// proper stack location
	if(index <= 6) {
		opd->genStore(out, indexToReg(index));
	}
}

void SetRetQuad::codegenX64(std::ostream& out){
	opd->genLoad(out, "%rax");
}

void GetRetQuad::codegenX64(std::ostream& out){
	opd->genStore(out, "%rax");
}

void SymOpd::genLoad(std::ostream & out, std::string regStr){
	std::string mov = getWidth() == BYTE ? "movzbq " : "movq ";
	out << mov << opdX64Repr() << ", " << regStr << "\n";
}

void SymOpd::genStore(std::ostream& out, std::string regStr){
	bool byte = getWidth() == BYTE;
	std::string mov = byte ? "movb " : "movq ";
	std::string reg = byte ? quadRegByte0(regStr) : regStr;
	out << mov << reg << ", " << opdX64Repr() << "\n";

	Opd * dOpd = getDerefOpd();
	if(dOpd != nullptr) {
		dOpd->genLoad(out, "%r8");
		std::string regDeref = byte ? quadRegByte0(regStr) : regStr;
		out << mov << " " << regDeref << ", (%r8)\n";
	}
}

void AuxOpd::genLoad(std::ostream & out, std::string regStr){
	std::string mov = getWidth() == BYTE ? "movzbq " : (getIsString() ? "leaq " : "movq ");
	out << mov << opdX64Repr() << ", " << regStr << "\n";
}

void AuxOpd::genStore(std::ostream& out, std::string regStr){
	bool byte = getWidth() == BYTE;
	std::string mov = byte ? "movb " : "movq ";
	std::string reg = byte ? quadRegByte0(regStr) : regStr;
	out << mov << reg << ", " << opdX64Repr() << "\n";

	Opd * dOpd = getDerefOpd();
	if(dOpd != nullptr) {
		dOpd->genLoad(out, "%r8");
		std::string regDeref = byte ? quadRegByte0(regStr) : regStr;
		out << mov << " " << regDeref << ", (%r8)\n";
	}
}

void LitOpd::genLoad(std::ostream & out, std::string regStr){
	out << "movq $" << val << ", " << regStr << "\n"; 
}

void LitOpd::genStore(std::ostream& out, std::string regStr){
	throw new InternalError("Cannot use literal as l-val");
}

bool isCmpOp(BinOp op) {
	return (op >= EQ);
}

std::string quadRegByte0(std::string reg) {
	if(reg == "%rax") return "%al";
	else if(reg == "%rcx") return "%cl";
	else if(reg == "%rdx") return "%dl";
	else if(reg == "%rbx") return "%bl";
	else if(reg == "%rsi") return "%sil";
	else if(reg == "%rdi") return "%dil";
	else if(reg == "%rsp") return "%spl";
	else if(reg == "%rbp") return "%bpl";
	else if(reg == "%r8") return "%r8b";
	else if(reg == "%r9") return "%r9b";
	else if(reg == "%r10") return "%r10b";
	else if(reg == "%r11") return "%r11b";
	else if(reg == "%r12") return "%r12b";
	else if(reg == "%r13") return "%r13b";
	else if(reg == "%r14") return "%r14b";
	else if(reg == "%r15") return "%r15b";
	throw new InternalError("Invalid register");
}

std::string binOpToX64(BinOp op) {
	switch(op) {
		case ADD: return "addq";
		case SUB: return "subq";
		case DIV: return "idivq";
		case MULT: return "imulq";
		case OR: return "orq";
		case AND: return "andq";
		case EQ: return "sete";
		case NEQ: return "setne";
		case LT: return "setl";
		case GT: return "setg";
		case LTE: return "setle";
		default: return "setge";
	}
	throw new InternalError("Invalid op");
}

}
