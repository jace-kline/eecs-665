#include "cfg_constants.hpp"

using namespace holeyc;

bool ConstantsAnalysis::runGraph(ControlFlowGraph * cfg){
	//This function should iterate over each block, much like the
	// dead code elimination pass. However, constant propagation
	// is a FORWARD analysis, so the inFacts will be made up
	// of the union of PREDECESSORS, rather than the union of SUCCESSORS
	bool changed = true;

	// remove unreachable blocks
	// unreachable == 0 edges with destination of that block
	// or blocks that can only be reached from other unreachable blocks
	cfg->removeUnreachableBlocks();

	// cut jmps to next block
	cfg->cutJmpToNext();

	while(changed) {
		changed = false;

		// initialize facts for each block
		for (BasicBlock * b : *cfg->getBlocks()) {
			ConstantsFacts init;
			inFacts[b] = init;
			outFacts[b] = init;
		}
		// iterate over each block
		// overwrite the known inFacts by merging predecessor fact sets
		for (BasicBlock * b : *cfg->getBlocks()) {
			ConstantsFacts facts;
			// merge all the predecessor facts, 2 at a time
			for(BasicBlock * pred : cfg->blockPredecessors(b)) {
				facts = ConstantsFacts::mergeFacts(facts, outFacts[pred]);
			}
			// set the inFacts of the current block
			// to the accumulated merge of the predecessor blocks' facts
			inFacts[b] = facts;

			// run the block optimizations with the updated 
			// input fact set -> see if changed
			bool blockchanged = runBlock(cfg, b);
			changed = changed || blockchanged;
		}
	}
	effectful = effectful || changed;
	return effectful;
}

bool ConstantsAnalysis::runBlock(ControlFlowGraph * cfg, BasicBlock * block){
	//This function should iterate over each quad in the block, 
	// much like the dead code elimination pass, except that this
	// is a forward analysis, so you should iterate over the block
	// from the first quad to the last, collecting the known constants

	// There are two things you should do for each quad: 
	// 1) Constant Folding
	// if all source operands are constants (i.e. can cast to LitOpd),
	// evaluate the result and replace the quad with a simple assignment
	// 2) Constant Propagation
	// if the source operand of an assignment is a constant, record
	// that fact and keep rolling
	// if the source operand is a variable (SymOpd or AuxOpd), but that
	// variable is known to be constant, replace
	bool changed = false;

	std::list<std::pair<Quad *, Quad *>> replacements;

	ConstantsFacts facts = inFacts[block];
	std::list<Quad *> * quads = block->getQuads();
	for(Quad * quad : *quads) {
		// check bin op quad for fold/propogation
		if (BinOpQuad * q = dynamic_cast<BinOpQuad *>(quad)) {
			// try to look up left operand
			ConstantVal * maybeval = facts.lookupVal(q->getSrc1());
			// if success, replace with constant val with corresponding lit opd
			// in the quad
			if(maybeval != nullptr) {
				q->setSrc1(maybeval->toLitOpd());
				changed = true;
			}
			// same thing with right operand
			maybeval = facts.lookupVal(q->getSrc2());
			if(maybeval != nullptr) {
				q->setSrc2(maybeval->toLitOpd());
				changed = true;
			}
			// try fold
			AssignQuad * aq = tryBinaryFold(q, facts);
			// if fold successful, replace the bin op quad with an assignment quad
			if(aq != nullptr) {
				replacements.push_back({quad, aq});
			} 
		}
		// check unary op quad for fold/propagation
		else if (UnaryOpQuad * q = dynamic_cast<UnaryOpQuad *>(quad)) {
			// try fold
			// try to look up operand
			ConstantVal * maybeval = facts.lookupVal(q->getSrc());
			// if success, replace with constant val with corresponding lit opd
			// in the quad
			if(maybeval != nullptr) {
				q->setSrc(maybeval->toLitOpd());
				changed = true;
			}

			AssignQuad * aq = tryUnaryFold(q, facts);
			if(aq != nullptr) { 
				replacements.push_back({quad, aq});
			}
		}
		// check assign quad for propagation opportunity
		else if (AssignQuad * q = dynamic_cast<AssignQuad *>(quad)) {
			// attempt to add fact (if src is literal operand)
			if(LitOpd * src = dynamic_cast<LitOpd *>(q->getSrc())) {
				const BasicType * dt = src->getType()->asBasic();
				// ensure the operand is a basic type
				if(dt != nullptr) {
					ConstantVal v;
					BaseType bt = dt->getBaseType();
					// produce a corresponding constant val
					switch(bt) {
						case INT: v = ConstantVal(src->intVal()); break;
						case BOOL: v = ConstantVal(src->boolVal()); break;
						case CHAR: v = ConstantVal(src->charVal()); break;
						default: break;
					}
					// add fact
					facts.gen(q->getDst(), v);
				}
			} else {
				// try to look up operand
				ConstantVal * maybeval = facts.lookupVal(q->getSrc());
				// if success, replace with constant val with corresponding lit opd
				// in the quad
				// Also, add (dst, val) to the facts map
				if(maybeval != nullptr) {
					q->setSrc(maybeval->toLitOpd());
					facts.gen(q->getDst(), *maybeval);
					changed = true;
				}
			}
		}
		// check toconsole quad for propagation opportunity
		else if (IntrinsicOutputQuad * q = dynamic_cast<IntrinsicOutputQuad *>(quad)) {
			// try to look up operand
			ConstantVal * maybeval = facts.lookupVal(q->getSrc());
			// if success, replace with constant val with corresponding lit opd
			// in the quad
			if(maybeval != nullptr) {
				q->setSrc(maybeval->toLitOpd());
				changed = true;
			}
		}
		// check jmpif quad
		else if (JmpIfQuad * q = dynamic_cast<JmpIfQuad *>(quad)) {
			// try to look up operand
			ConstantVal * maybeval = facts.lookupVal(q->getCnd());
			// if success, replace with constant val with corresponding lit opd
			// in the quad
			if(maybeval != nullptr) {
				q->setCnd(maybeval->toLitOpd());
				changed = true;
			}
		}
		// check setarg quad
		else if (SetArgQuad * q = dynamic_cast<SetArgQuad *>(quad)) {
			// try to look up operand
			ConstantVal * maybeval = facts.lookupVal(q->getSrc());
			// if success, replace with constant val with corresponding lit opd
			// in the quad
			if(maybeval != nullptr) {
				q->setSrc(maybeval->toLitOpd());
				changed = true;
			}
		}
		// check setret quad
		else if (SetRetQuad * q = dynamic_cast<SetRetQuad *>(quad)) {
			// try to look up operand
			ConstantVal * maybeval = facts.lookupVal(q->getSrc());
			// if success, replace with constant val with corresponding lit opd
			// in the quad
			if(maybeval != nullptr) {
				q->setSrc(maybeval->toLitOpd());
				changed = true;
			}
		}
	}
	// replace the quads that should be replaced
	for(auto pair : replacements) {
		Quad * old = pair.first;
		Quad * cur = pair.second;
		changed = true;
		cfg->replaceQuad(old, cur);
	}

	// update the fact set for successor blocks
	// if call quad, remove the global variable vals
	if(CallQuad * q = dynamic_cast<CallQuad *>(block->getTerminator())) {
		facts.killGlobalFacts(cfg);
	}
	outFacts[block] = facts;
	return changed;
}

AssignQuad * holeyc::tryBinaryFold(BinOpQuad * q, ConstantsFacts& facts) {
	Opd * src1 = q->getSrc1();
	Opd * src2 = q->getSrc2();
	Opd * dst = q->getDst();
	BinOp op = q->getOp();

	AssignQuad * aq = nullptr;

	// attempt to cast operands to LitOpds
	LitOpd * l = dynamic_cast<LitOpd *>(src1);
	LitOpd * r = dynamic_cast<LitOpd *>(src2);
	if(l != nullptr && r != nullptr) {
		LitOpd * fold = nullptr;
		if(op == ADD || op == SUB || op == DIV || op == MULT) {
			int valL = l->intVal();
			int valR = r->intVal();
			bool error = false;
			int foldval;
			switch(op) {
				case ADD: foldval = valL + valR; break;
				case SUB: foldval = valL - valR; break;
				case DIV: if(valR != 0) { foldval = valL / valR; } else { error = true; } break;
				case MULT: foldval = valL * valR; break;
				default: break;
			}
			if(!error) {
				fold = new LitOpd(std::to_string(foldval), BasicType::produce(INT));
				facts.gen(dst, ConstantVal(foldval)); 
			}
		} else if (op == OR || op == AND) {
			int valL = l->boolVal();
			int valR = r->boolVal();
			bool foldval;
			if(op == OR) { foldval = (valL || valR); }
			else { foldval = (valL && valR); }
			fold = new LitOpd(foldval ? "1" : "0", BasicType::produce(BOOL));
			facts.gen(dst, ConstantVal(foldval));
		} else if (op == LT || op == GT || op == LTE || op == GTE) {
			int valL = l->intVal();
			int valR = r->intVal();
			bool foldval;
			switch(op) {
				case LT: foldval = valL < valR; break;
				case GT: foldval = valL > valR; break;
				case LTE: foldval = valL <= valR; break;
				case GTE: foldval = valL >= valR; break;
				default: break;
			}
			fold = new LitOpd(foldval ? "1" : "0", BasicType::produce(BOOL));
			facts.gen(dst, ConstantVal(foldval));
		} else {
			bool eq = l->valString() == r->valString();
			bool foldval = op == EQ ? eq : !eq;
			fold = new LitOpd(foldval ? "1" : "0", BasicType::produce(BOOL));
			facts.gen(dst, ConstantVal(foldval));
		}
		if(fold != nullptr) { aq = new AssignQuad(dst, fold); }
	}
	return aq;
}

AssignQuad * holeyc::tryUnaryFold(UnaryOpQuad * q, ConstantsFacts& facts) {
	Opd * src = q->getSrc();
	Opd * dst = q->getDst();
	UnaryOp op = q->getOp();

	AssignQuad * aq = nullptr;
	if(LitOpd * opd = dynamic_cast<LitOpd *>(src)) {
		LitOpd * fold = nullptr;
		if(op == NOT) {
			bool foldval = !(opd->boolVal());
			fold = new LitOpd(foldval ? "1" : "0", BasicType::produce(BOOL));
			facts.gen(dst, ConstantVal(foldval));
		} else {
			int foldval = -1 * (opd->intVal());
			fold = new LitOpd(std::to_string(foldval), BasicType::produce(INT));
			facts.gen(dst, ConstantVal(foldval));
		}
		aq = new AssignQuad(dst, fold);
	}
	return aq;
}