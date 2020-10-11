#include "ast.hpp"
#include "symbol_table.hpp"
#include "errors.hpp"
#include "types.hpp"
#include "name_analysis.hpp"
#include "type_analysis.hpp"

namespace holeyc{

TypeAnalysis * TypeAnalysis::build(NameAnalysis * nameAnalysis){
	//To emphasize that type analysis depends on name analysis
	// being complete, a name analysis must be supplied for 
	// type analysis to be performed.
	TypeAnalysis * typeAnalysis = new TypeAnalysis();
	auto ast = nameAnalysis->ast;
	typeAnalysis->ast = ast;

	ast->typeAnalysis(typeAnalysis);
	if (typeAnalysis->hasError){
		return nullptr;
	}

	return typeAnalysis;

}

DataType * ASTNode::typeAnalysis(TypeAnalysis * ta){
	return nullptr;
}

DataType * ProgramNode::typeAnalysis(TypeAnalysis * ta){
	for(auto global : *myGlobals) {
		global->typeAnalysis(ta);
	}
	return nullptr;
}

DataType * IDNode::typeAnalysis(TypeAnalysis * ta){
	return mySymbol->toDType();
}

DataType * RefNode::typeAnalysis(TypeAnalysis * ta){
	DataType * t = myID->typeAnalysis(ta)->toDeref();
	if(t->isErrType()) {
		if(t->isFn()) ta->fnRef(this->line(), this->col());
		else if(t->isConcrete()) ta->badRef(this->line(), this->col());
	} 
	return t;
}

DataType * DerefNode::typeAnalysis(TypeAnalysis * ta){
	DataType * t = myID->typeAnalysis(ta)->toDeref();
	if(t->isErrType()) {
		if(t->isFn()) ta->fnRef(this->line(), this->col());
		else if(t->isConcrete()) ta->badDeref(this->line(), this->col());
	} 
	return t;
}

DataType * IndexNode::typeAnalysis(TypeAnalysis * ta){
	DataType * id_base_type = myBase->typeAnalysis(ta)->toDeref();
	if(id_base_type->isErrType()) {
		ta->badPtrBase(myBase->line(), myBase->col());
	}

	DataType * exp_type = myOffset->typeAnalysis(ta);
	if(!exp_type->isErrType() && (exp_type->getRetType() != INT)) {
		ta->badIndex(myOffset->line(), myOffset->col());
	}

	return id_base_type;
}

DataType * DeclNode::typeAnalysis(TypeAnalysis * ta){
	return nullptr;
}

DataType * FnDeclNode::typeAnalysis(TypeAnalysis * ta){
	Type retType = myID->typeAnalysis(ta)->getRetType();
	ta->setCurrentFnType(retType);
	for (auto stmt : *myBody) {
		stmt->typeAnalysis(ta);
	}
	return nullptr;
}

DataType * AssignStmtNode::typeAnalysis(TypeAnalysis * ta){
	myExp->typeAnalysis(ta);
	return nullptr;
}

DataType * FromConsoleStmtNode::typeAnalysis(TypeAnalysis * ta){
	DataType * dt = myDst->typeAnalysis(ta);
	if(dt->isFn()){
		ta->readFn(myDst->line(), myDst->col());
	} else if(isPtrType(dt->getRetType())) {
		ta->readPtr(myDst->line(), myDst->col());
	}
	return nullptr;
}

DataType * ToConsoleStmtNode::typeAnalysis(TypeAnalysis * ta){
	DataType * dt = mySrc->typeAnalysis(ta);
	if(dt->isFn()){
		ta->writeFn(mySrc->line(), mySrc->col());
	} else if(isPtrType(dt->getRetType())) {
		ta->readPtr(mySrc->line(), mySrc->col());
	}
	return nullptr;
}

DataType * PostDecStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myLVal->typeAnalysis(ta);
	if(!dt->isConcreteOf(INT) && !dt->isErrType()) {
		ta->badMathOpd(myLVal->line(), myLVal->col());
	}
	return nullptr;
}

DataType * PostIncStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myLVal->typeAnalysis(ta);
	if(!dt->isConcreteOf(INT) && !dt->isErrType()) {
		ta->badMathOpd(myLVal->line(), myLVal->col());
	}
	return nullptr;
}

DataType * IfStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * condType = myCond->typeAnalysis(ta);
	if(!condType->isConcreteOf(BOOL) && !condType->isErrType()) {
		ta->badIfCond(myCond->line(), myCond->col());
	}

	for(auto stmt : *myBody) {
		stmt->typeAnalysis(ta);
	}
	return nullptr;
}

DataType * IfElseStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * condType = myCond->typeAnalysis(ta);
	if(!condType->isConcreteOf(BOOL) && !condType->isErrType()) {
		ta->badIfCond(myCond->line(), myCond->col());
	}

	for(auto stmt : *myBodyTrue) {
		stmt->typeAnalysis(ta);
	}

	for(auto stmt : *myBodyFalse) {
		stmt->typeAnalysis(ta);
	}
	return nullptr;
}

DataType * WhileStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * condType = myCond->typeAnalysis(ta);
	if(!condType->isConcreteOf(BOOL) && !condType->isErrType()) {
		ta->badWhileCond(myCond->line(), myCond->col());
	}

	for(auto stmt : *myBody) {
		stmt->typeAnalysis(ta);
	}
	return nullptr;
}

DataType * ReturnStmtNode::typeAnalysis(TypeAnalysis * ta) {
	Type targetRetType = ta->getCurrentFnType();
	bool shouldReturnVoid = (targetRetType == VOID);
	if(myExp != nullptr) {
		if(shouldReturnVoid) {
			ta->extraRetValue(myExp->line(), myExp->col());
		} else {
			DataType * retType = myExp->typeAnalysis(ta);
			if(!retType->isErrType() && (retType->getRetType() != targetRetType)) {
				ta->badRetValue(myExp->line(), myExp->col());
			}
		}
	} else if(!shouldReturnVoid) {
		ta->badNoRet(line(), col());
	}
	return nullptr;
}

DataType * CallExpNode::typeAnalysis(TypeAnalysis * ta) {
	bool valid = true;
	DataType * id_type = myID->typeAnalysis(ta);
	if(id_type->isFn()) {
		FnType * id_fntype = dynamic_cast<FnType *>(id_type);
		std::list<Type> * required_arg_types = id_fntype->getArgTypes();
		size_t len = required_arg_types->size();
		if(myArgs->size() != len) {
			ta->badArgCount(myID->line(), myID->col());
			return new ErrType();
		}

		// for each arg, check argument type compatibility
		auto iter_required = required_arg_types->begin();
		auto iter_exps = myArgs->begin();
		for(size_t i = 0; i < len; i++) {
			Type reqtype = *iter_required;
			ExpNode * exp = *iter_exps;
			DataType * dt = exp->typeAnalysis(ta);
			if(dt->isErrType()) {
				valid = false;
			} else if (!dt->isConcreteOf(reqtype)) {
				ta->badArgMatch(exp->line(), exp->col());
				valid = false;
			}
			std::advance(iter_required, 1);
			std::advance(iter_exps, 1);
		}
	} else { // we know that an ID will not singlehandedly return error
		ta->badCallee(myID->line(), myID->col());
		valid = false;
	}
	if(valid) return new VarType(id_type->getRetType());
	else return new ErrType();
}

DataType * BinaryExpNode::typeAnalysisHelper(TypeAnalysis * ta, Type argtype, Type rettype) {
	bool valid = true;
	DataType * dt_left = myExp1->typeAnalysis(ta);
	DataType * dt_right = myExp1->typeAnalysis(ta);
	bool prev_errs = (dt_left->isErrType() || dt_right->isErrType());
	bool opds_same = (*dt_left == *dt_right);
	bool left_op_match = (dt_left->isConcreteOf(argtype));
	bool right_op_match = (dt_right->isConcreteOf(argtype));

	if(prev_errs) valid = false;
	else if(argtype == INT) {
		if(!opds_same && rettype == INT) {
			ta->badMathOpr(line(),col());
			valid = false;
		}
		if(!left_op_match) {
			if(rettype == INT) ta->badMathOpd(myExp1->line(), myExp1->col());
			else if(rettype == BOOL) ta->badRelOpd(myExp1->line(), myExp1->col());
			valid = false;
		}
		if(!right_op_match) {
			if(rettype == INT) ta->badMathOpd(myExp2->line(), myExp2->col());
			else if(rettype == BOOL) ta->badRelOpd(myExp2->line(), myExp2->col());
			valid = false;
		}
	} else if(argtype == BOOL && rettype == BOOL) {
		if(!opds_same) {
			// ta->badLogicOpr(line(),col());
			valid = false;
		}
		if(!left_op_match) {
			ta->badLogicOpd(myExp1->line(), myExp1->col());
			valid = false;
		}
		if(!right_op_match) {
			ta->badLogicOpd(myExp2->line(), myExp2->col());
			valid = false;
		}
	} else if(argtype == GENERIC) {
		if(!opds_same) {
			ta->badEqOpr(line(),col());
			valid = false;
		}
		if(!dt_left->isBase()) {
			ta->badEqOpd(myExp1->line(), myExp1->col());
			valid = false;
		}
		if(!dt_right->isBase()) {
			ta->badEqOpd(myExp2->line(), myExp2->col());
			valid = false;
		}
	}

	if(valid) return new VarType(rettype);
	else return new ErrType();
}

DataType * PlusNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, INT);
}

DataType * MinusNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, INT);
}

DataType * TimesNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, INT);
}

DataType * DivideNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, INT);
}

DataType * AndNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, BOOL, BOOL);
}

DataType * OrNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, BOOL, BOOL);
}

DataType * EqualsNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, GENERIC, BOOL);
}

DataType * NotEqualsNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, GENERIC, BOOL);
}

DataType * LessNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, BOOL);
}

DataType * LessEqNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, BOOL);
}

DataType * GreaterNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, BOOL);
}

DataType * GreaterEqNode::typeAnalysis(TypeAnalysis * ta) {
	return typeAnalysisHelper(ta, INT, BOOL);
}

DataType * NegNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myExp->typeAnalysis(ta);
	if(!dt->isErrType() && !dt->isConcreteOf(INT)) {
		ta->badMathOpd(myExp->line(), myExp->col());
		return new ErrType();
	}
	return dt;
}

DataType * NotNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myExp->typeAnalysis(ta);
	if(!dt->isErrType() && !dt->isConcreteOf(BOOL)) {
		ta->badLogicOpd(myExp->line(), myExp->col());
		return new ErrType();
	}
	return dt;
}

DataType * AssignExpNode::typeAnalysis(TypeAnalysis * ta){
	bool valid = true;
	DataType * lhs = myDst->typeAnalysis(ta);
	DataType * rhs = mySrc->typeAnalysis(ta);
	// check for previous errors
	if(lhs->isErrType() || rhs->isErrType()) {
		valid = false;
	}
	// check lhs and rhs are compatible
	if(!(*lhs == *rhs) && !(lhs->isPtr() && rhs->getRetType() == GENERICPTR)) {
		ta->badAssignOpr(line(),col());
		valid = false;
	}
	// check lhs is concrete (valid operand)
	if(!lhs->isErrType() && !lhs->isConcrete()) {
		ta->badAssignOpd(myDst->line(), myDst->col());
		valid = false;
	}
	// check rhs is concrete (valid operand)
	if(!rhs->isErrType() && !rhs->isConcrete()) {
		ta->badAssignOpd(mySrc->line(), mySrc->col());
		valid = false;
	}

	if(valid) return lhs;
	else return new ErrType();
}

DataType * IntLitNode::typeAnalysis(TypeAnalysis * ta) {
	return new VarType(INT);
}

DataType * StrLitNode::typeAnalysis(TypeAnalysis * ta) {
	return new VarType(CHARPTR);
}

DataType * CharLitNode::typeAnalysis(TypeAnalysis * ta) {
	return new VarType(CHAR);
}

DataType * NullPtrNode::typeAnalysis(TypeAnalysis * ta) {
	return new VarType(GENERICPTR);
}

DataType * TrueNode::typeAnalysis(TypeAnalysis * ta) {
	return new VarType(BOOL);
}

DataType * FalseNode::typeAnalysis(TypeAnalysis * ta) {
	return new VarType(BOOL);
}

DataType * CallStmtNode::typeAnalysis(TypeAnalysis * ta) {
	myCallExp->typeAnalysis(ta);
	return nullptr;
}

}
