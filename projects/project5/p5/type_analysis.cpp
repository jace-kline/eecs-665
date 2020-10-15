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
	if(t->isErr()) {
		if(t->isFn()) ta->fnRef(this->line(), this->col());
		else if(t->isConcrete()) ta->badRef(this->line(), this->col());
	} 
	return t;
}

DataType * DerefNode::typeAnalysis(TypeAnalysis * ta){
	DataType * t = myID->typeAnalysis(ta)->toDeref();
	if(t->isErr()) {
		if(t->isFn()) ta->fnRef(this->line(), this->col());
		else if(t->isConcrete()) ta->badDeref(this->line(), this->col());
	} 
	return t;
}

DataType * IndexNode::typeAnalysis(TypeAnalysis * ta){
	DataType * id_base_type = myBase->typeAnalysis(ta)->toDeref();
	if(id_base_type->isErr()) {
		ta->badPtrBase(myBase->line(), myBase->col());
	}

	DataType * exp_type = myOffset->typeAnalysis(ta);
	if(!exp_type->isErr() && (exp_type->getRetType() != INT)) {
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
	} else if(dt->getRetType() == VOID) {
		ta->badWriteVoid(mySrc->line(), mySrc->col());
	}
	return nullptr;
}

DataType * PostDecStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myLVal->typeAnalysis(ta);
	if(!dt->isConcreteOf(INT) && !dt->isErr()) {
		ta->badMathOpd(myLVal->line(), myLVal->col());
	}
	return nullptr;
}

DataType * PostIncStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myLVal->typeAnalysis(ta);
	if(!dt->isConcreteOf(INT) && !dt->isErr()) {
		ta->badMathOpd(myLVal->line(), myLVal->col());
	}
	return nullptr;
}

DataType * IfStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * condType = myCond->typeAnalysis(ta);
	if(!condType->isConcreteOf(BOOL) && !condType->isErr()) {
		ta->badIfCond(myCond->line(), myCond->col());
	}

	for(auto stmt : *myBody) {
		stmt->typeAnalysis(ta);
	}
	return nullptr;
}

DataType * IfElseStmtNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * condType = myCond->typeAnalysis(ta);
	if(!condType->isConcreteOf(BOOL) && !condType->isErr()) {
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
	if(!condType->isConcreteOf(BOOL) && !condType->isErr()) {
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
			if(!retType->isErr() && (retType->getRetType() != targetRetType)) {
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
	bool isfunc = false;
	DataType * id_type = myID->typeAnalysis(ta);
	if(id_type->isFn()) {
		isfunc = true;
		FnType * id_fntype = dynamic_cast<FnType *>(id_type);
		std::list<Type> * required_arg_types = id_fntype->getArgTypes();
		size_t len = required_arg_types->size();
		if(myArgs->size() != len) {
			ta->badArgCount(myID->line(), myID->col());
			return new ErrType(id_type->getRetType());
		}

		// for each arg, check argument type compatibility
		auto iter_required = required_arg_types->begin();
		auto iter_exps = myArgs->begin();
		for(size_t i = 0; i < len; i++) {
			Type reqtype = *iter_required;
			ExpNode * exp = *iter_exps;
			DataType * dt = exp->typeAnalysis(ta);
			if(dt->isErr()) {
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
	else if (isfunc) return new ErrType(id_type->getRetType());
	else return new ErrType(ERRTYPE);
}

DataType * BinaryExpNode::typeAnalysisHelper(TypeAnalysis * ta, Type argtype, Type rettype) {
	bool valid = true;
	DataType * dt_left = myExp1->typeAnalysis(ta);
	DataType * dt_right = myExp2->typeAnalysis(ta);
	bool err_left = dt_left->isErr();
	bool err_right = dt_right->isErr();
	bool errtype_left = dt_left->isErrType();
	bool errtype_right = dt_right->isErrType();
	bool prev_errs = (err_left || err_right);
	bool opds_same = (*dt_left == *dt_right);
	bool left_op_match = (dt_left->isConcreteOf(argtype));
	bool right_op_match = (dt_right->isConcreteOf(argtype));
	bool left_concrete = dt_left->isConcrete();
	bool right_concrete = dt_right->isConcrete();

	if(prev_errs) valid = false;

	// Case of binary numerical operator
	// + , - , * , / , < , <= , > , >=
	if(argtype == INT) {
		// if(!opds_same && rettype == INT) {
		// 	ta->badMathOpr(line(),col());
		// 	valid = false;
		// }
		if(!left_op_match && !errtype_left) {
			// math operator
			if(rettype == INT) ta->badMathOpd(myExp1->line(), myExp1->col());
			// relational operator
			else if(rettype == BOOL) ta->badRelOpd(myExp1->line(), myExp1->col());
			valid = false;
		}
		if(!right_op_match && !errtype_right) {
			// math operator
			if(rettype == INT) ta->badMathOpd(myExp2->line(), myExp2->col());
			// relational operator
			else if(rettype == BOOL) ta->badRelOpd(myExp2->line(), myExp2->col());
			valid = false;
		}
	} 
	// Case of boolean operator
	// && , ||
	else if(argtype == BOOL && rettype == BOOL) {
		// if(!opds_same) {
		// 	ta->badLogicOpr(line(),col());
		// 	valid = false;
		// }
		if(!left_op_match && !errtype_left) {
			ta->badLogicOpd(myExp1->line(), myExp1->col());
			valid = false;
		}
		if(!right_op_match && !errtype_right) {
			ta->badLogicOpd(myExp2->line(), myExp2->col());
			valid = false;
		}
	}
	// Case of equality comparison operator
	// == , !=
	// both arguments must have the same type
	else if(argtype == GENERIC) {
		if(!left_concrete && !err_left) {
			ta->badEqOpd(myExp1->line(), myExp1->col());
			valid = false;
		}
		if(!right_concrete && !err_right) {
			ta->badEqOpd(myExp2->line(), myExp2->col());
			valid = false;
		}
		if(!opds_same && valid) {
			ta->badEqOpr(line(),col());
			valid = false;
		}
	}

	if(valid) return new VarType(rettype);
	else return new ErrType(ERRTYPE);
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
	if(!dt->isErr() && !dt->isConcreteOf(INT)) {
		ta->badMathOpd(myExp->line(), myExp->col());
		return new ErrType(ERRTYPE);
	}
	return dt;
}

DataType * NotNode::typeAnalysis(TypeAnalysis * ta) {
	DataType * dt = myExp->typeAnalysis(ta);
	if(!dt->isErr() && !dt->isConcreteOf(BOOL)) {
		ta->badLogicOpd(myExp->line(), myExp->col());
		return new ErrType(ERRTYPE);
	}
	return dt;
}

DataType * AssignExpNode::typeAnalysis(TypeAnalysis * ta){
	bool valid = true;
	DataType * lhs = myDst->typeAnalysis(ta);
	DataType * rhs = mySrc->typeAnalysis(ta);
	// check for previous errors
	if(lhs->isErr() || rhs->isErr()) {
		valid = false;
	}
	// check lhs is concrete (valid operand)
	if(!lhs->isErr() && !lhs->isConcrete()) {
		ta->badAssignOpd(myDst->line(), myDst->col());
		valid = false;
	}
	// check rhs is concrete (valid operand)
	if(!rhs->isErr() && !rhs->isConcrete()) {
		ta->badAssignOpd(mySrc->line(), mySrc->col());
		valid = false;
	}
	// check lhs and rhs are compatible
	// only check if we are valid to this point
	if(!(*lhs == *rhs) && valid) {
		ta->badAssignOpr(line(),col());
		valid = false;
	}

	if(valid) return lhs;
	else return new ErrType(ERRTYPE);
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
