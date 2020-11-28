#include "cfg_constants.hpp"

using namespace holeyc;

bool ConstantsAnalysis::runGraph(ControlFlowGraph * cfg){
	//This function should iterate over each block, much like the
	// dead code elimination pass. However, constant propagation
	// is a FORWARD analysis, so the inFacts will be made up
	// of the union of PREDECESSORS, rather than the union of SUCCESSORS
	TODO(implement me!)
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
	// variable is known to be constant, replace the 

	TODO(implement me!)
}
