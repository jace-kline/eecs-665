#include "3ac.hpp"
#include "cfg.hpp"
#include <set>
#include <algorithm>
#include <unordered_map>

//#include "cfg_dce.hpp"
#include "cfg_constants.hpp"

using namespace holeyc;
using namespace std;

ControlFlowGraph::ControlFlowGraph(CFGFactory * b){
	proc = b->proc;
	exit = b->exitBlock;
	entry = b->entryBlock;
	blocks = b->blocks;
	edges = b->edges;

	assert(exit != nullptr);
	assert(entry != nullptr);
}

void ControlFlowGraph::setEntryBlock(BasicBlock * block){
	assert(entry == nullptr);
	entry = block;
}

BasicBlock * ControlFlowGraph::getEntryBlock(){
	return entry;
}

void ControlFlowGraph::setExitBlock(BasicBlock * block){
	assert(exit == nullptr);
	exit = block;
}

BasicBlock * ControlFlowGraph::getExitBlock(){
	return exit;
}

void ControlFlowGraph::removeBlock(BasicBlock * block){
	// for the rest of the elements in the blocks list,
	// decrement the block number if the number is greater
	// than the deleted one
	for(BasicBlock * b : *blocks) {
		if(block->getNum() < b->getNum()) { b->decNum(); }
	}

	// remove the block from the blocks list
	blocks->remove(block);

	// remove the block's quads
	for (Quad * quad : *block->getQuads()){
		proc->getQuads()->remove(quad);
	}

	// remove the edges that connect to that block
	auto edgeItr = edges->begin();
	while  (edgeItr != edges->end()){
		auto edge = *edgeItr;
		if (edge->src == block){
			edgeItr = edges->erase(edgeItr);
		} else if (edge->tgt == block){
			edges->remove(edge);
		} else {
			edgeItr++;
		}
	}
}

BasicBlock * ControlFlowGraph::getBlock(Quad * quad){
	for (BasicBlock * block : *blocks){
		std::list<Quad *> * quads = block->getQuads();
		auto qItr = std::find(quads->begin(), quads->end(), quad);
		if (qItr != quads->end()){
			return block;
		}
	}
	return nullptr;
}

std::list<BasicBlock *> * ControlFlowGraph::getBlocks(){ return blocks; }

bool ControlFlowGraph::removeQuad(Quad * quad){
	std::list<Quad *> * allQuads = proc->getQuads();

	BasicBlock * block = getBlock(quad);
	std::list<Quad *> * blockQuads = block->getQuads();

	if (blockQuads->size() == 1){
		return false;
	}

	allQuads->remove(quad);
	blockQuads->remove(quad);
	if (block->getLeader() == quad){
		Label * lbl = quad->getLabel();

		Quad * front = blockQuads->front();
		front->clearLabels();
		front->addLabel(lbl);
		block->setLeader(front);
	}
	if (block->getTerminator() == quad){
		block->setTerminator(blockQuads->back());
	}
	return true;
}

void ControlFlowGraph::replaceWithNop(Quad * quad){
	NopQuad * nop = new NopQuad();

	{
	BasicBlock * block = getBlock(quad);
	std::list<Quad *> * blockQuads = block->getQuads();
	auto pos = std::find(blockQuads->begin(), blockQuads->end(), quad);
	blockQuads->insert(pos, nop);
	blockQuads->erase(pos);

	if (block->getLeader() == quad){
		block->setLeader(nop);
		if (Label * label = quad->getLabel()){
			nop->addLabel(label);
		}
	}
	if (block->getTerminator() == quad){
		block->setTerminator(nop);
	}
	}

	{
	std::list<Quad *> * procQuads = proc->getQuads();
	auto pos = std::find(procQuads->begin(), procQuads->end(), quad);
	procQuads->insert(pos, nop);
	procQuads->erase(pos);
	}
}

void ControlFlowGraph::replaceQuad(Quad * old, Quad * cur){
	{
	BasicBlock * block = getBlock(old);
	std::list<Quad *> * blockQuads = block->getQuads();
	auto pos = std::find(blockQuads->begin(), blockQuads->end(), old);
	blockQuads->insert(pos, cur);
	blockQuads->erase(pos);

	if (block->getLeader() == old){
		block->setLeader(cur);
		if (Label * label = old->getLabel()){
			cur->addLabel(label);
		}
	}
	if (block->getTerminator() == old){
		block->setTerminator(cur);
	}
	}

	{
	std::list<Quad *> * procQuads = proc->getQuads();
	auto pos = std::find(procQuads->begin(), procQuads->end(), old);
	procQuads->insert(pos, cur);
	procQuads->erase(pos);
	}
}

void ControlFlowGraph::removeUnreachableBlocks() {
	std::set<BasicBlock *> unreachable;
	std::map<BasicBlock *, std::list<BasicBlock *> *> targets;

	// map each block to those that have an edge to it
	for (CFGEdge * edge : *edges) {
		auto it = targets.find(edge->tgt);
		if(it == targets.end()) {
			std::list<BasicBlock *> * list = new std::list<BasicBlock *>();
			list->push_back(edge->src);
			targets[edge->tgt] = list;
		} else {
			it->second->push_back(edge->src);
		}
	}

	// find the initially unreachable blocks
	// exclude the first block
	for (BasicBlock * block : *blocks) {
		if(block != blocks->front()) {
			auto it = targets.find(block);
			if(it == targets.end()) {
				unreachable.insert(block);
			}
		}
	}

	// loop till fixed point
	// if a block can only be reached by other
	// unreachable blocks, then it is set to unreachable
	size_t pre_size = 0;
	size_t post_size = 1;
	while(post_size != pre_size) {
		pre_size = unreachable.size();
		for(auto pair : targets) {
			bool keep = false;
			for(auto src : *pair.second) {
				auto it = unreachable.find(src);
				if(it == unreachable.end()) {
					keep = true;
					break;
				}
			}
			if(!keep) {
				unreachable.insert(pair.first);
				targets.erase(pair.first);
			}
		}
		post_size = unreachable.size();
	}

	// remove all unreachable blocks
	for(BasicBlock * block : unreachable) {
		removeBlock(block);
	}
}

void ControlFlowGraph::cutJmpToNext() {
	for(CFGEdge * edge : *edges) {
		int n = edge->src->getNum();
		int m = edge->tgt->getNum();
		CFGEdgeType t = edge->type;
		// if there is a jump edge to the next block,
		// remove the terminator jump quad from that block
		// and change the edge type from jump to fall
		if(m == n + 1 && t == JUMP) {
			removeQuad(edge->src->getTerminator());
			edge->type = FALL;
		}
	}
}

std::string ControlFlowGraph::getProcName(){
	return proc->getName();
}

void ControlFlowGraph::optimize(){
	// The dead code elimination pass is left in the codebase
	// for your reference, but you should not run it as part
	// of your project
	// bool dceEffect = DeadCodeElimination::run(this);

	// TODO: implement this code
	bool constantEffect = ConstantsAnalysis::run(this);
}

std::set<BasicBlock *> ControlFlowGraph::blockSuccessors(BasicBlock * block){
	std::set<BasicBlock *> res;
	for (auto edge : *edges){
		if (edge->src == block){
			res.insert(edge->tgt);
		}
	}
	return res;
}

std::set<BasicBlock *> ControlFlowGraph::blockPredecessors(BasicBlock * block){
	std::set<BasicBlock *> res;
	for (auto edge : *edges){
		if (edge->tgt == block){
			res.insert(edge->src);
		}
	}
	return res;
}

static void replaceAllSubstrs(std::string& str, std::string from, std::string to){
	size_t pos = str.find(from);
	while( pos != std::string::npos){
		str.replace(pos, from.size(), to);
		pos = str.find(from, pos + to.size());
	}
}

static void dottifyString(std::string& str){
	replaceAllSubstrs(str, "\"", "\\\"");
	replaceAllSubstrs(str, "[", "\\[");
	replaceAllSubstrs(str, "]", "\\]");
	replaceAllSubstrs(str, "\n", "\\n");
}

void ControlFlowGraph::toDot(std::ostream& out){
	out << "digraph G {\n";
	for (auto block : *blocks){
		out << "blk" << block->getNum() << " [";
		std::string res = block->toString();
		dottifyString(res);
		out << "shape=record ";
		out << "label=\"" << res << "\"";
		out << "]\n";
	}
	for (auto edge : *edges){
		std::string srcNum = std::to_string(edge->src->getNum());
		std::string tgtNum = std::to_string(edge->tgt->getNum());
		std::string edgeLbl = "";
		switch (edge->type){
			case FALL:
				edgeLbl = "FALL";
				break;
			case JUMP:
				edgeLbl = "JUMP";
				break;
			case LINK:
				edgeLbl = "LINK";
				break;
		}
		if (edge->type == FALL){
			edgeLbl = "FALL";
		} else {
		}
		out << "blk" << srcNum << " -> " << "blk" << tgtNum 
		    << "[label=" << edgeLbl << "]" << "\n";
	}

	out << "}\n";
	out << std::flush;
}
