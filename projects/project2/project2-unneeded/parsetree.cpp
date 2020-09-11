#include "parsetree.hpp"

using namespace holyc;
using TreeList = std::list<holyc::ParseTree*>;
using TokenList = std::list<holyc::Token*>;

ParseTree::ParseTree(Token* t) : is_leaf(true), leaf(t), subtrees(nullptr) {}

ParseTree::ParseTree(TreeList* subs) : is_leaf(false), leaf(nullptr), subtrees(subs){}

ParseTree::~ParseTree() {}

bool ParseTree::isLeaf() const { return is_leaf; }

TokenList * ParseTree::unparse() {
    TokenList * list = new std::list<Token *>();
    if(this->isLeaf()) {
        list->push_front(this->leaf);
    } else {
        for(ParseTree* subtree : *(this->subtrees)) {
            TokenList* sublist = subtree->unparse();
            for(unsigned int i = 0; i < sublist->size(); i++) {
                Token * t = sublist->back();
                list->push_front(t);
                sublist->pop_back();
            }
        }
    }
    return list;
}

StringList holyc::tokenStringList(TokenList* tokens) {
    StringList strlist;
    for(Token * t_ptr : *tokens) {
        strlist.push_back(t_ptr->toString());
    }
    return strlist;
}



