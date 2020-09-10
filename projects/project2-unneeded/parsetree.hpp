#ifndef __PARSE_TREE_HPP
#define __PARSE_TREE_HPP

#include <list>
#include "tokens.hpp"

using TokenList = std::list<holyc::Token*>;
using StringList = std::list<std::string>;

namespace holyc {

class ParseTree {
    private:
        bool is_leaf;
        Token* leaf;
        std::list<holyc::ParseTree*>* subtrees;
    public:
        // 3 constructors
        // if the first one is invoked, then this is a leaf
        // if the second is invoked, this is a tree node
        ParseTree(Token* t);
        ParseTree(std::list<holyc::ParseTree*>* subs);
        ~ParseTree();

        // returns whether or not this class instance is a leaf
        bool isLeaf() const;

        // recursively flattens the parse tree left to right
        TokenList* unparse();
};

// fold over list and convert each token to its string form
StringList tokenStringList(TokenList* tokens);

}

#endif