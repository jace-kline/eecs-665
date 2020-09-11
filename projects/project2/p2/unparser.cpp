#include "unparser.hpp"

UnparseNode::UnparseNode(holyc::Token& token) {
    this->str = token.toString();
}

UnparseNode::UnparseNode(std::string s) {
    this->str = s;
}

UnparseNode::UnparseNode(const UnparseNode& other) {
    this->str = other.str;
}

UnparseNode UnparseNode::operator+(const UnparseNode& other) {
    return(UnparseNode(this->str + "\n" + other.str));
}

void writeUnparsed(UnparseNode* node, std::ostream& os) {
    os << node->str << std::endl;
}


