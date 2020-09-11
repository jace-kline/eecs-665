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

void writeUnparsed(UnparseNode& node, std::ostream& os) {
    os << removeBlankLines(node.str);
    // os << node.str << std::endl;
}

std::list<std::string> lines(std::string s) {
    std::string accum;
    std::list<std::string> list;

    for(char c : s) {
        if(c == '\n') {
            list.push_back(accum);
            accum.clear();
        } else {
            accum += c;
        }
    }
    list.push_back(accum);
    return list;
}

bool onlyWS(std::string s) {
    for(char c : s) {
        if(c != ' ' && c != '\t') return false;
    }
    return true;
}

std::string removeBlankLines(std::string s) {
    std::list<std::string> list;
    for(std::string str : lines(s)) {
        if(!(str.empty() || onlyWS(str))) list.push_back(str);
    }

    std::string accum;
    for(std::string str : list) {
        accum += (str + "\n");
    }
    return accum;
}


