#include "helper.hpp"

TokenList operator+(const TokenList& left, const TokenList& right) {
    // create new list
    TokenList combined;

    // add left list elements
    copyElements(left, combined);

    // add right list elements
    copyElements(right, combined);
    
    return combined;
}

TokenList operator+(holyc::Token* left_token, const TokenList& right) {
    // create new list
    TokenList combined;

    // add left token
    combined.push_back(left_token);

    // add right list elements
    copyElements(right, combined);
    
    return combined;
}

TokenList operator+(const TokenList& left, holyc::Token* right_token) {
    // create new list
    TokenList combined;

    // add left list elements
    copyElements(left, combined);

    // add right token
    combined.push_back(right_token);
    
    return combined;
}

TokenList operator*=(TokenList& left, const TokenList& right) {
    left.clear();
    left = right;
    return left;
}

TokenList operator*=(TokenList& left, holyc::Token* right_token) {
    left.clear();
    left.push_back(right_token);
    return left;
}

void copyElements(const TokenList& from, TokenList& to) {
    for(holyc::Token * tokPtr : from) {
        to.push_back(tokPtr);
    }
}

StringList tokenStrList(TokenList tokens) {
    StringList strs;
    for(holyc::Token * tokPtr : tokens) {
        if(tokPtr != nullptr) strs.push_back(tokPtr->toString());
    }
    return strs;
}

void outputTokenStrs(StringList strlist, std::ostream& os) {
    for(std::string str : strlist) {
        os << str << std::endl;
    }
    return;
}