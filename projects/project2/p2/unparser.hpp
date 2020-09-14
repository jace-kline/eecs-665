#ifndef __UNPARSER_HPP
#define __UNPARSER_HPP

#include <list>
#include <string>
#include <ostream>
#include <fstream>
#include "tokens.hpp"

// An UnparseNode object is simply a wrapper around a std::string with overloaded operators
struct UnparseNode {
    std::string str;
    UnparseNode(holyc::Token& token);
    UnparseNode(std::string s);
    UnparseNode(const UnparseNode& other);
    UnparseNode operator+(const UnparseNode& other);
};

void writeUnparsed(UnparseNode& node, std::ostream& os);

std::list<std::string> lines(std::string s);

bool onlyWS(std::string s);

std::string removeBlankLines(std::string s);

#endif