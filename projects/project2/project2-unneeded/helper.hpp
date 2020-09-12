#ifndef __LIST_HELP_HPP
#define __LIST_HELP_HPP

#include <list>
#include <ostream>
#include "tokens.hpp"

using TokenList = std::list<holyc::Token*>;
using StringList = std::list<std::string>;

TokenList operator+(const TokenList& left, const TokenList& right);

TokenList operator+(holyc::Token* left_token, const TokenList& right);

TokenList operator+(const TokenList& left, holyc::Token* right_token);

TokenList operator*=(TokenList& left, const TokenList& right);

TokenList operator*=(TokenList& left, holyc::Token* right_token);

void copyElements(const TokenList& from, TokenList& to);

// fold over list and convert each token to its string form
StringList tokenStrList(TokenList tokens);

// write parsed token strings to output stream
void outputTokenStrs(StringList strlist, std::ostream& os);

#endif