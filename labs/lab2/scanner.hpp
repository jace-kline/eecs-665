#ifndef __LAB2_SCANNER_HPP
#define __LAB2_SCANNER_HPP

#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "grammar.hh"
#include <istream>

namespace LAB2{
    class Scanner : yyFlexLexer {
        private:
            LAB2::Parser::semantic_type *yylval = nullptr;
            size_t lineNum;
            size_t charNum;
        public:
            Scanner(std::istream * in) : yyFlexLexer(in) {
                lineNum = 0;
                charNum = 0;
            }
            virtual ~Scanner() {}
            virtual int yylex( LAB2::Parser::semantic_type * lval);

            void warn(int lineNum, int charNum, std::string msg) {
                std::cerr << "Lexical WARNING [" << lineNum << ", " << charNum << "]: " << msg << std::endl;
            }

            void error(int lineNum, int charNum, std::string msg) {
                std::cerr << "Lexical ERROR [" << lineNum << ", " << charNum << "]: " << msg << std::endl;
            }
    };
}

#endif