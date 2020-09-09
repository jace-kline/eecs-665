#ifndef __LAB2_MANAGER_HPP
#define __LAB2_MANAGER_HPP

#include "scanner.hpp"
#include "grammar.hh"
#include <fstream>
#include <cassert>

namespace LAB2 {
    class Manager {
        private:
            Scanner * scanner;
            Parser * parser;
        public:
            Manager(): scanner(nullptr), parser(nullptr) {}
            ~Manager() {}
            void parse(const char * filename);
    };
}

#endif
