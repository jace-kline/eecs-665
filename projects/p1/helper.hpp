#ifndef P1_HELPER_HPP
#define P1_HELPER_HPP

#include <string>

struct StrTracker {
    int col_start;
    std::string str;
    
    // default constructor
    StrTracker() {
        col_start = -1;
    }

    // append a single character to the string
    void append(char c) {
        str.append(1, c);
    }

    // reset the tracker to track a new string
    void reset(int col) {
        col_start = col;
        str.clear();
    }
};



#endif