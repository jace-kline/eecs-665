#ifndef P1_TRACKER_HPP
#define P1_TRACKER_HPP

#include <string>

// used to track the state of a built char or string literal
struct Tracker {
    bool bad_esc;
    int col_start;
    int col_count;
    std::string str;
    
    // default constructor
    Tracker() {
        bad_esc = false;
        col_start = -1;
        str = "";
    }

    // append a single character to the string
    void append(const char * s) {
        str.append(s);
    }

    void append(std::string s) {
        str.append(s);
    }

    // reset the tracker to track a new string
    void set(int col) {
        bad_esc = false;
        col_start = col;
        str.clear();
        str = "";
    }
};



#endif