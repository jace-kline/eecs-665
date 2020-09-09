#include "calc.hpp"

void LAB2::Manager::parse(const char * filepath) {
    std::ifstream in_stream(filepath);
    this->scanner = new LAB2::Scanner(&in_stream);
    this->parser = new LAB2::Parser(*scanner, *this);

    parser->parse();
}

int main(const int argc, const char *argv[]) {
    assert(argc > 1);

    LAB2::Manager manager;
    manager.parse(argv[1]);
}