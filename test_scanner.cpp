#include "scanner.hpp"
#include <cassert>
#include <fstream>

int main (int argc, char** argv) {
    assert(argc > 1);
    std::ifstream input (argv[1]);
    scanner gettoken;
    token tok;
    while (gettoken(input, tok)) {
        std::cout << tok << '\n';
    }
    std::cout << tok << '\n';
}
