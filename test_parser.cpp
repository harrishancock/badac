#include "parser.hpp"

#include <cassert>

#include <fstream>

int main (int argc, char** argv) {
    assert(argc > 1);
    std::ifstream input (argv[1]);
    parser p (input, scanner());
    input.close();
}
