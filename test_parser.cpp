/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (14 November 2013)
 *
 * test_parser.cpp
 *
 * A quick and dirty test driver for the Baby Ada parser.
 */

#include "parser.hpp"
#include <cassert>
#include <fstream>
#include <iostream>

/* Usage: ./test_parser <filename>
 *      where filename is an Ada program
 */
int main (int argc, char** argv) {
    assert(argc > 1);
    std::ifstream input (argv[1]);
    /* Note that this next line performs the actual parse during object
     * instantiation. */
    parser p (input, scanner());
    input.close();

    /* Since we simply abort on syntactic errors (for now), if we get this far
     * then we know that the program has a valid parse tree. */
    std::cout << "\nProgram is syntactically correct\n";

    if (p.good()) {
        std::cout << "Program accepted!\n";
    }
    else {
        std::cout << "Program failed semantic checking\n";
    }

    p.display_symbol_table(std::cout);
}
