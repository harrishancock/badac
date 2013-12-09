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

    const char* in_filename = argv[1];
    const char* out_filename = "out.s";

    std::cout << "Translating from " << in_filename << " to " << out_filename << '\n';

    std::ifstream input (in_filename);
    std::ofstream output (out_filename);
    /* Note that this next line performs the actual parse during object
     * instantiation. */
    parser p (input, scanner(), output);
    input.close();
    output.close();

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

    return !p.good();
}
