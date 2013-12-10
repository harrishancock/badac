/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (14 November 2013)
 *
 * main.cpp
 *
 * Harris' Baby Ada Compiler main function.
 */

#include "parser.hpp"

#include <cassert>

#include <fstream>
#include <iostream>
#include <sstream>

/* Usage: ./badac <filename>
 *      where filename is an Ada program
 */
int main (int argc, char** argv) {
    if (argc < 2) {
        std::cout << "Usage: badac <input file>\n\n"
            "<input file> is a Baby Ada source file\n";
        return 1;
    }

    const char* in_filename = argv[1];
    const char* out_filename = "out.s";

    std::cout << "Translating from " << in_filename << " to " << out_filename << '\n';

    std::ifstream input (in_filename);
    std::stringstream output;
    /* Note that this next line performs the actual parse during object
     * instantiation. */
    parser p (input, scanner(), output);
    input.close();

    if (p.good()) {
        std::cout << "Program accepted!\n";

        /* Write the actual MIPS code file. */
        std::ofstream outfile (out_filename);
        outfile << output.rdbuf();
    }
    else {
        if (parser::SYNTAX_ERROR == p.errcode()) {
            std::cout << "Program failed syntax checking.\n";
        }
        else if (parser::STATIC_SEMANTIC_ERROR == p.errcode()) {
            std::cout << "Program failed static semantic checking.\n";
        }
    }

    p.display_symbol_table(std::cout);

    return !p.good();
}
