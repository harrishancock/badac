/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (7 November 2013)
 *
 * test_parser.cpp
 *
 * A quick and dirty test driver for the Baby Ada parser.
 */

#include "parser.hpp"
#include <cassert>
#include <fstream>

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
    printf("\nProgram accepted!\n");
}
