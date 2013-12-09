/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (14 November 2013)
 *
 * bada.cpp
 *
 * Implementation counterpart to bada.hpp.
 */

#include "bada.hpp"

#include <cassert>

/* Convert a keyword lexeme representing a Baby Ada type to its corresponding
 * bada_type. */
bada_type keyword_to_type (const ci_string& keyword) {
    /* With only three types to worry about, no point getting too
     * sophisticated. An if/else series will do fine. */
    if (!keyword.compare(KWD_INTEGER)) {
        return bada_type::integer;
    }
    else if (!keyword.compare(KWD_REAL)) {
        return bada_type::real;
    }
    else if (!keyword.compare(KWD_BOOLEAN)) {
        return bada_type::boolean;
    }

    /* Should never be reached, unless the scanner's broken. */
    assert(false);
}

/* Deduce the type of a given literal value. */
bada_type literal_to_type (const ci_string& literal) {
    if (!literal.compare(KWD_TRUE) || !literal.compare(KWD_FALSE)) {
        return bada_type::boolean;
    }
    else if (ci_string::npos != literal.find('.')) {
        return bada_type::real;
    }
    else {
        return bada_type::integer;
    }
}
