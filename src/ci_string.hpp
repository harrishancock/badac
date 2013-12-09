/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (14 November 2013)
 *
 * ci_string.hpp
 *
 * A case-insensitive string for use with Baby Ada identifiers.
 */

#ifndef CI_STRING_HPP
#define CI_STRING_HPP

#include <cctype>

#include <string>

/* A case-insensitive character traits structure, for use in creating a
 * case-insensitive string type. */
struct ci_char_traits : std::char_traits<char> {
    static bool lt (char lhs, char rhs) {
        return tolower(lhs) < tolower(rhs);
    }

    static bool eq (char lhs, char rhs) {
        return tolower(lhs) == tolower(rhs);
    }

    static int compare (const char* lhs, const char* rhs, size_t count) {
        for (size_t i = 0; i < count; ++i) {
            if (lt(lhs[i], rhs[i])) { return -1; }
            if (lt(rhs[i], lhs[i])) { return 1; }
        }
        return 0;
    }
};

/* A case-insensitive string, which we will use for all Baby Ada identifiers
 * and keywords. */
using ci_string = std::basic_string<ci_char_traits::char_type, ci_char_traits>;

static inline std::ostream& operator<< (std::ostream& os, const ci_string& s) {
    return os << s.c_str();
}

#endif
