/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (14 November 2013)
 *
 * bada.hpp
 *
 * Some details of Baby Ada, such as the exact string literals to use for
 * keywords, an architecture word size, type enumerations, etc., need to be
 * collected in one place for maintainability. Here is where I'm collecting
 * them.
 */

#ifndef BADA_HPP
#define BADA_HPP

#include "ci_string.hpp"

#include <iostream>

#define WORD_SIZE (4)

/* Keyword string literals. Note that these must always be used in a
 * case-insensitive manner. */
#define KWD_TRUE "true"
#define KWD_FALSE "false"
#define KWD_PROCEDURE "procedure"
#define KWD_IS "is"
#define KWD_DECLARE "declare"
#define KWD_CONSTANT "constant"
#define KWD_INTEGER "integer"
#define KWD_REAL "real"
#define KWD_BOOLEAN "boolean"
#define KWD_OR "or"
#define KWD_MOD "mod"
#define KWD_AND "and"
#define KWD_NOT "not"
#define KWD_IF "if"
#define KWD_THEN "then"
#define KWD_WHILE "while"
#define KWD_PUT "put"
#define KWD_PUT_LINE "put_line"
#define KWD_GET "get"
#define KWD_BEGIN "begin"
#define KWD_END "end"
#define KWD_LOOP "loop"

enum class bada_type {
    integer,
    real,
    boolean
};

/* Convert a keyword lexeme representing a Baby Ada type to its corresponding
 * bada_type. */
bada_type keyword_to_type (const ci_string& keyword);

/* Deduce the type of a given literal value. */
bada_type literal_to_type (const ci_string& literal);

using location_type = std::ptrdiff_t;

/* A simple symbol table entry. */
struct data_object_record {
    void display (std::ostream& output = std::cout) const {
        output << "is_constant<" << is_constant << ">, "
               << "type<"        << static_cast<int>(type) << ">, "
               << "location<"    << location    << ">\n";
    }

    bool is_constant;
    bada_type type;
    location_type location;
};

#endif
