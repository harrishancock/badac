/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (5 November 2013)
 *
 * scanner.hpp
 */

#ifndef SCANNER_HPP
#define SCANNER_HPP

#include <cctype>

#include <algorithm>
#include <iostream>
#include <map>
#include <string>

/* Case-insensitive comparator for use with our keyword map. */
struct ciless {
    bool operator() (const std::string& lhs, const std::string& rhs) const {
        return std::lexicographical_compare(
                lhs.begin(), lhs.end(),
                rhs.begin(), rhs.end(),
                [] (char x, char y) { return tolower(x) < tolower(y); });
    }
};

/* Representation of a token, complete with lexeme and line number from which
 * it appears. */
class token {
public:
    /* All the different token values, plus some error cases. The names chosen
     * are pretty spare; this is fine, because to access them we have to
     * qualify it with token's scope, anyway (i.e., token::eof). */
    enum token_id {
        unterminated_string_error = -10,
        invalid_identifier_error,
        invalid_numeric_error,
        unrecognized_input_error,
        eof = 0,
        identifier,
        literal,
        string,
        procedure,
        is,
        declare,
        constant,
        type,
        addop,
        mulop,
        not_,
        if_,
        then,
        while_,
        write,
        read,
        begin,
        end,
        loop,
        relop,
        semicolon,
        colon,
        assign,
        lparen,
        rparen
    };

    token () = default;

    token (token_id elem, int lineno, char c)
        : m_token_id(elem)
        , m_lineno(lineno)
        , m_lexeme(1, c) { }

    token (token_id elem, int lineno)
        : m_token_id(elem)
        , m_lineno(lineno) { }

    token (int lineno, char c)
        : m_token_id(eof)
        , m_lineno(lineno)
        , m_lexeme(1, c) { }

    void push_char (char c) { m_lexeme += c; }
    void set_lineno (int lineno) { m_lineno = lineno; }
    void set_token_id (token_id id) { m_token_id = id; }

    int lineno () const { return m_lineno; }

    token_id get_token_id () const { return m_token_id; }

    /* For an alphabetic lexeme, look up the token number in the keyword
     * dictionary. Sets token to identifier, if not found in the dictionary. */
    void classify_alpha () {
        auto it = s_keyword_to_token_id.find(m_lexeme);
        if (s_keyword_to_token_id.end() != it) {
            m_token_id = it->second;
        }
        else {
            m_token_id = identifier;
        }
    }

    /* Write a text representation of this token to output. */
    friend std::ostream& operator<< (std::ostream& output, const token& tok) {
        return output << tok.m_lineno << ": "
                      << token_id_to_string(tok.m_token_id)
                      << " is \"" << tok.m_lexeme << "\"";
    }

private:
    /* A case-insensitive map of keywords to token_ids. */
    static const std::map<std::string, token::token_id, ciless> s_keyword_to_token_id;

    /* Get a human-readable C string for a given token_id. */
    static const char* token_id_to_string (token_id elem);

    token_id m_token_id;
    int m_lineno;
    std::string m_lexeme;
};

//////////////////////////////////////////////////////////////////////////////

/* A function object which extracts a token from an input stream while
 * maintaining line number state. Use like so:
 *
 *  token tok;
 *  scanner gettoken;
 *  while (gettoken(input, tok)) {
 *      ...
 *  }
 * 
 * where input is some object of type std::istream.
 */
class scanner {
public:
    /* The primary scanning routine. Extract the next lexeme from the input
     * stream, classify it and assign it to tok. Return the input stream, so
     * the user can call this function inside a while condition. */
    std::istream& operator() (std::istream& input, token& tok);

private:
    int skip_ws_and_comments (std::istream& input);

    /* Handler functions to scan certain kinds of lexemes. */
    void handle_quote (std::istream& input, token& tok);
    void handle_alpha (std::istream& input, token& tok, bool invalid);
    void handle_digit (std::istream& input, token& tok, bool invalid);
    void handle_operator (std::istream& input, token& tok, char op);

    int m_current_line = 1;
};

#endif
