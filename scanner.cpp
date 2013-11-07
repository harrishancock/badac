/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Scanner Assignment (22 October 2013)
 *
 * scanner.cpp
 */

#include "scanner.hpp"

namespace {

/* Clear a stream's end-of-file state. */
void clear_eof (std::ios& stream) {
    stream.clear(stream.rdstate() & ~std::ios::eofbit);
}

} // anonymous namespace

//////////////////////////////////////////////////////////////////////////////

/* class token implementation */

const char* token::token_id_to_string (token_id elem) {
    switch (elem) {
#define ENUMITEM(x) case x: return #x;
        ENUMITEM(unterminated_string_error)
        ENUMITEM(invalid_identifier_error)
        ENUMITEM(invalid_numeric_error)
        ENUMITEM(unrecognized_input_error)
        ENUMITEM(eof)
        ENUMITEM(identifier)
        ENUMITEM(literal)
        ENUMITEM(string)
        ENUMITEM(procedure)
        ENUMITEM(is)
        ENUMITEM(declare)
        ENUMITEM(constant)
        ENUMITEM(type)
        ENUMITEM(addop)
        ENUMITEM(mulop)
        ENUMITEM(not_)
        ENUMITEM(if_)
        ENUMITEM(then)
        ENUMITEM(while_)
        ENUMITEM(write)
        ENUMITEM(read)
        ENUMITEM(begin)
        ENUMITEM(end)
        ENUMITEM(loop)
        ENUMITEM(relop)
        ENUMITEM(semicolon)
        ENUMITEM(colon)
        ENUMITEM(assign)
        ENUMITEM(lparen)
        ENUMITEM(rparen)
#undef ENUMITEM
        default:
            return "(unknown token)";
    }
}

const std::map<std::string, token::token_id, ciless> token::s_keyword_to_token_id {
    { "true", literal },
    { "false", literal },
    { "procedure", procedure },
    { "is", is },
    { "declare", declare },
    { "constant", constant },
    { "integer", type },
    { "real", type },
    { "boolean", type },
    { "or", addop },
    { "mod", mulop },
    { "and", mulop },
    { "not", not_ },
    { "if", if_ },
    { "then", then },
    { "while", while_ },
    { "put", write },
    { "put_line", write },
    { "get", read },
    { "begin", begin },
    { "end", end },
    { "loop", loop }
};

//////////////////////////////////////////////////////////////////////////////

/* class scanner implementation */

std::istream& scanner::operator() (std::istream& input, token& tok) {
    char c = skip_ws_and_comments(input);

    if (!input) {
        tok = token(token::eof, m_current_line);
        return input;
    }

    /* Decide what kind of lexeme this is, and call the appropriate scanning
     * function. */

    if ('"' == c) {
        tok = token(token::string, m_current_line, c);
        handle_quote(input, tok);
    }
    else if ('_' == c || isalpha(c)) {
        tok = token(m_current_line, c);
        handle_alpha(input, tok, '_' == c);
    }
    else if ('.' == c || isdigit(c)) {
        tok = token(token::literal, m_current_line, c);
        handle_digit(input, tok, '.' == c);
    }
    else {
        tok = token(m_current_line, c);
        handle_operator(input, tok, c);
    }

    /* We would have caught an eof at the very top. Any eof state in our input
     * stream at this point must therefore be due to calling peek(), and we
     * shouldn't report this to the user yet. */
    clear_eof(input);

    return input;
}

/* Fast forward the input stream over whitespace and comments, return the
 * first other character. */
int scanner::skip_ws_and_comments (std::istream& input) {
    char c = input.get();

    if (!input) {
        return std::istream::traits_type::eof();
    }

    /* Fast forward over comments. */
    if (input && '-' == c && '-' == input.peek()) {
        while (input.get(c) && '\n' != c) { }
    }

    /* Fast forward over whitespace and comments. */
    while (input && isspace(c)) {
        if ('\n' == c) {
            ++m_current_line;
        }
        c = input.get();

        /* Fast forward over comments. */
        if (input && '-' == c && '-' == input.peek()) {
            while (input.get(c) && '\n' != c) { }
        }
    }

    return c;
}

/* Scan a quoted string literal. */
void scanner::handle_quote (std::istream& input, token& tok) {
    enum { terminated, unterminated } state = unterminated;

    char c;

    while (input && '\n' != input.peek()) {
        if (terminated == state) {
            if ('"' == input.peek()) {
                state = unterminated;
                tok.push_char(input.get());
            }
            else {
                break;
            }
        }
        else {
            input.get(c);
            tok.push_char(c);
            if ('"' == c) {
                state = terminated;
            }
        }
    }

    if (unterminated == state) {
        tok.set_token_id(token::unterminated_string_error);
    }
}

/* Scan a keyword or identifier. */
void scanner::handle_alpha (std::istream& input, token& tok, bool invalid) {
    enum { terminated, unterminated } state = terminated;

    // FIXME
    //tok.set_lineno(m_current_line);

    /* This while loop scoops up every alphanumeric or underscore character
     * on the input until it hits something else. This does not entirely match
     * the DFA I wrote (which would error out at the first double underscore),
     * but this helps generate a more readable error. */
    while (input) {
        if (isalpha(input.peek()) || isdigit(input.peek())) {
            state = terminated;
            tok.push_char(input.get());
        }
        else if ('_' == input.peek()) {
            state = unterminated;
            tok.push_char(input.get());
            if ('_' == input.peek()) {
                invalid = true;
            }
        }
        else {
            break;
        }
    }

    if (invalid || unterminated == state) {
        tok.set_token_id(token::invalid_identifier_error);
    }
    else {
        tok.classify_alpha();
    }
}

/* Scan a numeric. The invalid flag is used so that we can use this function
 * to scan numerics that begin with a period, and set an error condition
 * appropriately. */
void scanner::handle_digit (std::istream& input, token& tok, bool invalid) {
    enum { terminated, unterminated } state = terminated;
    int dots = 0;

    /* Like handle_alpha(), scoop up all digits or periods ahead of us in the
     * input stream, to generate more readable errors. Note that this will
     * eat things like "1..2", which means that if a period were a separate
     * lexeme, we'd have to modify this. */
    while (input) {
        if (isdigit(input.peek())) {
            state = terminated;
            tok.push_char(input.get());
        }
        else if ('.' == input.peek()) {
            ++dots;
            state = unterminated;
            tok.push_char(input.get());
        }
        else {
            break;
        }
    }

    if (invalid || dots > 1 || unterminated == state) {
        tok.set_token_id(token::invalid_numeric_error);
    }
}

/* Scan an operator. */
void scanner::handle_operator (std::istream& input, token& tok, char op) {
    switch (op) {
        case '+': /* fall-through */
        case '-':
            tok.set_token_id(token::addop);
            break;
        case '*': /* fall-through */
        case '/':
            tok.set_token_id(token::mulop);
            break;
        case '<': /* fall-through */
        case '>': /* fall-through */
        case '=':
            tok.set_token_id(token::relop);
            break;
        case ';':
            tok.set_token_id(token::semicolon);
            break;
        case ':':
            if ('=' == input.peek()) {
                tok.push_char(input.get());
                tok.set_token_id(token::assign);
            }
            else {
                tok.set_token_id(token::colon);
            }
            break;
        case '(':
            tok.set_token_id(token::lparen);
            break;
        case ')':
            tok.set_token_id(token::rparen);
            break;
        default:
            tok.set_token_id(token::unrecognized_input_error);
            break;
    }
}
