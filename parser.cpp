/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (7 November 2013)
 *
 * parser.cpp
 *
 * This file contains the implementation of a Baby Ada recursive descent
 * parser. In addition to the production rules themselves, we define an error
 * function, predict and match functions, and some miscellaneous
 * infrastructure.
 */

#include "parser.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>

/* For every right-hand-side in the Baby Ada BNF grammar, we need a rule
 * number that we can display to depict the parser's derivation. I really
 * don't feel like hard-coding the numbers into the recursive descent
 * functions and maintaining them separately, so I'm using an enum. In
 * general, a _null suffix is the rule number corresponding to the empty
 * right-hand-side for nullable rules. Rules with many different
 * right-hand-sides, e.g. statmt, will have several different suffixes.
 *
 * Note that these numbers are used only for display--they have no effect on
 * the actual parser. */

enum class rule {
    program = 1,
    stats, stats_null,
    decls, decls_null,
    decl,
    rest_type, rest_constant,
    statmt_assign, statmt_if,
    statmt_read, statmt_write,
    statmt_block, statmt_loop,
    assignstat,
    ifstat,
    readstat,
    writestat,
    loopst,
    blockst,
    declpart, declpart_null,
    writeexp_string, writeexp_express,
    express,
    expprime, expprime_null,
    term,
    termprime, termprime_null,
    relfactor,
    factorprime, factorprime_null,
    factor_not, factor_id, factor_literal, factor_express,
    idnonterm
};

/* Convenience macro to print out a rule number. */
#define PRINTRULE(x) do { printf("%d ", rule::x); fflush(stdout); } while (0)

/* We also need to have access to the FIRST sets for some of the rules. In
 * general, if a production rule needs to use a FIRST set to predict which way
 * to branch, and that FIRST set has more than one element, I list it here. If
 * it only has one element, I just call predict() with that one token. This
 * means the grammar is a little less maintainable, but defining single-token
 * first_sets felt gratuitous. */

namespace first {

first_set statmt {
    token::identifier,
    token::if_,
    token::read,
    token::write,
    token::declare,
    token::while_,
    token::begin
};

first_set blockst {
    token::declare,
    token::begin
};

first_set express {
    token::not_,
    token::identifier,
    token::literal,
    token::lparen
};

} // namespace first

/* Emit an error message and abort. fmt must be a printf-compatible format
 * string suitable for use with the rest of the arguments passed. */
void error (const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    abort();
}

//////////////////////////////////////////////////////////////////////////////

/* match and predict are the two primary operations that my recursive descent
 * routines will call. predict allows the parser to decide which right-hand-
 * side to branch to for a given production rule. It can be called with a set
 * of token_ids (i.e., a FIRST set), or a single token_id (a FIRST set of size
 * one).
 *
 * match is the means by which the parser verifies that the current token is
 * expected, and advances the token stream. */

/* Check if the current token matches the expected token_id. If it does,
 * advance the token stream. If it does not, report an error. */
void parser::match (const token::token_id tok) {
    if (tok != m_token.get_token_id()) {
        error("line %d: expected token %d, got token %d\n", m_token.lineno(),
                tok, m_token.get_token_id());
    }
    else {
        m_gettoken(m_input, m_token);
    }
}

/* Return true if the current token would match tok, false otherwise. */
bool parser::predict (const token::token_id tok) const {
    return tok == m_token.get_token_id();
}

/* Return true if the current token is a member of the given FIRST set, false
 * otherwise. */
bool parser::predict (const first_set& fir) const {
    return fir.end() != fir.find(m_token.get_token_id());
}

//////////////////////////////////////////////////////////////////////////////

/* What follows is the grammar for Baby Ada, translated into mutually
 * recursive functions. For nullable rules, I return on any unexpected token,
 * not just the rule's FOLLOW set. There are no real surprises in the
 * functions, with each one following a fairly obvious pattern, so I didn't
 * see any need for further comments. */

void parser::program () {
    PRINTRULE(program);

    match(token::procedure);
    match(token::identifier);
    match(token::is);
    decls();
    match(token::begin);
    stats();
    match(token::end);
    match(token::identifier);
    match(token::semicolon);
}

void parser::stats () {
    if (predict(first::statmt)) {
        PRINTRULE(stats);

        statmt();
        stats();
    }
    else {
        PRINTRULE(stats_null);
    }
}

void parser::decls () {
    if (predict(token::identifier)) {
        PRINTRULE(decls);

        decl();
        decls();
    }
    else {
        PRINTRULE(decls_null);
    }
}

void parser::decl () {
    PRINTRULE(decl);

    match(token::identifier);
    match(token::colon);
    rest();
}

void parser::rest () {
    if (predict(token::type)) {
        PRINTRULE(rest_type);

        match(token::type);
        match(token::semicolon);
    }
    else if (predict(token::constant)) {
        PRINTRULE(rest_constant);

        match(token::constant);
        match(token::type);
        match(token::assign);
        match(token::literal);
        match(token::semicolon);
    }
    else {
        error("line %d: expected token (%d | %d), got token %d\n",
                m_token.lineno(),
                token::type, token::constant, m_token.get_token_id());
    }
}

void parser::statmt () {
    if (predict(token::identifier)) {
        PRINTRULE(statmt_assign);
        assignstat();
    }
    else if (predict(token::if_)) {
        PRINTRULE(statmt_if);
        ifstat();
    }
    else if (predict(token::read)) {
        PRINTRULE(statmt_read);
        readstat();
    }
    else if (predict(token::write)) {
        PRINTRULE(statmt_write);
        writestat();
    }
    else if (predict(first::blockst)) {
        PRINTRULE(statmt_block);
        blockst();
    }
    else if (predict(token::while_)) {
        PRINTRULE(statmt_loop);
        loopst();
    }
    else {
        error("line %d: expected token (%d | %d | %d | %d | %d | %d | %d), "
                "got token %d\n", m_token.lineno(), token::identifier,
                token::if_, token::read, token::write, token::begin,
                token::declare, token::while_, m_token.get_token_id());
    }
}

void parser::assignstat () {
    PRINTRULE(assignstat);

    idnonterm();
    match(token::assign);
    express();
    match(token::semicolon);
}

void parser::ifstat () {
    PRINTRULE(ifstat);

    match(token::if_);
    express();
    match(token::then);
    stats();
    match(token::end);
    match(token::if_);
    match(token::semicolon);
}

void parser::readstat () {
    PRINTRULE(readstat);

    match(token::read);
    match(token::lparen);
    idnonterm();
    match(token::rparen);
    match(token::semicolon);
}

void parser::writestat () {
    PRINTRULE(writestat);

    match(token::write);
    match(token::lparen);
    writeexp();
    match(token::rparen);
    match(token::semicolon);
}

void parser::loopst () {
    PRINTRULE(loopst);

    match(token::while_);
    express();
    match(token::loop);
    stats();
    match(token::end);
    match(token::loop);
    match(token::semicolon);
}

void parser::blockst () {
    PRINTRULE(blockst);

    declpart();
    match(token::begin);
    stats();
    match(token::end);
    match(token::semicolon);
}

void parser::declpart () {
    if (predict(token::declare)) {
        PRINTRULE(declpart);

        match(token::declare);
        decl();
        decls();
    }
    else {
        PRINTRULE(declpart_null);
    }
}

void parser::writeexp () {
    if (predict(token::string)) {
        PRINTRULE(writeexp_string);

        match(token::string);
    }
    else if (predict(first::express)) {
        PRINTRULE(writeexp_express);

        express();
    }
    else {
        error("line %d: expected token (%d | %d | %d | %d | %d | %d), "
                "got token %d\n", m_token.lineno(), token::string,
                token::not_, token::identifier, token::literal,
                token::lparen, m_token.get_token_id());
    }
}

void parser::express () {
    PRINTRULE(express);

    term();
    expprime();
}

void parser::expprime () {
    if (predict(token::addop)) {
        PRINTRULE(expprime);

        match(token::addop);
        term();
        expprime();
    }
    else {
        PRINTRULE(expprime_null);
    }
}

void parser::term () {
    PRINTRULE(term);

    relfactor();
    termprime();
}

void parser::termprime () {
    if (predict(token::mulop)) {
        PRINTRULE(termprime);

        match(token::mulop);
        relfactor();
        termprime();
    }
    else {
        PRINTRULE(termprime_null);
    }
}

void parser::relfactor () {
    PRINTRULE(relfactor);

    factor();
    factorprime();
}

void parser::factorprime () {
    if (predict(token::relop)) {
        PRINTRULE(factorprime);

        match(token::relop);
        factor();
    }
    else {
        PRINTRULE(factorprime_null);
    }
}

void parser::factor () {
    if (predict(token::not_)) {
        PRINTRULE(factor_not);

        match(token::not_);
        factor();
    }
    else if (predict(token::identifier)) {
        PRINTRULE(factor_id);

        idnonterm();
    }
    else if (predict(token::literal)) {
        PRINTRULE(factor_literal);

        match(token::literal);
    }
    else if (predict(token::lparen)) {
        PRINTRULE(factor_express);

        match(token::lparen);
        express();
        match(token::rparen);
    }
    else {
        error("line %d: expected token (%d | %d | %d | %d), got token %d\n",
                m_token.lineno(), token::not_, token::identifier,
                token::literal, token::lparen, m_token.get_token_id());
    }
}

void parser::idnonterm () {
    PRINTRULE(idnonterm);

    match(token::identifier);
}
