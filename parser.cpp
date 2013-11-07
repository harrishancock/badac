#include "parser.hpp"

#include <cstdlib>

namespace {

/* For every right-hand-side in the Baby Ada BNF grammar, we need a rule
 * number that we can display to depict the parser's derivation. In general,
 * a _null suffix is the rule number corresponding to the empty right-hand-
 * side for nullable rules. Rules with many different right-hand-sides, e.g.
 * statmt, will have several different suffixes.
 *
 * Note that these numbers are used only for display--they have no effect on
 * the actual parser. */
enum class rule {
    program,
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
    factor_not, factor_id, factor_numeric, factor_express,
    idnonterm
};

/* We also need to have access to the FIRST sets for some of the rules. */

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

first_set decl {
    token::identifier
};

first_set assignstat {
    token::identifier
};

first_set ifstat {
    token::if_
};

first_set readstat {
    token::read
};

first_set writestat {
    token::write
};

first_set blockst {
    token::declare,
    token::begin
};

first_set loopst {
    token::while_
};

first_set express {
    token::not_,
    token::identifier,
    token::numeric,
    token::lparen
};

first_set idnonterm {
    token::identifier
};

} // namespace first

void error () {
    std::cout << "shit\n";
    abort();
}

} // anonymous namespace

//////////////////////////////////////////////////////////////////////////////

/* Print out a rule number. */
#define RULE(x) std::cout << static_cast<int>(rule::x) << ' '

void parser::match (const token::token_id tok) {
    if (tok != m_token.get_token_id()) {
        error();
    }
    else {
        m_gettoken(m_input, m_token);
    }
}

/* Return true if the current token would match tok. */
bool parser::predict (const token::token_id tok) const {
    return tok == m_token.get_token_id();
}

/* Return true if the current token is a member of the given FIRST set. */
bool parser::predict (const first_set& fir) const {
    return fir.end() != fir.find(m_token.get_token_id());
}

//////////////////////////////////////////////////////////////////////////////

void parser::program () {
    RULE(program);

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
        RULE(stats);

        statmt();
        stats();
    }
    else {
        RULE(stats_null);
    }
}

void parser::decls () {
    if (predict(first::decl)) {
        RULE(decls);

        decl();
        decls();
    }
    else {
        RULE(decls_null);
    }
}

void parser::decl () {
    RULE(decl);

    match(token::identifier);
    match(token::colon);
    rest();
}

void parser::rest () {
    if (predict(token::type)) {
        RULE(rest_type);

        match(token::type);
        match(token::semicolon);
    }
    else if (predict(token::constant)) {
        RULE(rest_constant);

        match(token::constant);
        match(token::type);
        match(token::assign);
        match(token::numeric);
        match(token::semicolon);
    }
    else {
        error();
    }
}

void parser::statmt () {
    if (predict(first::assignstat)) {
        RULE(statmt_assign);
        assignstat();
    }
    else if (predict(first::ifstat)) {
        RULE(statmt_if);
        ifstat();
    }
    else if (predict(first::readstat)) {
        RULE(statmt_read);
        readstat();
    }
    else if (predict(first::writestat)) {
        RULE(statmt_write);
        writestat();
    }
    else if (predict(first::blockst)) {
        RULE(statmt_block);
        blockst();
    }
    else if (predict(first::loopst)) {
        RULE(statmt_loop);
        loopst();
    }
    else {
        error();
    }
}

void parser::assignstat () {
    RULE(assignstat);

    idnonterm();
    match(token::assign);
    express();
    match(token::semicolon);
}

void parser::ifstat () {
    RULE(ifstat);

    match(token::if_);
    express();
    match(token::then);
    stats();
    match(token::end);
    match(token::if_);
    match(token::semicolon);
}

void parser::readstat () {
    RULE(readstat);

    match(token::read);
    match(token::lparen);
    idnonterm();
    match(token::rparen);
    match(token::semicolon);
}

void parser::writestat () {
    RULE(writestat);

    match(token::write);
    match(token::lparen);
    writeexp();
    match(token::rparen);
    match(token::semicolon);
}

void parser::loopst () {
    RULE(loopst);

    match(token::while_);
    express();
    match(token::loop);
    stats();
    match(token::end);
    match(token::loop);
    match(token::semicolon);
}

void parser::blockst () {
    RULE(blockst);

    declpart();
    match(token::begin);
    stats();
    match(token::end);
    match(token::semicolon);
}

void parser::declpart () {
    if (predict(token::declare)) {
        RULE(declpart);

        match(token::declare);
        decl();
        decls();
    }
    else {
        RULE(declpart_null);
    }
}

void parser::writeexp () {
    if (predict(token::string)) {
        RULE(writeexp_string);

        match(token::string);
    }
    else if (predict(first::express)) {
        RULE(writeexp_express);

        express();
    }
    else {
        error();
    }
}

void parser::express () {
    RULE(express);

    term();
    expprime();
}

void parser::expprime () {
    if (predict(token::addop)) {
        RULE(expprime);

        match(token::addop);
        term();
        expprime();
    }
    else {
        RULE(expprime_null);
    }
}

void parser::term () {
    RULE(term);

    relfactor();
    termprime();
}

void parser::termprime () {
    if (predict(token::mulop)) {
        RULE(termprime);

        match(token::mulop);
        relfactor();
        termprime();
    }
    else {
        RULE(termprime_null);
    }
}

void parser::relfactor () {
    RULE(relfactor);

    factor();
    factorprime();
}

void parser::factorprime () {
    if (predict(token::relop)) {
        RULE(factorprime);

        match(token::relop);
        factor();
    }
    else {
        RULE(factorprime_null);
    }
}

void parser::factor () {
    if (predict(token::not_)) {
        RULE(factor_not);

        match(token::not_);
        factor();
    }
    else if (predict(first::idnonterm)) {
        RULE(factor_id);

        idnonterm();
    }
    else if (predict(token::numeric)) {
        RULE(factor_numeric);

        match(token::numeric);
    }
    else if (predict(token::lparen)) {
        RULE(factor_express);

        match(token::lparen);
        express();
        match(token::rparen);
    }
    else {
        error();
    }
}

void parser::idnonterm () {
    RULE(idnonterm);

    match(token::identifier);
}
