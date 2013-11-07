#ifndef PARSER_HPP
#define PARSER_HPP

#include "scanner.hpp"

#include <set>

using first_set = std::set<token::token_id>;

class parser {
public:
    parser (std::istream& input, scanner gettoken)
            : m_gettoken(gettoken)
            , m_input(input) {
        m_gettoken(m_input, m_token);
        program();
        match(token::eof);
    }

private:
    void program ();
    void stats ();
    void decls ();
    void decl ();
    void rest ();
    void statmt ();
    void assignstat ();
    void ifstat ();
    void readstat ();
    void writestat ();
    void loopst ();
    void blockst ();
    void declpart ();
    void writeexp ();
    void express ();
    void expprime ();
    void term ();
    void termprime ();
    void relfactor ();
    void factorprime ();
    void factor ();
    void idnonterm ();

    void match (const token::token_id);
    bool predict (const token::token_id) const;
    bool predict (const first_set&) const;

    scanner m_gettoken;
    std::istream& m_input;
    token m_token;
};

#endif
