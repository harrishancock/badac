/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (7 November 2013)
 *
 * parser.hpp
 *
 * Declaration of the parser class.
 */

#ifndef PARSER_HPP
#define PARSER_HPP

#include "scoped_symbol_table.hpp"
#include "scanner.hpp"

#include <set>

using first_set = std::set<token::token_id>;

/* The parser requires some state (an input stream, a scanner, and a current
 * token), which would normally necessitate either global variables or
 * "threading" the state as arguments through the recursive descent routines.
 * I don't particularly like either of those approaches, so I made the parser
 * an object instead: the constructor sets up the state, then performs the
 * parse--a parser object is therefore a single-use object which performs a
 * computation once. Since we're not building parse trees yet, there is no
 * "return value", but this can be provided for later by including a
 * get_parse_tree() const accessor function. The parser object can later be
 * wrapped by a free function, say called parse(), that would look sort of
 * like this:
 *
 *      parse_tree parse (std::istream& input) {
 *          parser p (input, scanner());
 *          return p.get_parse_tree();
 *      }
 *
 * Such a free function isn't necessary; it would just hide some of the oddity
 * of using a constructor to perform a computation. */
class parser {
public:
    parser (std::istream& input, scanner gettoken)
            : m_gettoken(gettoken)
            , m_input(input) {
        /* Perform the actual parse. */
        m_gettoken(m_input, m_token);
        program();
        match(token::eof);
    }

    /* Dump the current state of the symbol table to the given output stream. */
    void display_symbol_table (std::ostream& output) {
        m_symtab.display(output);
    }

    /* Indicate whether or not the program passed semantic checking during the
     * parse. */
    bool good () const { return m_good; }

private:
    /* Baby Ada nonterminals. */
    void program ();
    void stats ();
    void decls ();
    void decl ();
    void rest (const token& declared_id);
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

    /* match and predict are the two primary operations the parser uses to
     * work its way through the token stream. */
    void match (const token::token_id);
    bool predict (const token::token_id) const;
    bool predict (const first_set&) const;

    /* Add a data object (variable) to the symbol table. The
     * add_constant_data_object function is a variant of this which records a
     * constant "variable". */
    void add_data_object (const token& type, const token& id, bool is_constant = false);
    void add_constant_data_object (const token& type, const token& id);

    /* For now, this simply verifies that the given identifer has been
     * previously declared in some active scope. */
    void check_referenced_data_object (const token& id);

    scanner m_gettoken;
    std::istream& m_input;
    token m_token;

    scoped_symbol_table m_symtab;
    location_type m_next_location = 0;

    /* This flag is set to false if an error occurs. */
    bool m_good = true;
};

#endif
