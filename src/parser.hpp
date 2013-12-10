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

#include <cstdarg>

#include <set>

using first_set = std::set<token::token_id>;

/* The parser requires some state (an input stream, a scanner, and a current
 * token), which would normally necessitate either global variables or
 * "threading" the state as arguments through the recursive descent routines.
 * I don't particularly like either of those approaches, so I made the parser
 * an object instead: the constructor sets up the state, then performs the
 * parse--a parser object is therefore a single-use object which performs a
 * computation once. */
class parser {
public:
    parser (std::istream& input, scanner gettoken, std::ostream& output)
            : m_gettoken(gettoken)
            , m_input(input)
            , m_output(output) {
        codegen_preamble();

        /* Perform the actual parse. */
        m_gettoken(m_input, m_token);
        program();
        match(token::eof);

        codegen_postamble();
    }

    /* Dump the current state of the symbol table to the given output stream. */
    void display_symbol_table (std::ostream& output) {
        m_symtab.display(output);
    }

    /* Indicate whether or not the program passed syntax and static semantic
     * checking. */
    bool good () const { return !m_errcode; }

    enum error_code {
        NO_ERROR = 0,
        SYNTAX_ERROR,
        STATIC_SEMANTIC_ERROR
    };

    /* Get detailed error code information. */
    error_code errcode () const { return m_errcode; }

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
    void express (data_object_record&);
    void expprime (data_object_record&);
    void term (data_object_record&);
    void termprime (data_object_record&);
    void relfactor (data_object_record&);
    void factorprime (data_object_record&);
    void factor (data_object_record&);
    void idnonterm (data_object_record&);

    //////////////////////////////////////////////////////////////////////////

    /* Emit an error message. fmt must be a printf-compatible format string
     * suitable for use with the rest of the arguments passed. */
    void emit_error (int lineno, const char* fmt, va_list ap);
    
    /* Emit a syntax error and abort. fmt has the same constraints as for
     * emit_error(). */
    void syntax_error (int lineno, const char* fmt, ...);

    /* Emit a static semantic error and continue. fmt has the same constraints as for
     * emit_error(). */
    void static_semantic_error (int lineno, const char* fmt, ...);

    /* match and predict are the two primary operations the parser uses to
     * work its way through the token stream. */
    void match (const token::token_id);
    bool predict (const token::token_id) const;
    bool predict (const first_set&) const;

    //////////////////////////////////////////////////////////////////////////

    /* Add a data object (variable) to the symbol table. The
     * add_constant_data_object function is a variant of this which records a
     * constant "variable". */
    void add_data_object_to_symtab (const token& id, const data_object_record& record);

    /* Load the type and location of the given identifier's referenced object
     * into exprec. */
    void get_referenced_data_object (const token& id, data_object_record& exprec);

    /* Make data objects on the stack. */
    data_object_record make_data_object_on_stack (bada_type type, bool is_constant = false);
    data_object_record make_constant_data_object_on_stack (bada_type type);

    //////////////////////////////////////////////////////////////////////////

    /* Generate the MIPS program start/end boilerplate.
     * A comment on an earlier iteration of this code asked if "postamble" is
     * a word. While few dictionaries define it, I find that it is a fairly
     * common word in a computing context; moreover, preamble/postamble was the
     * word pair used in the textbook for CS 2430 in the context of function
     * calls in MIPS, so it seemed a natural choice here. */
    void codegen_preamble ();
    void codegen_postamble ();

    /* Generate one line of MIPS code, unindented, tab-delimited. */
    void codegen_raw (std::string arg1);
    void codegen_raw (std::string arg1, std::string arg2);
    void codegen_raw (std::string arg1, std::string arg2, std::string arg3);

    /* Generate one line of MIPS code, indented, tab after instr, commas
     * between arguments. */
    void codegen (std::string instr);
    void codegen (std::string instr, std::string arg1);
    void codegen (std::string instr, std::string arg1, std::string arg2);
    void codegen (std::string instr, std::string arg1, std::string arg2, std::string arg3);

    /* Generate code corresponding to a Baby Ada operation. */
    void codegen_op (const token& tok, data_object_record dest, data_object_record lhs, data_object_record rhs);

    /* Generate code to initialize a Baby Ada data object. */
    void codegen_initialize_data_object (const data_object_record& record, const token& lit);

    /* Generate a unique MIPS label for a section of data. */
    std::string next_data_label () {
        return std::string("_data_") + std::to_string(m_next_string_label++);
    }

    /* Generate a unique MIPS label for a section of code. */
    std::string next_code_label () {
        return std::string("_code_") + std::to_string(m_next_code_label++);
    }

    //////////////////////////////////////////////////////////////////////////

    scanner m_gettoken;
    token m_token;

    std::istream& m_input;
    std::ostream& m_output;

    scoped_symbol_table m_symtab;
    location_type m_next_location = 0;

    int m_next_string_label = 0;
    int m_next_code_label = 0;

    /* This flag is set to false if an error occurs. */
    error_code m_errcode = NO_ERROR;

    /* True after "true" and "false" .asciiz MIPS directives have been
     * generated. */
    bool m_have_boolean_string_literals = false;
};

#endif
