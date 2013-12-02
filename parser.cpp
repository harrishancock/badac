/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Baby Ada Compiler Assignment (14 November 2013)
 *
 * parser.cpp
 *
 * Update 14 Nov: Rather than tokens using std::strings as their lexeme type,
 * my scanner now uses a ci_string (case-insensitive string), whose compare
 * operations are always case-insensitive. Also, the add_data_object(),
 * add_constant_data_object(), and get_referenced_data_object() functions
 * were added, and used in the grammar.
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


//////////////////////////////////////////////////////////////////////////////


/* Emit an error message. fmt must be a printf-compatible format string
 * suitable for use with the rest of the arguments passed. */
void emit_error (int lineno, const char* fmt, va_list ap) {
    /* For now, compiler output is a bit of a mess, since we're printing the
     * leftmost derivation interleaved with errors. A leading newline will
     * help readability until I can clean up the output. */
    fprintf(stderr, "\nerror on line %d: ", lineno);
    vfprintf(stderr, fmt, ap);
}

/* Emit a syntax error and abort. fmt has the same constraints as for
 * emit_error(). */
void syntax_error (int lineno, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    emit_error(lineno, fmt, ap);
    va_end(ap);
    abort();
}

/* Emit a semantic error and continue. fmt has the same constraints as for
 * emit_error(). */
void semantic_error (int lineno, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    emit_error(lineno, fmt, ap);
    va_end(ap);
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
        syntax_error(m_token.lineno(), "expected token %d, got token %d\n",
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


/* The *_data_object routines here are auxiliary functions for use in the
 * recursive descent grammar to manage identifiers in the symbol table. */

/* Add a data object to the symbol table represented by the given type and
 * identifier tokens. The symbol table entry will be initialized from the
 * data_object_record parameter. */
void parser::add_data_object_to_symtab (const token& id, const data_object_record& record) {
    auto result = m_symtab.insert(id.lexeme(), record);

    if (false == result.second) {
        m_good = false;
        semantic_error(id.lineno(), "redeclaration of '%s'\n", id.lexeme().c_str());
    }
}

/* Check to make sure that an identifier exists in the symbol table. Does not
 * perform any action beyond this semantic check. */
void parser::get_referenced_data_object (const token& id, data_object_record& exprec) {
    auto it = m_symtab.find_in_active_scopes(id.lexeme());

    if (m_symtab.end() == it) {
        m_good = false;
        semantic_error(id.lineno(), "use of undeclared identifier '%s'\n",
                id.lexeme().c_str());
    }
    else {
        /* Load the data_object_record from the symbol table. */
        exprec = it->second;
    }
}

/* Create a data_object_record that represents an object on the stack. */
data_object_record parser::make_data_object_on_stack (bada_type type, bool is_constant) {
    data_object_record rec;

    rec.is_constant = is_constant;
    rec.type = type;
    rec.location = m_next_location;

    m_next_location -= WORD_SIZE;

    return rec;
}

/* Create a data_object_record that represents a constant object on the stack. */
data_object_record parser::make_constant_data_object_on_stack (bada_type type) {
    return make_data_object_on_stack(type, true);
}


//////////////////////////////////////////////////////////////////////////////


/* Generate the code for program startup. */
void parser::codegen_preamble () {
    if (good()) {
        codegen_raw("# Begin preamble");
        codegen_raw(".text");
        codegen_raw(".globl main");
        codegen_raw("main:");
        codegen("move", "$fp", "$sp");
        codegen("la", "$a0", "ProgStart");
        codegen("li", "$v0", "4");
        codegen("syscall");
        codegen_raw("# Begin translated code");
    }
}

/* Generate the code for program shutdown. */
void parser::codegen_postamble () {
    if (good()) {
        codegen_raw("# Begin postamble");
        codegen("la", "$a0", "ProgEnd");
        codegen("li", "$v0", "4");
        codegen("syscall");
        codegen("li", "$v0", "10");
        codegen("syscall");
        codegen_raw(".data");
        codegen_raw("ProgStart:\t.asciiz\t\"Program Start\\n\"");
        codegen_raw("ProgEnd:\t.asciiz\t\"Program End\\n\"");
    }
}

/* Generate one line of MIPS code, unindented, tab-delimited. */
void parser::codegen_raw (std::string arg1) {
    if (good()) {
        m_output << arg1 << '\n';
    }
}

/* Generate one line of MIPS code, unindented, tab-delimited. */
void parser::codegen_raw (std::string arg1, std::string arg2) {
    if (good()) {
        m_output << arg1 << '\t' << arg2 << '\n';
    }
}

/* Generate one line of MIPS code, unindented, tab-delimited. */
void parser::codegen_raw (std::string arg1, std::string arg2, std::string arg3) {
    if (good()) {
        m_output << arg1 << '\t' << arg2 << '\t' << arg3 << '\n';
    }
}

/* Generate one line of MIPS code, indented, tab after instr, commas between
 * arguments. */
void parser::codegen (std::string instr) {
    if (good()) {
        m_output << '\t' << instr << '\n';
    }
}

/* Generate one line of MIPS code, indented, tab after instr, commas between
 * arguments. */
void parser::codegen (std::string instr, std::string arg1) {
    if (good()) {
        m_output << '\t' << instr << '\t' << arg1 << '\n';
    }
}

/* Generate one line of MIPS code, indented, tab after instr, commas between
 * arguments. */
void parser::codegen (std::string instr, std::string arg1, std::string arg2) {
    if (good()) {
        m_output << '\t' << instr << '\t' << arg1 << ',' << arg2 << '\n';
    }
}

/* Generate one line of MIPS code, indented, tab after instr, commas between
 * arguments. */
void parser::codegen (std::string instr, std::string arg1, std::string arg2, std::string arg3) {
    if (good()) {
        m_output << '\t' << instr << '\t' << arg1 << ',' << arg2 << ',' << arg3 << '\n';
    }
}

void parser::codegen_op (const token& op,
        data_object_record dest,
        data_object_record lhs,
        data_object_record rhs) {
    bool type_op_mismatch = false;

    if (bada_type::integer == dest.type) {
        codegen("lw", "$t0", std::to_string(lhs.location) + "($fp)");
        codegen("lw", "$t1", std::to_string(rhs.location) + "($fp)");

        if (!op.lexeme().compare(ADDOP_ADD)) {
            codegen("add", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(ADDOP_SUBTRACT)) {
            codegen("sub", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(MULOP_MULTIPLY)) {
            codegen("mul", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(MULOP_DIVIDE)) {
            codegen("div", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(MULOP_MOD)) {
            codegen("rem", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(RELOP_LT)) {
            codegen("slt", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(RELOP_GT)) {
            codegen("sgt", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(RELOP_EQ)) {
            codegen("seq", "$t0", "$t0", "$t1");
        }
        else {
            type_op_mismatch = true;
        }

        codegen("sw", "$t0", std::to_string(dest.location) + "($fp)");
    }
    else if (bada_type::boolean == dest.type) {
        codegen("lw", "$t0", std::to_string(lhs.location) + "($fp)");
        codegen("lw", "$t1", std::to_string(rhs.location) + "($fp)");

        if (!op.lexeme().compare(ADDOP_OR)) {
            codegen("or", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(MULOP_AND)) {
            codegen("and", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(RELOP_LT)) {
            codegen("slt", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(RELOP_GT)) {
            codegen("sgt", "$t0", "$t0", "$t1");
        }
        else if (!op.lexeme().compare(RELOP_EQ)) {
            codegen("seq", "$t0", "$t0", "$t1");
        }
        else {
            type_op_mismatch = true;
        }

        codegen("sw", "$t0", std::to_string(dest.location) + "($fp)");
    }
    else if (bada_type::real == dest.type) {
        /* TODO implement */
        assert(false);
        {
            type_op_mismatch = true;
        }
    }
    else {
        /* never reached */
        assert(false);
    }

    if (type_op_mismatch) {
        semantic_error(op.lineno(), "operator %s used with incompatible type", op.lexeme().c_str());
        m_good = false;
    }
}

void parser::initialize_data_object (const data_object_record& record, const token& lit) {
    if (bada_type::integer == record.type) {
        codegen("li", "$t0", lit.lexeme().c_str());
        codegen("sw", "$t0", std::to_string(record.location) + "($fp)");
    }
    else if (bada_type::boolean == record.type) {
        if (!lit.lexeme().compare(KWD_TRUE)) {
            codegen("li", "$t0", "1");
        }
        else if (!lit.lexeme().compare(KWD_FALSE)) {
            codegen("li", "$t0", "0");
        }
        else {
            /* never reached */
            assert(false);
        }

        codegen("sw", "$t0", std::to_string(record.location) + "($fp)");
    }
    else if (bada_type::real == record.type) {
        /* TODO implement */
        assert(false);
    }
    else {
        /* never reached */
        assert(false);
    }
}
  

//////////////////////////////////////////////////////////////////////////////


/* What follows is the grammar for Baby Ada, translated into mutually
 * recursive functions. For nullable rules, I return on any unexpected token,
 * not just the rule's FOLLOW set. There are no real surprises in the
 * functions, with each one following a fairly obvious pattern. */

/* program ::= PROCEDURE ID IS decls BEGIN stats END ID ';' */
void parser::program () {
    PRINTRULE(program);

    m_symtab.open_scope();

    match(token::procedure);
    /* Note that I'm in CS4110, so I'm not worrying about procedures, and not
     * putting this identifier into my symbol table (at least not yet). */
    match(token::identifier);
    match(token::is);
    decls();
    match(token::begin);
    stats();
    match(token::end);
    match(token::identifier);
    match(token::semicolon);

    m_symtab.close_scope();
}

/* stats ::= statmt stats | <empty> */
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

/* decls ::= decl decls | <empty> */
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

/* decl ::= ID ':' rest */
void parser::decl () {
    PRINTRULE(decl);

    /* Save the current token until <rest> finds out the type. */
    token declared_id = m_token;

    match(token::identifier);
    match(token::colon);
    rest(declared_id);
}

/* rest ::= TYPE ';' | CONSTANT TYPE ASSIGN LITERAL ';' */
void parser::rest (const token& declared_id) {
    if (predict(token::type)) {
        PRINTRULE(rest_type);

        /* Save the current token--we want to perform our semantic action
         * (adding the id to the symbol table) after completing this rule's
         * syntax checks. */
        token declared_type = m_token;

        match(token::type);
        match(token::semicolon);

        auto record = make_data_object_on_stack(keyword_to_type(declared_type.lexeme()));
        add_data_object_to_symtab(declared_id, record);
    }
    else if (predict(token::constant)) {
        PRINTRULE(rest_constant);

        match(token::constant);

        /* Save current token--see comment above. */
        token declared_type = m_token;

        match(token::type);
        match(token::assign);

        /* Save the literal token so we can load it into memory. */
        auto lit = m_token;

        match(token::literal);
        match(token::semicolon);

        auto record = make_constant_data_object_on_stack(keyword_to_type(declared_type.lexeme()));
        add_data_object_to_symtab(declared_id, record);
        initialize_data_object(record, lit);
    }
    else {
        syntax_error(m_token.lineno(), "expected token (%d | %d), got token %d\n",
                token::type, token::constant, m_token.get_token_id());
    }
}

/* statmt ::= assignstat | ifstat | readstat | writestat | blockst | loopst */
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
        syntax_error(m_token.lineno(), "expected token (%d | %d | %d | %d | %d | %d | %d), "
                "got token %d\n", token::identifier,
                token::if_, token::read, token::write, token::begin,
                token::declare, token::while_, m_token.get_token_id());
    }
}

/* assignstat ::= idnonterm ASSIGN express ';' */
void parser::assignstat () {
    PRINTRULE(assignstat);

    data_object_record lhs, rhs;
    int lineno = m_token.lineno();

    idnonterm(lhs);
    match(token::assign);
    express(rhs);
    match(token::semicolon);

    if (lhs.type != rhs.type) {
        semantic_error(lineno, "type mismatch\n");
        m_good = false;
    }
    
    if (lhs.is_constant) {
        semantic_error(lineno, "assignment to constant\n");
        m_good = false;
    }

    /* Copy the object from the right-hand-side to the left-hand-side. */
    codegen("# assignstat");
    codegen("lw", "$t0", std::to_string(rhs.location) + "($fp)");
    codegen("sw", "$t0", std::to_string(lhs.location) + "($fp)");
}

/* ifstat ::= IF express THEN stats END IF ';' */
void parser::ifstat () {
    PRINTRULE(ifstat);

    match(token::if_);
    /* TODO implement */
    data_object_record TODO;
    express(TODO);
    match(token::then);
    stats();
    match(token::end);
    match(token::if_);
    match(token::semicolon);
}

/* readstat ::= READ '(' idnonterm ')' ';' */
void parser::readstat () {
    PRINTRULE(readstat);

    data_object_record exprec;

    /* Save the line number for possible error reporting. */
    int lineno = m_token.lineno();

    match(token::read);
    match(token::lparen);
    idnonterm(exprec);
    match(token::rparen);
    match(token::semicolon);

    if (bada_type::integer == exprec.type) {
        /* Read an integer. */
        codegen("# readstat -- read an integer");
        codegen("li", "$v0", "5");
        codegen("syscall");
        codegen("sw", "$v0", std::to_string(exprec.location) + "($fp)");
    }
    else if (bada_type::real == exprec.type) {
        /* TODO implement */
        assert(false);
    }
    else if (bada_type::boolean == exprec.type) {
        /* TODO implement */
        assert(false);
    }
    else {
        semantic_error(lineno, "read statement argument has incompatible type");
        m_good = false;
    }
}

/* writestat ::= WRITE '(' writeexp ')' ';' */
void parser::writestat () {
    PRINTRULE(writestat);

    token writetok = m_token;

    match(token::write);
    match(token::lparen);
    writeexp();
    match(token::rparen);
    match(token::semicolon);

    if (!writetok.lexeme().compare(KWD_PUT_LINE)) {
        /* Print a newline character. */
        codegen("# writestat -- print newline");
        codegen("li", "$a0", "0x0a");
        codegen("li", "$v0", "11");
        codegen("syscall");
    }
}

/* WHILE express LOOP stats END LOOP ';' */
void parser::loopst () {
    PRINTRULE(loopst);

    match(token::while_);
    data_object_record TODO;
    express(TODO);
    match(token::loop);
    stats();
    match(token::end);
    match(token::loop);
    match(token::semicolon);
}

/* blockst ::= declpart BEGIN stats END ';' */
void parser::blockst () {
    PRINTRULE(blockst);

    m_symtab.open_scope();

    declpart();
    match(token::begin);
    stats();
    match(token::end);
    match(token::semicolon);

    m_symtab.close_scope();
}

/* declpart ::= DECLARE decl decls | <empty> */
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

/* writeexp ::= STRING | express */
void parser::writeexp () {
    if (predict(token::string)) {
        PRINTRULE(writeexp_string);

        token writestring = m_token;

        match(token::string);

        auto label = next_string_label();

        codegen("# writeexp -- write a string literal");

        /* Define the string literal in MIPS.*/
        codegen_raw(".data");
        codegen_raw(label + ':', ".asciiz", writestring.lexeme().c_str());
        codegen_raw(".text");

        /* Print the string literal. */
        codegen("la", "$a0", label);
        codegen("li", "$v0", "4");
        codegen("syscall");
    }
    else if (predict(first::express)) {
        PRINTRULE(writeexp_express);

        data_object_record exprec;

        express(exprec);

        if (bada_type::integer == exprec.type) {
            /* Write an integer. */
            codegen("# writeexp -- write an integer literal");
            codegen("lw", "$a0", std::to_string(exprec.location) + "($fp)");
            codegen("li", "$v0", "1");
            codegen("syscall");
        }
        else if (bada_type::real == exprec.type) {
            /* TODO implement */
            assert(false);
        }
        else if (bada_type::boolean == exprec.type) {
            /* TODO implement */
            assert(false);
        }
        else {
            assert(false);
        }
    }
    else {
        syntax_error(m_token.lineno(), "expected token (%d | %d | %d | %d | %d | %d), "
                "got token %d\n", token::string,
                token::not_, token::identifier, token::literal,
                token::lparen, m_token.get_token_id());
    }
}

/* express ::= term expprime */
void parser::express (data_object_record& exprec) {
    PRINTRULE(express);

    term(exprec);
    expprime(exprec);
}

/* expprime ::= ADDOP term expprime | <empty> */
void parser::expprime (data_object_record& exprec) {
    if (predict(token::addop)) {
        PRINTRULE(expprime);

        /* Save the expression record of the left hand side of the op. */
        auto lhs = exprec;

        /* Save the operation to perform. */
        token operation = m_token;

        match(token::addop);
        term(exprec);

        auto rhs = exprec;
        if (rhs.type != lhs.type) {
            semantic_error(operation.lineno(), "add-expression uses incompatible types");
            m_good = false;
        }

        exprec = make_data_object_on_stack(exprec.type);

        /* exprec is now the destination of the result of an add operation
         * on lhs and rhs. */
        codegen_op(operation, exprec, lhs, rhs);

        expprime(exprec);
    }
    else {
        PRINTRULE(expprime_null);
    }
}

/* term ::= relfactor termprime */
void parser::term (data_object_record& exprec) {
    PRINTRULE(term);

    relfactor(exprec);
    termprime(exprec);
}

/* termprime ::= MULOP relfactor termprime | <empty> */
void parser::termprime (data_object_record& exprec) {
    if (predict(token::mulop)) {
        PRINTRULE(termprime);

        /* Save the expression record of the left hand side of the op. */
        auto lhs = exprec;

        /* Save the operation to perform. */
        token operation = m_token;

        match(token::mulop);
        relfactor(exprec);

        auto rhs = exprec;
        if (rhs.type != lhs.type) {
            semantic_error(operation.lineno(), "mul-expression uses incompatible types");
            m_good = false;
        }

        exprec = make_data_object_on_stack(exprec.type);

        codegen_op(operation, exprec, lhs, rhs);

        termprime(lhs);
    }
    else {
        PRINTRULE(termprime_null);
    }
}

/* relfactor ::= factor factorprime */
void parser::relfactor (data_object_record& exprec) {
    PRINTRULE(relfactor);

    factor(exprec);
    factorprime(exprec);
}

/* factorprime ::= RELOP factor | <empty> */
void parser::factorprime (data_object_record& exprec) {
    if (predict(token::relop)) {
        PRINTRULE(factorprime);

        /* Save the expression record of the left hand side of the op. */
        data_object_record lhs = exprec;

        /* Save the operation to perform. */
        token operation = m_token;

        match(token::relop);
        factor(exprec);

        auto rhs = exprec;
        if (rhs.type != lhs.type) {
            semantic_error(operation.lineno(), "rel-expression uses incompatible types");
            m_good = false;
        }

        exprec = make_data_object_on_stack(exprec.type);

        codegen_op(operation, exprec, lhs, rhs);
    }
    else {
        PRINTRULE(factorprime_null);
    }
}

/* factor ::= NOT factor | idnonterm | LITERAL | '(' express ')' */
void parser::factor (data_object_record& exprec) {
    if (predict(token::not_)) {
        PRINTRULE(factor_not);

        int lineno = m_token.lineno();

        match(token::not_);
        factor(exprec);

        auto operand = exprec;
        if (bada_type::boolean != operand.type) {
            semantic_error(lineno, "operator not applied to non-boolean");
            m_good = false;
        }

        exprec = make_data_object_on_stack(exprec.type);

        codegen("lw", "$t0", std::to_string(operand.location) + "($fp)");
        codegen("not", "$t0", "$t0");
        codegen("sw", "$t0", std::to_string(exprec.location) + "($fp)");
    }
    else if (predict(token::identifier)) {
        PRINTRULE(factor_id);

        idnonterm(exprec);
    }
    else if (predict(token::literal)) {
        PRINTRULE(factor_literal);

        token lit = m_token;
        match(token::literal);

        /* Allocate a place on the stack for this literal value. */
        exprec = make_constant_data_object_on_stack(literal_to_type(lit.lexeme()));
        initialize_data_object(exprec, lit);
    }
    else if (predict(token::lparen)) {
        PRINTRULE(factor_express);

        match(token::lparen);
        express(exprec);
        match(token::rparen);
    }
    else {
        syntax_error(m_token.lineno(), "expected token (%d | %d | %d | %d), got token %d\n",
                token::not_, token::identifier, token::literal, token::lparen,
                m_token.get_token_id());
    }
}

/* idnonterm ::= ID */
void parser::idnonterm (data_object_record& exprec) {
    PRINTRULE(idnonterm);

    /* Save current token for later semantic action. */
    token referenced_id = m_token;

    match(token::identifier);

    /* Load the type and location of this identifier's referent. */
    get_referenced_data_object(referenced_id, exprec);
}
