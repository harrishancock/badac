/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Symbol Table Assignment (8 October 2013)
 *
 * scoped_symbol_table.hpp
 *
 * Implementation of a symbol table and scope manager. In some places,
 * implementation is abbreviated for clarity. For example, I only have a
 * non-const version of scoped_symbol_table::begin(), because I only
 * use the non-const version for this assignment. Similarly, I only have a
 * const version of scoped_symbol_table::begin(scope_id), because I
 * only use the const version for this assignment.
 */

#ifndef SCOPED_SYMBOL_TABLE_HPP
#define SCOPED_SYMBOL_TABLE_HPP

#include "splaytree.hpp"
#include "bada.hpp"

#include <deque>

/* A scope_id is a numeric value which uniquely identifies a lexical scope. */
using scope_id = unsigned;
using identifier = ci_string;

/* We'll be using the one-giant-symbol-table strategy for scope management,
 * with an underlying data structure which supports a std::map-like interface.
 * For this associative container, we can use a 2-tuple of a scope_id and the
 * identifier string itself as the key. operator< is overloaded for std::pair
 * already, ensuring that the scope_ids will be compared first, and then the
 * identifier strings. This has the effect that we can easily select entire
 * scopes as iterable ranges by calling map::lower_bound on an id_key with an
 * empty identifier string. The first lower_bound call would have the scope_id
 * in question, and return a begin iterator; the second lower_bound call would
 * have the increment of the scope_id in question, and return an end iterator.
 */
using id_key = std::pair<scope_id, identifier>;

/* The underlying data structure that will hold our identifier records. Any
 * container which provides a std::map-like interface will do. Note that if we
 * used a hash table (such as std::unordered_map), we would need to write a
 * hash routine as well. */
using symbol_table = splaytree::map<id_key, data_object_record>;

/* Unified class that handles symbol table and scope management. */
class scoped_symbol_table {
public:
    /* These typedefs break encapsulation in a minor way: with an iterator to
     * a symbol in one scope, I could increment the iterator until I found
     * symbols in a different scope (since we're using the one-giant-symbol-
     * table strategy). This doesn't bother me too much, but I may redesign
     * this class in the future to be more defensive. */
    using iterator = symbol_table::iterator;
    using const_iterator = symbol_table::const_iterator;

    /* Open a new scope and push it onto the active scope stack. All future
     * insertions will use this new scope, until close_scope() is called. */
    void open_scope () {
        m_active_scopes.push_front(m_next_scope_id++);

        /* If we wrap around, something probably went wrong. */
        assert(m_next_scope_id);
    }

    /* Close the current scope. All future insertions will use the scope which
     * was previously open, if any. No symbols are erased from the symbol
     * table. */
    void close_scope () {
        assert(!m_active_scopes.empty());

        m_active_scopes.pop_front();
    }

    /* Get an iterator to the first symbol in the first scope. */
    iterator begin () {
        return m_symbol_table.begin();
    }

    /* Get an iterator to the first symbol in scope sid. */
    const_iterator begin (scope_id scope) const {
        auto key = std::make_pair(scope, identifier());
        return m_symbol_table.lower_bound(key);
    }

    /* Get an iterator to one past the last symbol in the last scope. */
    iterator end () {
        return m_symbol_table.end();
    }

    /* Get an iterator to one past the last symbol in scope sid. */
    const_iterator end (scope_id scope) const {
        auto key = std::make_pair(scope + 1, identifier());
        return m_symbol_table.lower_bound(key);
    }

    /* Insert identifier id into the symbol table in the currently active
     * scope. Returns an iterator to the newly-created element, or the
     * previously existing element, and a boolean signifying whether or not
     * an insertion actually took place (true == insertion succeeded). */
    std::pair<iterator, bool> insert (const identifier& id, const data_object_record& record) {
        assert(!m_active_scopes.empty());

        auto key = std::make_pair(m_active_scopes.front(), id);
        auto value = std::make_pair(key, record);
        return m_symbol_table.insert(value);
    }

    /* Search through each active scope until we find a match for this
     * identifier. If no match, return a one-past-the-end iterator.
     *
     * Note that since this uses only the active scopes, it may differ from
     * the intended meaning of the FIND routine in the assignment. If FIND is
     * meant to find an identifier in a specific scope, see the two-parameter
     * overload of find(). */
    iterator find_in_active_scopes (const identifier& id) {
        for (auto scope : m_active_scopes) {
            auto it = find(scope, id);
            if (m_symbol_table.end() != it) {
                return it;
            }
        }

        /* Exhausted our active scopes, return end() to signify not found. */
        return end();
    }

    /* Search for an identifier in a specific scope. */
    iterator find (const scope_id scope, const identifier& id) {
        auto key = std::make_pair(scope, id);
        return m_symbol_table.find(key);
    }

    /* Dump the entire symbol table (all scopes) to a given output stream. */
    void display (std::ostream& output = std::cout) const {
        static const size_t cols = 78;
        static const std::string title { "SYMBOL TABLE" };

        /* Print the title, surrounded by equals signs. */
        const auto num_equals = cols - title.length() - 2;
        for (size_t i = 0; i < num_equals / 2; ++i) {
            output << '=';
        }
        output << ' ' << title << ' ';
        for (size_t i = 0; i < num_equals / 2; ++i) {
            output << '=';
        }
        /* The two for loops above use integer division, which introduces a
         * parity issue if the number of equals signs we wanted to print was
         * odd. Account for this. */
        if (1 & num_equals) {
            output << '=';
        }
        output << '\n';

        /* And finally print the scopes, in order. */
        for (scope_id scope = 0; scope < m_next_scope_id; ++scope) {
            output << "Scope " << scope << ":\n";
            for (auto it = begin(scope); end(scope) != it; ++it) {
                auto& lexeme = it->first.second;
                output << '\t' << lexeme << ": ";
                it->second.display(output);
            }
        }
    }

private:
    scope_id m_next_scope_id = 0;

    /* std::stack would be a more logical choice for the active scope stack,
     * but it does not support iteration, which we need. std::deque does.
     * Another potential choice might be std::forward_list, whose interface is
     * a bit leaner, but for scope_ids, the memory allocation overhead of a
     * linked list is overkill. */
    std::deque<scope_id> m_active_scopes;
    symbol_table m_symbol_table;
};

#endif
