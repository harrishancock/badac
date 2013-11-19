/*
 * California State University East Bay
 * CS4110 - Compiler Design
 * Author: Harris Hancock (hhancock 'at' horizon)
 *
 * Symbol Table Assignment (8 October 2013)
 *
 * splaytree.hpp
 *
 * Implementation of a splay tree data structure. A splay tree is a type of
 * self-adjusting binary search tree. The salient feature with regards to its
 * use as a symbol table is that it exhibits a natural caching tendency: the
 * most often accessed elements stay close to the top of the tree. For a
 * better overview, see the Wikipedia article on splay trees, or the original
 * paper by Sleator and Tarjan:
 * http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf
 *
 * I chose to implement my own data structure for the symbol table because it
 * sounded like more fun than just using std::unordered_map. I chose to
 * implement a tree specifically instead of a hash table on a whim; I am
 * curious how performant this container will turn out to be when it gets
 * loaded down.
 *
 * Unlike symbol_table.hpp, I did not omit parts of the implementation for
 * clarity in this file. The reason for this is because many parts of the
 * implementation are interconnected with others, and also because removing
 * some parts but not others results in a source file that doesn't make much
 * sense. For example, the test program for this assignment only uses one
 * insert() method, so I could get rid of all the others. However, since the
 * splaytree template holds resources, it needs a destructor, and since it has
 * a destructor, it needs a copy constructor (by the "Rule of Five"). The copy
 * constructor has to be able to insert the other object's elements, which
 * means I actually do need a two-argument, range-based insert() method.
 * Navigating through all such dependencies in the interface such that the end
 * result is a stripped-down version that only provides the features required
 * for this current assignment, AND still makes some sort of sense to a C++
 * programmer reading the code seems like a lot of effort for little gain. You
 * should be able to safely skim the code in this file: it implements a splay
 * tree, it implements a std::map-like interface, and the test driver for the
 * symbol table assignment works fine.
 *
 * Since this is a templated container, its implementation is header-only.
 * Some work could be offloaded into a separate .cpp file, particularly from
 * the node class--I'll get to this when I have time. Additionally, I should
 * point out that I wanted to support both a std::set-like interface and a
 * std::map-like interface using the same underlying tree implementation. I
 * jumped through some hoops to do this with a minimum of code duplication.
 * This is what the set_base and map_base templates are all about. I fear that
 * this negatively impacted the readability of the code somewhat.
 *
 * I use some conventions for short identifier names. In binary functions, lhs
 * and rhs signify left-hand-side, and right-hand-side, respectively. In the
 * lowest levels of the tree implementation, I use the identifiers s and p
 * gratuitously. s usually means self, or start, and is the node that the
 * routine is currently considering. p means pointer, and is just used to save
 * a node pointer for later use.
 */

#ifndef SPLAYTREE_HPP
#define SPLAYTREE_HPP

#include <cassert>
#include <cstddef>

#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <stdexcept>
#include <type_traits>

/* iostream is only included for debugging (splaytree::dump_structure()) */
#include <iostream>

namespace splaytree {

template <typename Base>
class splaytree;

namespace detail {

//////////////////////////////////////////////////////////////////////////////

/* A node in a splaytree. This class has two levels of implementation:
 * operations on a single node, such as attach, rotate, and splay; and
 * operations on the tree as a whole, such as search, erase, increment, and
 * lower_bound. In general, the former operations are implemented as member
 * functions of node, while the latter are implemented as static member
 * functions that operate on pointers to node.
 *
 * The template parameter, T, is the user-specified value type stored in every
 * node of the tree.
 *
 * TODO separate the linkage (parent, left, right pointers) from the node
 * class and put it in a separate node_base class, from which node derives.
 * Implement as many tree operations as possible in terms of this node_base
 * class (i.e., not in a header file). This will also be necessary in order to
 * support bidirectional iteration. */
template <typename T>
class node {
public:
    using value_type = T;

    explicit node (const value_type& value)
            : m_value(value) { }

    template <typename... Args>
    explicit node (Args&&... args)
            : m_value(std::forward<Args>(args)...) { }

    ~node () {
        delete left();
        delete right();
    }

    /* Attach a child. We must not already have a child in that position,
     * and it must must not already have a parent. */
    void attach_left (node* other) { attach<LEFT>(other); }
    void attach_right (node* other) { attach<RIGHT>(other); }

    /* Detach a left child and return it. Returns nullptr if there is no left
     * child. */
    node* detach_left () { return detach<LEFT>(); }
    node* detach_right () { return detach<RIGHT>(); }

    value_type& value () {
        return m_value;
    }

    bool is_root () {
        return !m_parent;
    }

    bool is_left_child () {
        return m_parent && m_parent->left() == this;
    }

    /* Return a pointer to the smallest value in the tree s. Does NOT modify
     * the tree. */
    static node* minimum (node* s) {
        if (s) while (s->left()) {
            s = s->left();
        }
        return s;
    }

    /* Return a pointer to the largest value in the tree s. Does NOT modify
     * the tree. */
    static node* maximum (node* s) {
        if (s) while (s->right()) {
            s = s->right();
        }
        return s;
    }

    /* Descend the tree s, searching for the given value with the given
     * comparison function. Return a pointer to the found element, or the node
     * to which the value in question would have been attached, if not found.
     * Does NOT modify the tree. */
    template <typename Compare>
    static node* search_no_splay (node* s, const value_type& value, const Compare& comp) {
        node* p = nullptr;

        while (s) {
            p = s;
            if (comp(value, s->m_value)) {
                s = s->left();
            }
            else if (comp(s->m_value, value)) {
                s = s->right();
            }
            else {
                break;
            }
        }

        return p;
    }

    /* Same as search_no_splay, except the return value is also the new root
     * of the tree. DOES modify the tree. */
    template <typename Compare>
    static node* search (node* s, const value_type& value, const Compare& comp) {
        s = search_no_splay(s, value, comp);

        if (s) {
            s->splay();
        }

        return s;
    }

    /* Join two roots into a single tree. The new root will be the largest
     * element in the left-hand tree. If the left-hand tree is null, the new
     * root will be the right-hand tree. */
    static node* join (node* lhs, node* rhs) {
        if (!lhs) {
            return rhs;
        }
        if (!rhs) {
            return lhs;
        }

        assert(lhs->is_root());
        assert(rhs->is_root());

        /* Find the largest element of the left-hand tree. */
        auto root = maximum(lhs);
        root->splay();
        root->attach_right(rhs);

        return root;
    }

    /* Remove and delete the given node from its tree. Return a pointer to the
     * new root of the tree. DOES modify the tree. */
    static node* erase (node* s) {
        assert(s);
        s->splay();
        auto lhs = s->detach_left();
        auto rhs = s->detach_right();
        delete s;
        s = nullptr;
        return join(lhs, rhs);
    }

    /* Traverse the tree to the next node, in-order. Does NOT modify the tree.
     * FIXME some code duplication with decrement() here. */
    static node* increment (node* s) {
        if (!s) {
            return nullptr;
        }

        if (s->right()) {
            return minimum(s->right());
        }

        /* Climb up the tree as long as s is a right child. In other words:
         * stop when s is a left child OR the root. */
        while (s->m_parent && !s->is_left_child()) {
            s = s->m_parent;
        }

        return s->m_parent;
    }

    /* Traverse the tree to the previous node, in-order. Does NOT modify the
     * tree. */
    static node* decrement (node* s) {
        if (!s) {
            return nullptr;
        }

        if (s->left()) {
            return maximum(s->left());
        }

        /* Climb up the tree as long as s is a left child. In other words:
         * stop when s is a right child OR the root. */
        while (s->m_parent && s->is_left_child()) {
            s = s->m_parent;
        }

        return s->m_parent;
    }

    /* Return a pointer to the smallest node whose key is greater than or
     * equal to the given value. Does NOT modify the tree. */
    template <typename Compare>
    static node* lower_bound_no_splay (node* s, const value_type& value, const Compare& comp) {
        s = search_no_splay(s, value, comp);

        if (!s) {
            return nullptr;
        }

        /* If this node is less than the search key, then the search key was
         * not found. The next node must be the first node that is not less
         * than the search key. */
        if (comp(s->value(), value)) {
            return increment(s);
        }

        /* This node is greater than or equal to the search key. Decrement
         * until we find the first node that is not less than the search key.
         */
        auto p = s;
        while (p && !comp(p->value(), value)) {
            s = p;
            p = decrement(p);
        }

        return s;
    }

    /* Same as lower_bound_no_splay, except the return value is also the new
     * root of the tree, IF it is not null. If it is null, this simply means
     * that the lower bound is past the end of the tree. DOES modify the tree.
     */
    template <typename Compare>
    static node* lower_bound (node* s, const value_type& value, const Compare& comp) {
        s = lower_bound_no_splay(s, value, comp);

        if (s) {
            s->splay();
        }

        return s;
    }

    /* Return a pointer to the smallest node whose key is greater than the 
     * given value. Does NOT modify the tree. */
    template <typename Compare>
    static node* upper_bound_no_splay (node* s, const value_type& value, const Compare& comp) {
        s = search_no_splay(s, value, comp);

        if (!s) {
            return nullptr;
        }

        if (comp(s->value(), value)) {
            return increment(s);
        }

        /* This node is greater than or equal to the search key. Increment
         * until we find a node that is strictly greater. */
        while (s && !comp(value, s->value())) {
            s = increment(s);
        }

        return s;
    }

    /* Same as upper_bound_no_splay, except the return value is also the new
     * root of the tree, IF it is not null. If it is null, this simply means
     * that the upper bound is past the end of the tree. DOES modify the tree. */
    template <typename Compare>
    static node* upper_bound (node* s, const value_type& value, const Compare& comp) {
        s = upper_bound_no_splay(s, value, comp);

        if (s) {
            s->splay();
        }

        return s;
    }

    /* Dump out an adjacency list of the tree. */
    void dump_structure () {
        std::cout << m_value << " | ";
        if (left()) {
            std::cout << left()->value() << ' ';
        }
        else {
            std::cout << "(nil) ";
        }

        if (right()) {
            std::cout << right()->value();
        }
        else {
            std::cout << "(nil)";
        }
        std::cout << '\n';

        if (left()) {
            left()->dump_structure();
        }
        if (right()) {
            right()->dump_structure();
        }
    }

private:
    /* Use like so: std::get<LEFT>(m_children) = ... */
    enum child_tag { LEFT, RIGHT };

    template <child_tag Child>
    void attach (node* other) {
        assert(!get<Child>());
        get<Child>() = other;

        if (get<Child>()) {
            assert(!get<Child>()->m_parent);
            get<Child>()->m_parent = this;
        }
    }

    template <child_tag Child>
    node* detach () {
        auto other = get<Child>();
        get<Child>() = nullptr;
        if (other) {
            assert(other->m_parent == this);
            other->m_parent = nullptr;
        }
        return other;
    }

    /* Rotate this node so that it becomes the parent of its rotation-side
     * child. For example, for a right rotation:
     *      y             x
     *     / \           / \
     *    x   C   ==>   A   y
     *   / \               / \
     *  A   B             B   C
     * where x, y are splay tree nodes; A, B, C are splay subtrees.
     *
     * In the code below, RS = rotation-side, OS = other-side.
     */
    template <child_tag RS>
    void rotate () {
        constexpr static const child_tag OS
            = RS == LEFT ? RIGHT : LEFT;

        assert(m_parent);
        assert(m_parent->get<OS>() == this);

        if (get<RS>()) {
            get<RS>()->m_parent = m_parent;
        }
        m_parent->get<OS>() = get<RS>();
        get<RS>() = m_parent;

        auto gparent = m_parent->m_parent;
        if (gparent) {
            (gparent->left() == m_parent ?
             gparent->left() : gparent->right()) = this;
        }
        m_parent = gparent;
        get<RS>()->m_parent = this;
    }

    /* Any given node can only rotate in one direction (or not at all, for the
     * root node), so wrap the rotate<LEFT> and rotate<RIGHT> calls. */
    void rotate () {
        assert(!this->is_root());

        if (this->is_left_child()) {
            rotate<RIGHT>();
        }
        else {
            rotate<LEFT>();
        }
    }

    /* Move this node into the root position, while maintaining the symmetric
     * order of the tree as a whole. */
    void splay () {
        while (!this->is_root()) {
            if (!m_parent->is_root()) {
                if (this->is_left_child() == m_parent->is_left_child()) {
                    m_parent->rotate();
                }
                else {
                    rotate();
                }
            }
            rotate();
        }
    }

    /* Access this node's left child. */
    node*& left () {
        return get<LEFT>();
    }

    /* Access this node's right child. */
    node*& right () {
        return get<RIGHT>();
    }

    template <child_tag Child>
    node*& get () {
        return std::get<Child>(m_children);
    }

    value_type m_value;
    
    node* m_parent = nullptr;

    /* m_children's pointers will be default-constructed to nullptr by
     * std::pair's constructor. */
    std::pair<node*, node*> m_children;
};

//////////////////////////////////////////////////////////////////////////////

/* iterator and const_iterator both use a shared implementation template,
 * iterator_tpl. */
template <typename T, typename Base>
struct iterator_tpl;

/* TODO my splaytree iterators only support forward iteration. Bidirectional
 * iteration is possible, but would require some refactoring of node to
 * accomplish. */
template <typename T>
using iterator = iterator_tpl<T, std::iterator<std::forward_iterator_tag, T>>;

template <typename T>
using const_iterator = iterator_tpl<T, std::iterator<std::forward_iterator_tag,
      typename std::add_const<T>::type>>;

template <typename T, typename Base>
struct iterator_tpl : Base {
    using node_type = node<T>;
    
    using base_type = Base;

    using reference = typename base_type::reference;
    using pointer = typename base_type::pointer;

    explicit iterator_tpl (node_type* node = nullptr) : m_node(node) { }

    /* We need to be able to implicitly convert an iterator into a
     * const_iterator. */
    iterator_tpl (const iterator<T>& other) : m_node(other.m_node) { }

    bool operator== (const iterator_tpl& other) const {
        return m_node == other.m_node;
    }

    bool operator!= (const iterator_tpl& other) const {
        return m_node != other.m_node;
    }

    reference operator* () { return m_node->value(); }
    pointer operator-> () { return &m_node->value(); }

    iterator_tpl& operator++ () {
        m_node = node_type::increment(m_node);
        return *this;
    }

    /* Postfix */
    iterator_tpl operator++ (int) {
        auto ret = *this;
        ++*this;
        return ret;
    }

    node_type* m_node;
};

//////////////////////////////////////////////////////////////////////////////

/* Base class for using a splaytree as a set. */
template <typename T, typename Compare>
struct set_base {
    /* Derived is not actually used for set's base class, and is only provided
     * for symmetry with map_base. */
    template <typename Derived>
    struct base {
        using key_type = T;
        using key_compare = Compare;
        using value_type = key_type;
        using value_compare = key_compare;

        /* const_iterator is the only iterator implemented, because a splaytree
         * is a model of a set: the key and the value are the same, and
         * modifying the key of an element of an associative container is
         * stupid (without removing and then reinserting the element). For
         * example, take the following splaytree, s, with elements stored in
         * nondecreasing order:
         *    2
         *   /
         *  1
         * With a mutable iterator, the expression "*s.begin() = 3;" would be
         * valid, but it would put the tree into an invalid state:
         *    2
         *   /
         *  3
         * Best to avoid such horrors--if mutability is required, the user can
         * use const_cast. */
        using iterator = detail::const_iterator<value_type>;
        using const_iterator = detail::const_iterator<value_type>;

    protected:
        /* In a set, the key and the value are one and the same, so making a
         * default value from a key is as simple as returning the key. */
        static value_type make_value (const key_type& key) { return key; }

        static value_type make_value (key_type&& key) {
            return std::forward<key_type>(key);
        }
    };
};

/* Base class for using a splaytree as a map. */
template <typename Key, typename T, typename Compare>
struct map_base {
    template <typename Derived>
    struct base {
        using key_type = Key;
        using key_compare = Compare;
        using mapped_type = T;
        using value_type = std::pair<typename std::add_const<Key>::type, T>;

        /* Class to create function objects which can compare the keys
         * embedded inside of two objects of type value_type. */
        struct value_compare : std::binary_function<value_type, value_type, bool> {
            /* Allow splaytree, our derived class, to construct this object. */
            friend class splaytree<map_base>;

            /* Return true if lhs's key is less than rhs's key. */
            bool operator() (const value_type& lhs, const value_type& rhs) const {
                return m_comp(lhs.first, rhs.first);
            }

        protected:
            /* Get the underlying key_compare object from this value_compare
             * object.
             *
             * This is abuse of the conversion operator, but it saves some
             * metaprogramming down below to implement splaytree::key_comp(). */
            explicit operator key_compare () const { return m_comp; }

            explicit value_compare (const Compare& comp = Compare())
                    : m_comp(comp) { }

        private:
            key_compare m_comp;
        };

        /* A map can have mutable iterators, because they point to a
         * std::pair<const Key, T>. That is, the key is still const, and only the
         * mapped type may be changed. */
        using iterator = detail::iterator<value_type>;
        using const_iterator = detail::const_iterator<value_type>;

        /* The at() function and operator[] implemented here use the
         * Curiously-Recurring Template Pattern to call down into the
         * splaytree implementation. They're implemented here instead of in
         * splaytree, because they would not make sense in a set interface. */

        /* Get a reference to the element at the given key, throwing
         * std::out_of_range if the element does not exist. */
        mapped_type& at (const key_type& key) {
            auto self = static_cast<Derived*>(this);

            auto elem = self->find(key);
            if (self->end() == elem) {
                throw std::out_of_range("element not in tree");
            }
            return elem->second;
        }

        /* No const version of at(), because it relies on find(), which is
         * non-const. TODO maybe provide a const version of find()? */

        /* Get a reference to the element at the given key, inserting it if it
         * does not already exist. */
        mapped_type& operator[] (const key_type& key) {
            auto self = static_cast<Derived*>(this);

            auto elem = self->find(key);
            if (self->end() == elem) {
                bool success;
                std::tie(elem, success) = self->insert(make_value(key));
                assert(success);
            }
            return elem->second;
        }

        /* Get a reference to the element at the given key, inserting it if it
         * does not already exist. */
        mapped_type& operator[] (key_type&& key) {
            auto self = static_cast<Derived*>(this);

            auto elem = self->find(key);
            if (self->end() == elem) {
                bool success;
                std::tie(elem, success)
                    = self->insert(make_value(std::forward<key_type>(key)));
                assert(success);
            }
            return elem->second;
        }

    protected:
        static value_type make_value (const key_type& key) {
            return std::make_pair(key, mapped_type());
        }

        static value_type make_value (key_type&& key) {
            return std::make_pair(std::forward<key_type>(key), mapped_type());
        }
    };
};

} // namespace detail

//////////////////////////////////////////////////////////////////////////////

/* Main implementation of a splaytree-based container. Inherits from a base
 * class to get its configuration, and any pieces of interface uncommon to all
 * containers that it can emulate. */
template <typename Base>
class splaytree : public Base::template base<splaytree<Base>> {
public:
    using base_type = typename Base::template base<splaytree>;

    using key_type = typename base_type::key_type;
    using key_compare = typename base_type::key_compare;
    using value_type = typename base_type::value_type;
    using value_compare = typename base_type::value_compare;

    using reference = value_type&;
    using const_reference = const value_type&;

    using node_type = detail::node<value_type>;

    using iterator = typename base_type::iterator;
    using const_iterator = typename base_type::const_iterator;

    using difference_type = ptrdiff_t;
    using size_type = size_t;

    /* Default constructor */
    explicit splaytree (const key_compare& comp = key_compare())
            : m_comp(comp)
            , m_size(0)
            , m_root(nullptr) { }

    /* Copy constructor */
    /* TODO think about exception safety here */
    splaytree (const splaytree& other)
            : splaytree(other.begin(), other.end(), other.key_comp()) { }

    /* Move constructor */
    splaytree (splaytree&& other) : splaytree() {
        using std::swap;
        swap(*this, other);
    }

    template <typename Iter>
    splaytree (Iter first, Iter last, const key_compare& comp = key_compare())
            : splaytree(comp) {
        /* TODO think about exception safety here */
        insert(first, last);
    }

    splaytree (std::initializer_list<value_type> ilist,
               const key_compare& comp = key_compare())
            : splaytree(ilist.begin(), ilist.end(), comp) { }

    ~splaytree () {
        delete m_root;
    }

    void swap (splaytree& other) {
        using std::swap;
        swap(*this, other);
    }

    friend void swap (splaytree& lhs, splaytree& rhs) {
        using std::swap;
        swap(lhs.m_comp, rhs.m_comp);
        swap(lhs.m_size, rhs.m_size);
        swap(lhs.m_root, rhs.m_root);
    }

    iterator begin () { return iterator(node_type::minimum(m_root)); }
    const_iterator begin () const { return const_iterator(node_type::minimum(m_root)); }
    const_iterator cbegin () const { return begin(); }

    iterator end () { return iterator(nullptr); }
    const_iterator end () const { return const_iterator(nullptr); }
    const_iterator cend () const { return end(); }

    /* Requires our keys to be EqualityComparable. */
    friend bool operator== (const splaytree& lhs, const splaytree& rhs) {
        if (lhs.size() != rhs.size()) {
            return false;
        }

        return std::equal(lhs.begin(), lhs.end(), rhs.begin());
    }

    /* Requires our keys to be EqualityComparable. */
    friend bool operator!= (const splaytree& lhs, const splaytree& rhs) {
        return !(lhs == rhs);
    }

    friend bool operator< (const splaytree& lhs, const splaytree& rhs) {
        /* TODO worry about comparison objects with state--i.e., what if every
         * comparison generates side effects? Other things would probably
         * break, too. */
        return std::lexicographical_compare(
                lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), lhs.m_comp);
    }

    friend bool operator> (const splaytree& lhs, const splaytree& rhs) {
        return rhs < lhs;
    }

    friend bool operator<= (const splaytree& lhs, const splaytree& rhs) {
        return !(rhs < lhs);
    }

    friend bool operator>= (const splaytree& lhs, const splaytree& rhs) {
        return !(lhs < rhs);
    }

    splaytree& operator= (splaytree other) {
        using std::swap;
        swap(*this, other);
        return *this;
    }
    
    splaytree& operator= (std::initializer_list<value_type> ilist) {
        return *this = splaytree(ilist, m_comp);
    }

    size_type size () const { return m_size; }

    size_type max_size () const {
        return std::numeric_limits<size_type>::max();
    }

    bool empty () const {
        assert(!!m_size == !!m_root);
        return !m_root;
    }

    key_compare key_comp () const {
        /* If we're a set, then key_compare and value_compare are the same
         * type, and this static_cast should be a no-op. If we're a map,
         * value_compare's explicit conversion operator gives us the right
         * comparison object. */
        return static_cast<key_compare>(m_comp);
    }

    value_compare value_comp () const { return m_comp; }

    template <typename... Args>
    std::pair<iterator, bool> emplace (Args&&... args) {
        /* TODO I'll have to study how other people implement emplace() for
         * their data structures--this feels flawed. */
        auto newroot = new node_type (std::forward<Args>(args)...);
        if (end() == find_value(newroot->value())) {
            return insert_aux(newroot);
        }
        delete newroot;
        newroot = nullptr;
        return std::make_pair(iterator(m_root), false);
    }

    /* All hints are ignored. */
    template <typename... Args>
    iterator emplace_hint (const_iterator, Args&&... args) {
        return emplace(std::forward<Args>(args)...).first;
    }

    /* The assignment as written mentions that "Probably neither [FIND nor
     * INSERT] should call each other". However, it seems perfectly reasonable
     * to me that insert() should call find() (or in this case, find_value()).
     * In the case of splay trees, no efficiency is lost, because find()
     * prepares the tree for insertion by splaying the insertion point to the
     * root. */
    std::pair<iterator, bool> insert (const value_type& value) {
        if (end() == find_value(value)) {
            auto newroot = new node_type(value);
            return insert_aux(newroot);
        }
        return std::make_pair(iterator(m_root), false);
    }

    std::pair<iterator, bool> insert (value_type&& value) {
        if (end() == find_value(value)) {
            auto newroot = new node_type(std::forward<value_type>(value));
            return insert_aux(newroot);
        }
        return std::make_pair(iterator(m_root), false);
    }

    /* All hints are ignored. */
    iterator insert (const_iterator, const value_type& value) {
        return insert(value).first;
    }

    /* All hints are ignored. */
    iterator insert (const_iterator, value_type&& value) {
        return insert(value).first;
    }

    template <typename Iter>
    void insert (Iter first, Iter last) {
        while (first != last) {
            insert(*first++);
        }
    }

    void insert (std::initializer_list<value_type> ilist) {
        insert(ilist.begin(), ilist.end());
    }

    size_type erase (const value_type& value) {
        auto range = equal_range(value);
        size_type count = std::distance(range.first, range.second);
        erase(range.first, range.second);
        return count;
    }

    iterator erase (const_iterator pos) {
        assert(pos.m_node);

        m_root = node_type::erase(pos++.m_node);
        --m_size;

        return iterator(pos.m_node);
    }

    iterator erase (const_iterator first, const_iterator last) {
        while (first != last) {
            first = erase(first);
        }
        return iterator(last.m_node);
    }
    
    void clear () {
        delete m_root;
        m_root = nullptr;
        m_size = 0;
    }

    /* Return an iterator to the element matching the given key, or an end()
     * iterator if the key is not found. */
    iterator find (const key_type& key) {
        return find_value(base_type::make_value(key));
    }

    size_type count (const key_type& key) {
        auto value = base_type::make_value(key);

        auto range = equal_range(value);
        return std::distance(range.first, range.second);
    }

    std::pair<iterator, iterator>
    equal_range (const key_type& key) {
        auto value = base_type::make_value(key);
        return std::make_pair(lower_bound(value), upper_bound(value));
    }

    /* Return an iterator to the smallest element greater than or equal to the
     * given search key, or end() if not found. */
    iterator lower_bound (const key_type& key) {
        auto value = base_type::make_value(key);

        auto bound = node_type::lower_bound(m_root, value, m_comp);
        
        if (!bound) {
            return iterator(nullptr);
        }

        m_root = bound;
        return iterator(bound);
    }

    /* Return an iterator to the smallest element greater than the given
     * search key, or end() if not found. */
    iterator upper_bound (const key_type& key) {
        auto value = base_type::make_value(key);

        auto bound = node_type::upper_bound(m_root, value, m_comp);

        if (!bound) {
            return iterator(nullptr);
        }

        m_root = bound;
        return iterator(bound);
    }

    size_type count (const key_type& key) const {
        auto value = base_type::make_value(key);
        auto range = equal_range(value);
        return std::distance(range.first, range.second);
    }

    std::pair<const_iterator, const_iterator>
    equal_range (const key_type& key) const {
        auto value = base_type::make_value(key);
        return std::make_pair(lower_bound(value), upper_bound(value));
    }

    const_iterator lower_bound (const key_type& key) const {
        auto value = base_type::make_value(key);
        return const_iterator(node_type::lower_bound_no_splay(m_root, value, m_comp));
    }

    const_iterator upper_bound (const key_type& key) const {
        auto value = base_type::make_value(key);
        return const_iterator(node_type::upper_bound_no_splay(m_root, value, m_comp));
    }

    void dump_structure () {
        if (m_root) {
            m_root->dump_structure();
        }
        else {
            std::cout << "(nil)\n";
        }
    }

private:
    /* Auxiliary function called by insert() to reduce code duplication.
     * Preconditions: newroot is the newly created element to be inserted, and
     * the tree has been arranged such that the correct place for the new root
     * node is between the current root and one of its children. */
    std::pair<iterator,bool> insert_aux (node_type* newroot) {
        assert(newroot);

        node_type* lhs = nullptr;
        node_type* rhs = nullptr;

        if (m_root) {
            if (m_comp(newroot->value(), m_root->value())) {
                lhs = m_root->detach_left();
                rhs = m_root;
            }
            else {
                assert(m_comp(m_root->value(), newroot->value()));
                lhs = m_root;
                rhs = m_root->detach_right();
            }
        }

        m_root = newroot;
        m_root->attach_left(lhs);
        m_root->attach_right(rhs);

        ++m_size;

        return std::make_pair(iterator(m_root), true);
    }

    iterator find_value (const value_type& value) {
        m_root = node_type::search(m_root, value, m_comp);

        if (!m_root || m_comp(value, m_root->value()) || m_comp(m_root->value(), value)) {
            /* Not found. */
            return iterator(nullptr);
        }

        return iterator(m_root);
    }

    value_compare m_comp;
    size_type m_size;
    node_type* m_root;
};

//////////////////////////////////////////////////////////////////////////////

/* A set container that uses a splaytree implementation. */
template <typename T, typename Compare = std::less<T>>
using set = splaytree<detail::set_base<T, Compare>>;

/* A map container that uses a splaytree implementation. */
template <typename Key, typename T, typename Compare = std::less<Key>>
using map = splaytree<detail::map_base<Key, T, Compare>>;

} // namespace splaytree

#endif
