/*
 * File:	Type.h
 *
 * Description:	This file contains the class definitions for types in
 *		Simple C.  A type is either a scalar type, an array type,
 *		or a function type.  Types include a specifier and the
 *		number of levels of indirection.  Array types also have a
 *		length, and function types also have a parameter list.  An
 *		error type is also supported for use in undeclared
 *		identifiers and the results of type checking.
 *
 *		The enumeration for the kinds of declarators should
 *		probably be within the class, but we have constants for
 *		tokens floating around in the global namespace anyway.
 *
 *		No subclassing is used to avoid the problem of object
 *		slicing (since we'll be treating types as value types) and
 *		the proliferation of small member functions.
 *
 *		As we've designed them, types are essentially immutable,
 *		since we haven't included any mutators.  In practice, we'll
 *		be creating new types rather than changing existing types.
 */

# ifndef TYPE_H
# define TYPE_H
# include <vector>
# include <ostream>

enum { ARRAY, FUNCTION, SCALAR };

typedef std::vector<class Type> Types;

struct Parameters {
    bool variadic;
    Types types;
};

class Type {
    short _kind, _specifier;
    unsigned _indirection;

    union {
	unsigned long _length;
	Parameters *_parameters;
    };

public:
    Type();
    Type(int specifier, unsigned indirection = 0);
    Type(int specifier, unsigned indirection, unsigned long length);
    Type(int specifier, unsigned indirection, Parameters *parameters);

    int kind() const;
    int specifier() const;
    unsigned indirection() const;

    unsigned long length() const;
    Parameters *parameters() const;

    bool operator ==(const Type &that) const;
    bool operator !=(const Type &that) const;

    bool isScalar() const;
    bool isArray() const;
    bool isFunction() const;

    bool isNumeric() const;
    bool isPointer() const;
    bool isCompatibleWith(const Type &that) const;

    Type decay() const;
    Type promote() const;
    Type dereference() const;

    unsigned long size() const;
    unsigned alignment() const;
};

std::ostream &operator <<(std::ostream &ostr, const Type &type);

# endif /* TYPE_H */
