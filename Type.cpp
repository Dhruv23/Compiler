/*
 * File:	Type.cpp
 *
 * Description:	This file contains the class definitions for types in
 *		Simple C.  A type is either a scalar type, an array type,
 *		or a function type.  Types include a specifier and the
 *		number of levels of indirection.  Array types also have a
 *		length, and function types also have a parameter list.  An
 *		error type is also supported for use in undeclared
 *		identifiers and the results of type checking.
 *
 *		Extra functionality:
 *		- equality and inequality operators
 *		- predicate functions
 *		- stream operator
 *		- error type
 */

# include <cassert>
# include "tokens.h"
# include "Type.h"

using namespace std;


/*
 * Function:	Type::Type (constructor)
 *
 * Description:	Initialize this type as an error type.
 */

Type::Type()
    : _kind(ERROR)
{
}


/*
 * Function:	Type::Type (constructor)
 *
 * Description:	Initialize this type object as a scalar type.
 */

Type::Type(int specifier, unsigned indirection)
    : _kind(SCALAR), _specifier(specifier), _indirection(indirection)
{
    assert(specifier == CHAR || specifier == INT || specifier == LONG);
}


/*
 * Function:	Type::Type (constructor)
 *
 * Description:	Initialize this type object as an array type.
 */

Type::Type(int specifier, unsigned indirection, unsigned long length)
    : _kind(ARRAY), _specifier(specifier), _indirection(indirection)
{
    assert(specifier == CHAR || specifier == INT || specifier == LONG);
    _length = length;
}


/*
 * Function:	Type::Type (constructor)
 *
 * Description:	Initialize this type object as a function type.
 */

Type::Type(int specifier, unsigned indirection, Parameters *parameters)
    : _kind(FUNCTION), _specifier(specifier), _indirection(indirection)
{
    assert(specifier == CHAR || specifier == INT || specifier == LONG);
    _parameters = parameters;
}


/*
 * Function:	Type::kind (accessor)
 *
 * Description:	Return the kind of this type.
 */

int Type::kind() const
{
    return _kind;
}


/*
 * Function:	Type::specifier (accessor)
 *
 * Description:	Return the specifier of this type.
 */

int Type::specifier() const
{
    return _specifier;
}


/*
 * Function:	Type::indirection (accessor)
 *
 * Description:	Return the number of levels of indirection of this type.
 */

unsigned Type::indirection() const
{
    return _indirection;
}


/*
 * Function:	Type::length (accessor)
 *
 * Description:	Return the length of this type, which must be an array
 *		type.  Is there a better way than calling assert?  There
 *		certainly isn't an easier way.
 */

unsigned long Type::length() const
{
    assert(_kind == ARRAY);
    return _length;
}


/*
 * Function:	Type::parameters (accessor)
 *
 * Description:	Return the parameters of this type, which must be a
 *		function type.
 */

Parameters *Type::parameters() const
{
    assert(_kind == FUNCTION);
    return _parameters;
}


/*
 * Function:	Type::operator ==
 *
 * Description:	Return whether another type is equal to this type.  The
 *		parameter lists are checked for function types, which C++
 *		makes so easy.  (At least, it makes something easy!)
 */

bool Type::operator ==(const Type &that) const
{
    if (_kind != that._kind)
	return false;

    if (_kind == ERROR)
	return true;

    if (_specifier != that._specifier || _indirection != that._indirection)
	return false;

    if (_kind == SCALAR)
	return true;

    if (_kind == ARRAY)
	return _length == that._length;

    assert(_kind == FUNCTION && _parameters && that._parameters);

    if (_parameters->variadic != that._parameters->variadic)
	return false;

    return _parameters->types == that._parameters->types;
}


/*
 * Function:	Type::operator !=
 *
 * Description:	Well, what do you think it does?  Why can't the language
 *		generate this function for us?  Because they think we want
 *		it to do something else?  Yeah, like that'd be a good idea.
 */

bool Type::operator !=(const Type &that) const
{
    return !operator ==(that);
}


/*
 * Function:	Type::isScalar (predicate)
 *
 * Description:	Return whether this type is a scalar type.
 */

bool Type::isScalar() const
{
    return _kind == SCALAR;
}


/*
 * Function:	Type::isArray (predicate)
 *
 * Description:	Return whether this type is an array type.
 */

bool Type::isArray() const
{
    return _kind == ARRAY;
}


/*
 * Function:	Type::isFunction (predicate)
 *
 * Description:	Return whether this type is a function type.
 */

bool Type::isFunction() const
{
    return _kind == FUNCTION;
}


/*
 * Function:	Type::isNumeric (predicate)
 *
 * Description:	Return whether this type is a numeric type, meaning it is
 *		simply char, int, or long.
 */

bool Type::isNumeric() const
{
    return _kind == SCALAR && _indirection == 0;
}


/*
 * Function:	Type::isPointer (predicate)
 *
 * Description:	Return whether this type is a pointer type, meaning it is a
 *		scalar type with indirection.
 */

bool Type::isPointer() const
{
    return _kind == SCALAR && _indirection > 0;
}


/*
 * Function:	Type::isCompatibleWith (predicate)
 *
 * Description:	Return whether this type is compatible with the given type.
 *		In Simple C, two types are compatible if they are both
 *		numeric types or are identical scalar types.
 */

bool Type::isCompatibleWith(const Type &that) const
{
    return (isNumeric() && that.isNumeric()) || (isScalar() && *this == that);
}


/*
 * Function:	Type::decay
 *
 * Description:	Return the result of performing type decay on this type.
 *		In Simple C, an array is decayed to a pointer type.
 */

Type Type::decay() const
{
    if (_kind == ARRAY)
	return Type(_specifier, _indirection + 1);

    return *this;
}


/*
 * Function:	Type::promote
 *
 * Description:	Return the result of performing arithmetic promotion on
 *		this type.  In Simple C, a character is promoted to an
 *		integer.
 */

Type Type::promote() const
{
    if (_kind == SCALAR && _indirection == 0 && _specifier == CHAR)
	return Type(INT);

    return *this;
}


/*
 * Function:	Type::dereference
 *
 * Description:	Return the result of dereferencing this type, which must be
 *		a pointer type.
 */

Type Type::dereference() const
{
    assert(_kind == SCALAR && _indirection > 0);
    return Type(_specifier, _indirection - 1);
}


/*
 * Function:	operator <<
 *
 * Description:	Write a type to the specified output stream.  At least C++
 *		let's us do some cool things.
 */

ostream &operator <<(ostream &ostr, const Type &type)
{
    if (type.kind() == ERROR)
	ostr << "error";

    else {
	if (type.specifier() == CHAR)
	    ostr << "char";
	else if (type.specifier() == INT)
	    ostr << "int";
	else if (type.specifier() == LONG)
	    ostr << "long";
	else
	    ostr << "unknown";

	if (type.indirection() > 0)
	    ostr << " " << string(type.indirection(), '*');

	if (type.kind() == ARRAY)
	    ostr << "[" << type.length() << "]";

	else if (type.kind() == FUNCTION)
	    ostr << "()";
    }

    return ostr;
}
