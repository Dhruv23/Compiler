/*
 * File:	checker.cpp
 *
 * Description:	This file contains the public and private function and
 *		variable definitions for the semantic checker for Simple C.
 *
 *		If a symbol is redeclared, the redeclaration is discarded
 *		and the original declaration is retained.
 *
 *		Extra functionality:
 *		- inserting an undeclared symbol with the error type
 *		- optionally deleting the symbols when closing a scope
 *		- scaling the operands and results of pointer arithmetic
 *		- explicit type conversions
 */

# include <set>
# include <iostream>
# include "lexer.h"
# include "tokens.h"
# include "checker.h"


using std::set;
using std::string;

static set<string> defined;
static Scope *current, *global;
static const Type error, character(CHAR), integer(INT), longint(LONG);

static string redefined = "redefinition of '%s'";
static string redeclared = "redeclaration of '%s'";
static string conflicting = "conflicting types for '%s'";
static string undeclared = "'%s' undeclared";

static string invalid_break = "break statement not within loop";
static string invalid_return = "invalid return type";
static string invalid_scalar = "scalar type required in statement";
static string invalid_lvalue = "lvalue required in expression";
static string invalid_operands = "invalid operands to binary %s";
static string invalid_operand = "invalid operand to unary %s";
static string invalid_sizeof = "invalid operand in sizeof expression";
static string invalid_cast = "invalid operand in cast expression";
static string invalid_function = "called object is not a function";
static string invalid_arguments = "invalid arguments to called function";


/*
 * Function:	cast (private)
 *
 * Description:	Convert the given expression to the specified type by
 *		inserting a cast expression if necessary.  No checking is
 *		done to determine the validity of the cast.  As an
 *		optimization, an integer literal can always be converted to
 *		a long integer literal without an explicit cast.
 */

static Expression *&cast(Expression *&expr, const Type &type)
{
    unsigned long value;


    if (expr->isNumber(value)) {
	if (expr->type() == integer && type == longint) {
	    delete expr;
	    expr = new Number(value);
	}
    }

    if (expr->type() != type)
	expr = new Cast(expr, type);

    return expr;
}


/*
 * Function:	promote (private)
 *
 * Description:	Perform arithmetic promotion on type of the given
 *		expression.  A character is promoted to an integer by
 *		inserting a cast expression.
 */

static Expression *&promote(Expression *&expr)
{
    return cast(expr, expr->type().promote());
}


/*
 * Function:	decay (private)
 *
 * Description:	Perform the result of performing type decay on type of the
 *		given expression.  An array is decayed to a pointer type by
 *		inserting an address expression.
 */

static const Type &decay(Expression *&expr)
{
    if (expr->type().isArray())
	expr = new Address(expr, expr->type().decay());

    return expr->type();
}


/*
 * Function:	extend (private)
 *
 * Description:	Convert the expression to the given type but only by
 *		sign-extending.  At the minimum, perform arithmetic
 *		promotion on the expression.
 */

static Expression *&extend(Expression *&expr, const Type &type)
{
    const Type &t = expr->type();


    if ((t == character || t == integer) && type == longint)
	return cast(expr, longint);

    return promote(expr);
}


/*
 * Function:	scale (private)
 *
 * Description:	Scale the result of pointer arithmetic.
 */

static Expression *scale(Expression *expr, unsigned size)
{
    unsigned long value;


    if (size == 1)
	return extend(expr, longint);

    if (expr->isNumber(value)) {
	delete expr;
	return new Number(value * size);
    }

    extend(expr, longint);
    return new Multiply(expr, new Number(size), longint);
}


/*
 * Function:	openScope
 *
 * Description:	Create a scope and make it the new top-level scope.
 */

Scope *openScope()
{
    current = new Scope(current);

    if (global == nullptr)
	global = current;

    return current;
}


/*
 * Function:	closeScope
 *
 * Description:	Remove the top-level scope, and make its enclosing scope
 *		the new top-level scope.
 */

Scope *closeScope(bool cleanup)
{
    Scope *old = current;
    current = current->enclosing();

    if (!cleanup)
	return old;

    for (auto symbol : old->symbols()) {
	if (symbol->type().isFunction())
	    delete symbol->type().parameters();

	delete symbol;
    }

    delete old;
    return nullptr;
}


/*
 * Function:	defineFunction
 *
 * Description:	Define a function with the specified NAME and TYPE.  A
 *		function is always defined in the outermost scope.  We
 *		simply rely on declareFunction() to do most of the actual
 *		work.
 */

Symbol *defineFunction(const string &name, const Type &type)
{
    if (defined.count(name) > 0)
	report(redefined, name);

    defined.insert(name);
    return declareFunction(name, type);
}


/*
 * Function:	declareFunction
 *
 * Description:	Declare a function with the specified NAME and TYPE.  A
 *		function is always declared in the outermost scope.  Any
 *		redeclaration is discarded.
 */

Symbol *declareFunction(const string &name, const Type &type)
{
    Symbol *symbol;


    symbol = global->find(name);

    if (symbol == nullptr) {
	symbol = new Symbol(name, type);
	global->insert(symbol);

    } else {
	if (symbol->type() != type)
	    report(conflicting, name);

	delete type.parameters();
    }

    return symbol;
}


/*
 * Function:	declareVariable
 *
 * Description:	Declare a variable with the specified NAME and TYPE.  Any
 *		redeclaration is discarded.
 */

Symbol *declareVariable(const string &name, const Type &type)
{
    Symbol *symbol;


    symbol = current->find(name);

    if (symbol == nullptr) {
	symbol = new Symbol(name, type);
	current->insert(symbol);

    } else {
	if (current != global)
	    report(redeclared, name);

	else if (symbol->type() != type)
	    report(conflicting, name);
    }

    return symbol;
}


/*
 * Function:	checkIdentifier
 *
 * Description:	Check if NAME is declared.  If it is undeclared, then
 *		declare it as having the error type in order to eliminate
 *		future error messages.
 */

Symbol *checkIdentifier(const string &name)
{
    Symbol *symbol;


    symbol = current->lookup(name);

    if (symbol == nullptr) {
	report(undeclared, name);
	symbol = new Symbol(name, error);
	current->insert(symbol);
    }

    return symbol;
}


/*
 * Function:	checkCall
 *
 * Description:	Check a function call expression: ID (ARGS).  The
 *		identifier must have type "function returning T" and the
 *		result has type T.  An argument before any ellipsis
 *		undergoes type decay and then must be compatible with its
 *		corresponding parameter.  Arguments after any ellipsis
 *		undergo promotion and decay and must all be scalar types.
 */

Expression *checkCall(Symbol *id, Expressions &args)
{
    const Type &t = id->type();
    Type result = error;
    Parameters *params;
    unsigned i;


    if (t != error) {
	if (!t.isFunction()) {
	    report(invalid_function);
	    return new Call(id, args, error);
	}

	params = id->type().parameters();

	if (args.size() < params->types.size()) {
	    report(invalid_arguments);
	    return new Call(id, args, error);
	}

	if (!params->variadic && args.size() > params->types.size()) {
	    report(invalid_arguments);
	    return new Call(id, args, error);
	}

	for (i = 0; i < params->types.size(); i ++)
	    if (args[i]->type() != error) {
		if (!params->types[i].isCompatibleWith(decay(args[i]))) {
		    report(invalid_arguments);
		    return new Call(id, args, error);
		} else
		    cast(args[i], params->types[i]);
	    }

	while (i < args.size())
	    if (args[i]->type() != error)
		if (!decay(promote(args[i ++])).isScalar()) {
		    report(invalid_arguments);
		    return new Call(id, args, error);
		}

	result = Type(t.specifier(), t.indirection());
    }

    return new Call(id, args, result);
}


/*
 * Function:	checkArray
 *
 * Description:	Check an array expression: LEFT [RIGHT].  Both operands
 *		undergo the usual conversions, and then the left operand
 *		must have type "pointer to T" and the right operand must
 *		have a numeric type, and the result has type T.
 */

Expression *checkArray(Expression *left, Expression *right)
{
    const Type &t1 = decay(promote(left));
    const Type &t2 = decay(extend(right, longint));
    Type result = error;


    if (t1 != error && t2 != error) {
	if (t1.isPointer() && t2.isNumeric()) {
	    right = scale(right, t1.dereference().size());
	    result = t1.dereference();
	} else
	    report(invalid_operands, "[]");
    }

    return new Dereference(new Add(left, right, t1), result);;
}


/*
 * Function:	checkNot
 *
 * Description:	Check a logical negation expression: ! EXPR.  The operand
 *		undergoes the usual conversions and then must have a scalar
 *		type, and the result has type integer.
 */

Expression *checkNot(Expression *expr)
{
    const Type &t = decay(promote(expr));
    Type result = error;


    if (t != error) {
	if (t.isScalar())
	    result = integer;
	else
	    report(invalid_operand, "!");
    }

    return new Not(expr, result);
}


/*
 * Function:	checkNegate
 *
 * Description:	Check an arithmetic negation expression: - EXPR.  The
 *		operand undergoes type promotion, and then must have a
 *		numeric type, and result has the same type.
 */

Expression *checkNegate(Expression *expr)
{
    const Type &t = decay(promote(expr));
    Type result = error;


    if (t != error) {
	if (t.isNumeric())
	    result = t;
	else
	    report(invalid_operand, "-");

    }
    return new Negate(expr, result);
}


/*
 * Function:	checkDereference
 *
 * Description:	Check a dereference expression: * EXPR.  The operand first
 *		undergoes type decay, and then must have type "pointer to
 *		T," and the result has type T.
 */

Expression *checkDereference(Expression *expr)
{
    const Type &t = decay(expr);
    Type result = error;


    if (t != error) {
	if (t.isPointer())
	    result = t.dereference();
	else
	    report(invalid_operand, "*");
    }

    return new Dereference(expr, result);;
}


/*
 * Function:	checkAddress
 *
 * Description:	Check an address expression: & EXPR.  The operand must be
 *		an lvalue, and if it has type T, then the result has type
 *		"pointer to T."
 */

Expression *checkAddress(Expression *expr)
{
    const Type &t = expr->type();
    Type result = error;


    if (t != error) {
	if (expr->lvalue())
	    result = Type(t.specifier(), t.indirection() + 1);
	else
	    report(invalid_lvalue);
    }

    return new Address(expr, result);
}


/*
 * Function:	checkSizeof
 *
 * Description:	Check a sizeof expression: sizeof EXPR.  The expression
 *		must not have a function type.
 */

Expression *checkSizeof(Expression *expr)
{
    const Type &t = expr->type();
    unsigned size = 0;


    if (t != error) {
	if (!t.isFunction())
	    size = t.size();
	else
	    report(invalid_sizeof);
    }

    return new Number(size);
}


/*
 * Function:	checkCast
 *
 * Description:	Check a cast expression: (TYPE) EXPR.  The operand type and
 *		desired type must both be numeric types, both be pointer
 *		types, or one is a pointer type and the other is long.
 */

Expression *checkCast(const Type &type, Expression *expr)
{
    const Type &t = decay(expr);
    Type result = error;


    if (t != error) {
	if (type.isNumeric() && t.isNumeric())
	    result = type;
	else if (type.isPointer() && t.isPointer())
	    result = type;
	else if (type.isPointer() && t == longint)
	    result = type;
	else if (type == longint && t.isPointer())
	    result = type;
	else
	    report(invalid_cast);
    }

    return cast(expr, result);
}


/*
 * Function:	checkMultiplicative (private)
 *
 * Description:	Check a multiplicative expression.  The operands undergo
 *		the usual conversions and then both must have a numeric
 *		type, and the result type long if either operand has type
 *		long and has type int otherwise.
 */

static Type
checkMultiplicative(Expression *&left, Expression *&right, const string &op)
{
    const Type &t1 = decay(extend(left, right->type()));
    const Type &t2 = decay(extend(right, left->type()));
    Type result = error;


    if (t1 != error && t2 != error) {
	if (t1.isNumeric() && t2.isNumeric())
	    result = t1;
	else
	    report(invalid_operands, op);
    }

    return result;
}


/*
 * Function:	checkMultiply
 *
 * Description:	Check a multiplication expression: LEFT * RIGHT.
 */

Expression *checkMultiply(Expression *left, Expression *right)
{
    Type t = checkMultiplicative(left, right, "*");
    return new Multiply(left, right, t);
}


/*
 * Function:	checkDivide
 *
 * Description:	Check a division expression: LEFT / RIGHT.
 */

Expression *checkDivide(Expression *left, Expression *right)
{
    Type t = checkMultiplicative(left, right, "/");
    return new Divide(left, right, t);
}


/*
 * Function:	checkRemainder
 *
 * Description:	Check a remainder expression: LEFT % RIGHT.
 */

Expression *checkRemainder(Expression *left, Expression *right)
{
    Type t = checkMultiplicative(left, right, "%");
    return new Remainder(left, right, t);
}


/*
 * Function:	checkAdd
 *
 * Description:	Check an addition expression: LEFT + RIGHT.  The operands
 *		first undergo the usual conversions.  If both then have
 *		numeric types, then the result has type long if either
 *		operand has type long and has type int otherwise.  If one
 *		operand has a pointer type and the other has a numeric
 *		type, then the result has that pointer type.
 */

Expression *checkAdd(Expression *left, Expression *right)
{
    const Type &t1 = decay(extend(left, right->type()));
    const Type &t2 = decay(extend(right, left->type()));
    Type result = error;


    if (t1 != error && t2 != error) {
	if (t1.isNumeric() && t2.isNumeric())
	    result = t1;

	else if (t1.isPointer() && t2.isNumeric()) {
	    right = scale(right, t1.dereference().size());
	    result = t1;

	} else if (t1.isNumeric() && t2.isPointer()) {
	    left = scale(left, t2.dereference().size());
	    result = t2;

	} else
	    report(invalid_operands, "+");
    }

    return new Add(left, right, result);
}


/*
 * Function:	checkSubtract
 *
 * Description:	Check a subtraction expression: LEFT - RIGHT.  The operands
 *		first undergo the usual conversions.  If both then have
 *		numeric types, then the result has type long if either
 *		operand has type long and has type int otherwise.  If the
 *		left operand has a pointer type and the right operand has a
 *		numeric type, then the result has that pointer type.  If
 *		the left and right operands have identical pointer types,
 *		then the result has type long.
 */

Expression *checkSubtract(Expression *left, Expression *right)
{
    const Type &t1 = decay(extend(left, right->type()));
    const Type &t2 = decay(extend(right, left->type()));
    Type result = error;
    Expression *expr;


    if (t1 != error && t2 != error) {
	if (t1.isNumeric() && t2.isNumeric())
	    result = t1;

	else if (t1.isPointer() && t1 == t2)
	    result = longint;

	else if (t1.isPointer() && t2.isNumeric()) {
	    right = scale(right, t1.dereference().size());
	    result = t1;

	} else
	    report(invalid_operands, "-");
    }

    expr = new Subtract(left, right, result);

    if (t1.isPointer() && t1 == t2)
	expr = new Divide(expr, new Number(t1.dereference().size()), longint);

    return expr;
}


/*
 * Function:	checkComparative (private)
 *
 * Description:	Check an equality or relational expression.  Both operands
 *		undergo the usual conversions and have their types made
 *		common, and then the two types must be compatible, and the
 *		result has type int.
 */

static Type
checkComparative(Expression *&left, Expression *&right, const string &op)
{
    const Type &t1 = decay(extend(left, right->type()));
    const Type &t2 = decay(extend(right, left->type()));
    Type result = error;


    if (t1 != error && t2 != error) {
	if (t1.isCompatibleWith(t2))
	    result = integer;
	else
	    report(invalid_operands, op);
    }

    return result;
}


/*
 * Function:	checkLessThan
 *
 * Description:	Check a less-than expression: LEFT < RIGHT.
 */

Expression *checkLessThan(Expression *left, Expression *right)
{
    Type t = checkComparative(left, right, "<");
    return new LessThan(left, right, t);
}


/*
 * Function:	checkGreaterThan
 *
 * Description:	Check a greater-than expression: LEFT > RIGHT.
 */

Expression *checkGreaterThan(Expression *left, Expression *right)
{
    Type t = checkComparative(left, right, ">");
    return new GreaterThan(left, right, t);
}


/*
 * Function:	checkLessOrEqual
 *
 * Description:	Check a less-than-or-equal expression: LEFT <= RIGHT.
 */

Expression *checkLessOrEqual(Expression *left, Expression *right)
{
    Type t = checkComparative(left, right, "<=");
    return new LessOrEqual(left, right, t);
}


/*
 * Function:	checkGreaterOrEqual
 *
 * Description:	Check a greater-than-or-equal expression: LEFT >= RIGHT.
 */

Expression *checkGreaterOrEqual(Expression *left, Expression *right)
{
    Type t = checkComparative(left, right, ">=");
    return new GreaterOrEqual(left, right, t);
}


/*
 * Function:	checkEqual
 *
 * Description:	Check an equality expression: LEFT == RIGHT.
 */

Expression *checkEqual(Expression *left, Expression *right)
{
    Type t = checkComparative(left, right, "==");
    return new Equal(left, right, t);
}


/*
 * Function:	checkNotEqual
 *
 * Description:	Check an inequality expression: LEFT != RIGHT.
 */

Expression *checkNotEqual(Expression *left, Expression *right)
{
    Type t = checkComparative(left, right, "!=");
    return new NotEqual(left, right, t);
}


/*
 * Function:	checkLogical (private)
 *
 * Description:	Check a logical-or or logical-and expression.  Both
 *		operands undergo the usual conversions and then must have
 *		scalar types, and the result has type int.
 */

static Type
checkLogical(Expression *&left, Expression *&right, const string &op)
{
    const Type &t1 = decay(promote(left));
    const Type &t2 = decay(promote(right));
    Type result = error;


    if (t1 != error && t2 != error) {
	if (t1.isScalar() && t2.isScalar())
	    result = integer;
	else
	    report(invalid_operands, op);
    }

    return result;
}


/*
 * Function:	checkLogicalAnd
 *
 * Description:	Check a logical-and expression: LEFT && RIGHT.
 */

Expression *checkLogicalAnd(Expression *left, Expression *right)
{
    Type t = checkLogical(left, right, "&&");
    return new LogicalAnd(left, right, t);
}


/*
 * Function:	checkLogicalOr
 *
 * Description:	Check a logical-or expression: LEFT || RIGHT.
 */

Expression *checkLogicalOr(Expression *left, Expression *right)
{
    Type t = checkLogical(left, right, "||");
    return new LogicalOr(left, right, t);
}


/*
 * Function:	checkTest
 *
 * Description:	Check if the type of the expression is a legal type in a
 *		test expression in a while, if-then, if-then-else, or for
 *		statement.  The expression undergoes the usual conversions
 *		and then must be a scalar type.
 */

Expression *checkTest(Expression *expr)
{
    const Type &t = decay(promote(expr));

    if (t != error && !t.isScalar())
	report(invalid_scalar);

    return expr;
}


/*
 * Function:	checkAssignment
 *
 * Description:	Check an assignment statement: LEFT = RIGHT.  The left-hand
 *		side must be an lvalue and the types of the two expressions
 *		must be compatible.
 */

Statement *checkAssignment(Expression *left, Expression *right)
{
    const Type &t1 = left->type();
    const Type &t2 = decay(right);


    if (t1 != error && t2 != error) {
	if (!left->lvalue())
	    report(invalid_lvalue);
	else if (!t1.isCompatibleWith(t2))
	    report(invalid_operands, "=");
	else
	    cast(right, left->type());
    }

    return new Assignment(left, right);
}


/*
 * Function:	checkReturn
 *
 * Description:	Check a return statement: return EXPR.  The type of the
 *		expression must be compatible with the return type of the
 *		enclosing function.
 */

Statement *checkReturn(Expression *expr, const Type &type)
{
    const Type &t = decay(expr);


    if (t != error) {
	if (t.isCompatibleWith(type))
	    cast(expr, type);
	else
	    report(invalid_return);
    }

    return new Return(expr);
}


/*
 * Function:	checkBreak
 *
 * Description:	Check if a break statement is within a loop.
 */

Statement *checkBreak(unsigned depth)
{
    if (depth == 0)
	report(invalid_break);

    return new Break();
}
