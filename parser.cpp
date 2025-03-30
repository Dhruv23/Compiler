/*
 * File:	parser.cpp
 *
 * Description:	This file contains the public and private function and
 *		variable definitions for the recursive-descent parser for
 *		Simple C.
 */

# include <cstdlib>
# include <iostream>
# include "generator.h"
# include "checker.h"
# include "string.h"
# include "tokens.h"
# include "lexer.h"

using namespace std;

static int lookahead, nexttoken;
static string lexbuf, nextbuf;

static Expression *expression();
static Statement *statement();

static Type returnType;
static unsigned loopDepth;


/*
 * Function:	error
 *
 * Description:	Report a syntax error to standard error.
 */

static void error()
{
    if (lookahead == DONE)
	report("syntax error at end of file");
    else
	report("syntax error at '%s'", lexbuf);

    exit(EXIT_FAILURE);
}


/*
 * Function:	match
 *
 * Description:	Match the next token against the specified token.  A
 *		failure indicates a syntax error and will terminate the
 *		program since our parser does not do error recovery.
 */

static void match(int t)
{
    if (lookahead != t)
	error();

    if (nexttoken != 0) {
	lookahead = nexttoken;
	lexbuf = nextbuf;
	nexttoken = 0;
    } else {
	lookahead = yylex();
	lexbuf = yytext;
    }
}


/*
 * Function:	peek
 *
 * Description:	Return the next token in the input stream and save it so
 *		that match() will later return it.
 */

static int peek()
{
    if (!nexttoken) {
	nexttoken = yylex();
	nextbuf = yytext;
    }

    return nexttoken;
}


/*
 * Function:	isSpecifier
 *
 * Description:	Return whether the given token is a type specifier.  Simple
 *		C has only char, int, and long.
 */

static bool isSpecifier(int token)
{
    return token == CHAR || token == INT || token == LONG;
}


/*
 * Function:	number
 *
 * Description:	Match the next token as a number and return its value.
 */

static unsigned long number()
{
    string buf;


    buf = lexbuf;
    match(NUM);
    return strtoul(buf.c_str(), NULL, 0);
}


/*
 * Function:	identifier
 *
 * Description:	Match the next token as an identifier and return its name.
 */

static string identifier()
{
    string buf;


    buf = lexbuf;
    match(ID);
    return buf;
}


/*
 * Function:	specifier
 *
 * Description:	Parse a type specifier.
 *
 *		specifier:
 *		  char
 *		  int
 *		  long
 */

static int specifier()
{
    int typespec = lookahead;

    if (isSpecifier(lookahead))
	match(lookahead);
    else
	error();

    return typespec;
}


/*
 * Function:	pointers
 *
 * Description:	Parse pointer declarators (i.e., zero or more asterisks).
 *
 *		pointers:
 *		  empty
 *		  * pointers
 */

static unsigned pointers()
{
    unsigned count = 0;

    while (lookahead == '*') {
	match('*');
	count ++;
    }

    return count;
}


/*
 * Function:	declarator
 *
 * Description:	Parse a declarator, which in Simple C is either a scalar
 *		variable or an array, with optional pointer declarators.
 *
 *		declarator:
 *		  pointers identifier
 *		  pointers identifier [ num ]
 */

static void declarator(int typespec)
{
    string name;
    unsigned indirection;


    indirection = pointers();
    name = identifier();

    if (lookahead == '[') {
	match('[');
	declareVariable(name, Type(typespec, indirection, number()));
	match(']');

    } else
	declareVariable(name, Type(typespec, indirection));
}


/*
 * Function:	declaration
 *
 * Description:	Parse a local variable declaration.  Global declarations
 *		are handled separately since we need to detect a function
 *		as a special case.
 *
 *		declaration:
 *		  specifier declarator-list ';'
 *
 *		declarator-list:
 *		  declarator
 *		  declarator , declarator-list
 */

static void declaration()
{
    int typespec;


    typespec = specifier();
    declarator(typespec);

    while (lookahead == ',') {
	match(',');
	declarator(typespec);
    }

    match(';');
}


/*
 * Function:	declarations
 *
 * Description:	Parse a possibly empty sequence of declarations.
 *
 *		declarations:
 *		  empty
 *		  declaration declarations
 */

static void declarations()
{
    while (isSpecifier(lookahead))
	declaration();
}


/*
 * Function:	primaryExpression
 *
 * Description:	Parse a primary expression.
 *
 *		primary-expression:
 *		  ( expression )
 *		  identifier ( expression-list )
 *		  identifier ( )
 *		  identifier
 *		  num
 *		  character
 *		  string
 *
 *		expression-list:
 *		  expression
 *		  expression , expression-list
 */

static Expression *primaryExpression()
{
    Expression *expr;


    if (lookahead == '(') {
	match('(');
	expr = expression();
	match(')');

    } else if (lookahead == STRING) {
	lexbuf = lexbuf.substr(1, lexbuf.size() - 2);
	expr = new String(parseString(lexbuf));
	match(STRING);

    } else if (lookahead == CHARACTER) {
	lexbuf = lexbuf.substr(1, lexbuf.size() - 2);
	expr = new Number(parseString(lexbuf)[0]);
	match(CHARACTER);

    } else if (lookahead == NUM) {
	expr = new Number(lexbuf);
	match(NUM);

    } else if (lookahead == ID) {
	Expressions args;
	Symbol *symbol;


	symbol = checkIdentifier(identifier());

	if (lookahead == '(') {
	    match('(');

	    if (lookahead != ')') {
		args.push_back(expression());

		while (lookahead == ',') {
		    match(',');
		    args.push_back(expression());
		}
	    }

	    expr = checkCall(symbol, args);
	    match(')');

	} else
	    expr = new Identifier(symbol);

    } else {
	expr = nullptr;
	error();
    }

    return expr;
}


/*
 * Function:	postfixExpression
 *
 * Description:	Parse a postfix expression.
 *
 *		postfix-expression:
 *		  primary-expression
 *		  postfix-expression [ expression ]
 */

static Expression *postfixExpression()
{
    Expression *left, *right;


    left = primaryExpression();

    while (lookahead == '[') {
	match('[');
	right = expression();
	left = checkArray(left, right);
	match(']');
    }

    return left;
}


/*
 * Function:	prefixExpression
 *
 * Description:	Parse a prefix expression.
 *
 *		prefix-expression:
 *		  postfix-expression
 *		  ! prefix-expression
 *		  - prefix-expression
 *		  * prefix-expression
 *		  & prefix-expression
 *		  sizeof prefix-expression
 *		  sizeof ( specifier pointers )
 *		  ( specifier pointers ) prefix-expression
 *
 *		This grammar is still ambiguous since "sizeof(type) * n"
 *		could be interpreted as a multiplicative expression or as a
 *		cast of a dereference.  The correct interpretation is the
 *		former, as the latter makes little sense semantically.  We
 *		resolve the ambiguity here by always consuming the "(type)"
 *		as part of the sizeof expression.
 */

static Expression *prefixExpression()
{
    Expression *expr;
    unsigned indirection;
    int typespec;


    if (lookahead == '!') {
	match('!');
	expr = prefixExpression();
	expr = checkNot(expr);

    } else if (lookahead == '-') {
	match('-');
	expr = prefixExpression();
	expr = checkNegate(expr);

    } else if (lookahead == '*') {
	match('*');
	expr = prefixExpression();
	expr = checkDereference(expr);

    } else if (lookahead == '&') {
	match('&');
	expr = prefixExpression();
	expr = checkAddress(expr);

    } else if (lookahead == SIZEOF) {
	match(SIZEOF);

	if (lookahead == '(' && isSpecifier(peek())) {
	    match('(');
	    typespec = specifier();
	    indirection = pointers();
	    expr = new Number(Type(typespec, indirection).size());
	    match(')');
	} else {
	    expr = prefixExpression();
	    expr = checkSizeof(expr);
	}

    } else if (lookahead == '(' && isSpecifier(peek())) {
	match('(');
	typespec = specifier();
	indirection = pointers();
	match(')');
	expr = prefixExpression();
	expr = checkCast(Type(typespec, indirection), expr);

    } else
	expr = postfixExpression();

    return expr;
}


/*
 * Function:	multiplicativeExpression
 *
 * Description:	Parse a multiplicative expression.
 *
 *		multiplicative-expression:
 *		  prefix-expression
 *		  multiplicative-expression * prefix-expression
 *		  multiplicative-expression / prefix-expression
 *		  multiplicative-expression % prefix-expression
 */

static Expression *multiplicativeExpression()
{
    Expression *left, *right;


    left = prefixExpression();

    while (1) {
	if (lookahead == '*') {
	    match('*');
	    right = prefixExpression();
	    left = checkMultiply(left, right);

	} else if (lookahead == '/') {
	    match('/');
	    right = prefixExpression();
	    left = checkDivide(left, right);

	} else if (lookahead == '%') {
	    match('%');
	    right = prefixExpression();
	    left = checkRemainder(left, right);

	} else
	    break;
    }

    return left;
}


/*
 * Function:	additiveExpression
 *
 * Description:	Parse an additive expression.
 *
 *		additive-expression:
 *		  multiplicative-expression
 *		  additive-expression + multiplicative-expression
 *		  additive-expression - multiplicative-expression
 */

static Expression *additiveExpression()
{
    Expression *left, *right;


    left = multiplicativeExpression();

    while (1) {
	if (lookahead == '+') {
	    match('+');
	    right = multiplicativeExpression();
	    left = checkAdd(left, right);

	} else if (lookahead == '-') {
	    match('-');
	    right = multiplicativeExpression();
	    left = checkSubtract(left, right);

	} else
	    break;
    }

    return left;
}


/*
 * Function:	relationalExpression
 *
 * Description:	Parse a relational expression.  Note that Simple C does not
 *		have shift operators, so we go immediately to additive
 *		expressions.
 *
 *		relational-expression:
 *		  additive-expression
 *		  relational-expression < additive-expression
 *		  relational-expression > additive-expression
 *		  relational-expression <= additive-expression
 *		  relational-expression >= additive-expression
 */

static Expression *relationalExpression()
{
    Expression *left, *right;


    left = additiveExpression();

    while (1) {
	if (lookahead == '<') {
	    match('<');
	    right = additiveExpression();
	    left = checkLessThan(left, right);

	} else if (lookahead == '>') {
	    match('>');
	    right = additiveExpression();
	    left = checkGreaterThan(left, right);

	} else if (lookahead == LEQ) {
	    match(LEQ);
	    right = additiveExpression();
	    left = checkLessOrEqual(left, right);

	} else if (lookahead == GEQ) {
	    match(GEQ);
	    right = additiveExpression();
	    left = checkGreaterOrEqual(left, right);

	} else
	    break;
    }

    return left;
}


/*
 * Function:	equalityExpression
 *
 * Description:	Parse an equality expression.
 *
 *		equality-expression:
 *		  relational-expression
 *		  equality-expression == relational-expression
 *		  equality-expression != relational-expression
 */

static Expression *equalityExpression()
{
    Expression *left, *right;


    left = relationalExpression();

    while (1) {
	if (lookahead == EQL) {
	    match(EQL);
	    right = relationalExpression();
	    left = checkEqual(left, right);

	} else if (lookahead == NEQ) {
	    match(NEQ);
	    right = relationalExpression();
	    left = checkNotEqual(left, right);

	} else
	    break;
    }

    return left;
}


/*
 * Function:	logicalAndExpression
 *
 * Description:	Parse a logical-and expression.  Note that Simple C does
 *		not have bitwise-and expressions.
 *
 *		logical-and-expression:
 *		  equality-expression
 *		  logical-and-expression && equality-expression
 */

static Expression *logicalAndExpression()
{
    Expression *left, *right;


    left = equalityExpression();

    while (lookahead == AND) {
	match(AND);
	right = equalityExpression();
	left = checkLogicalAnd(left, right);
    }

    return left;
}


/*
 * Function:	expression
 *
 * Description:	Parse an expression, or more specifically, a logical-or
 *		expression, since Simple C does not allow comma or
 *		assignment as an expression operator.
 *
 *		expression:
 *		  logical-and-expression
 *		  expression || logical-and-expression
 */

static Expression *expression()
{
    Expression *left, *right;


    left = logicalAndExpression();

    while (lookahead == OR) {
	match(OR);
	right = logicalAndExpression();
	left = checkLogicalOr(left, right);
    }

    return left;
}


/*
 * Function:	statements
 *
 * Description:	Parse a possibly empty sequence of statements.  Rather than
 *		checking if the next token starts a statement, we check if
 *		the next token ends the sequence, since a sequence of
 *		statements is always terminated by a closing brace.
 *
 *		statements:
 *		  empty
 *		  statement statements
 */

static Statements statements()
{
    Statements stmts;


    while (lookahead != '}')
	stmts.push_back(statement());

    return stmts;
}


/*
 * Function:	assignment
 *
 * Description:	Parse an assignment statement.
 *
 *		assignment:
 *		  expression = expression
 *		  expression
 */

static Statement *assignment()
{
    Expression *expr;


    expr = expression();

    if (lookahead == '=') {
	match('=');
	return checkAssignment(expr, expression());
    }

    return new Simple(expr);
}


/*
 * Function:	statement
 *
 * Description:	Parse a statement.  Note that Simple C has so few
 *		statements that we handle them all in this one function.
 *
 *		statement:
 *		  { declarations statements }
 *		  break ;
 *		  return expression ;
 *		  while ( expression ) statement
 *		  for ( assignment ; expression ; assignment ) statement
 *		  if ( expression ) statement
 *		  if ( expression ) statement else statement
 *		  assignment ;
 */

static Statement *statement()
{
    Scope *scope;
    Expression *expr;
    Statement *stmt, *init, *incr;
    Statements stmts;


    if (lookahead == '{') {
	match('{');
	openScope();
	declarations();
	stmts = statements();
	scope = closeScope();
	match('}');
	return new Block(scope, stmts);

    } else if (lookahead == BREAK) {
	match(BREAK);
	stmt = checkBreak(loopDepth);
	match(';');
	return stmt;

    } else if (lookahead == RETURN) {
	match(RETURN);
	expr = expression();
	stmt = checkReturn(expr, returnType);
	match(';');
	return stmt;

    } else if (lookahead == WHILE) {
	match(WHILE);
	match('(');
	expr = expression();
	expr = checkTest(expr);
	match(')');
	loopDepth ++;
	stmt = statement();
	loopDepth --;
	return new While(expr, stmt);

    } else if (lookahead == FOR) {
	match(FOR);
	match('(');
	init = assignment();
	match(';');
	expr = expression();
	expr = checkTest(expr);
	match(';');
	incr = assignment();
	match(')');
	loopDepth ++;
	stmt = statement();
	loopDepth --;
	return new For(init, expr, incr, stmt);

    } else if (lookahead == IF) {
	match(IF);
	match('(');
	expr = expression();
	expr = checkTest(expr);
	match(')');
	stmt = statement();

	if (lookahead != ELSE)
	    return new If(expr, stmt, nullptr);

	match(ELSE);
	return new If(expr, stmt, statement());

    } else {
	stmt = assignment();
	match(';');
	return stmt;
    }
}


/*
 * Function:	parameter
 *
 * Description:	Parse a parameter, which in Simple C is always a scalar
 *		variable with optional pointer declarators.
 *
 *		parameter:
 *		  specifier pointers identifier
 */

static Type parameter()
{
    string name;
    int typespec;
    unsigned indirection;


    typespec = specifier();
    indirection = pointers();
    name = identifier();

    declareVariable(name, Type(typespec, indirection));
    return Type(typespec, indirection);
}


/*
 * Function:	parameters
 *
 * Description:	Parse the parameters of a function, but not the opening or
 *		closing parentheses.
 *
 *		parameters:
 *		  void
 *		  parameter-list
 *		  parameter-list , ...
 */

static Parameters *parameters()
{
    Parameters *params;


    params = new Parameters();
    params->variadic = false;

    if (lookahead == VOID)
	match(VOID);

    else {
	params->types.push_back(parameter());

	while (lookahead == ',') {
	    match(',');

	    if (lookahead == ELLIPSIS) {
		params->variadic = true;
		match(ELLIPSIS);
		break;
	    }

	    params->types.push_back(parameter());
	}
    }

    return params;
}


/*
 * Function:	globalDeclarator
 *
 * Description:	Parse a declarator, which in Simple C is either a scalar
 *		variable, an array, or a function, with optional pointer
 *		declarators.
 *
 *		global-declarator:
 *		  pointers identifier
 *		  pointers identifier [ num ]
 *		  pointers identifier ( parameters )
 */

static void globalDeclarator(int typespec)
{
    string name;
    unsigned indirection;


    indirection = pointers();
    name = identifier();

    if (lookahead == '(') {
	match('(');
	openScope();
	declareFunction(name, Type(typespec, indirection, parameters()));
	closeScope(true);
	match(')');

    } else if (lookahead == '[') {
	match('[');
	declareVariable(name, Type(typespec, indirection, number()));
	match(']');

    } else
	declareVariable(name, Type(typespec, indirection));
}


/*
 * Function:	functionOrGlobal
 *
 * Description:	Parse a function definition or global declaration.
 *
 * 		function-or-global:
 * 		  specifier function-declarator { declarations statements }
 * 		  specifier global-declarator , global-declarator-list ;
 * 		  specifier global-declarator ;
 *
 * 		global-declarator-list:
 * 		  global-declarator
 * 		  global-declarator , global-declarator-list
 */

static void functionOrGlobal()
{
    string name;
    int typespec;
    unsigned indirection;
    Parameters *params;
    Statements stmts;
    Function *function;
    Symbol *symbol;
    Scope *decls;


    typespec = specifier();
    indirection = pointers();
    name = identifier();

    if (lookahead == '(') {
	match('(');
	openScope();
	params = parameters();
	match(')');

	if (lookahead == '{') {
	    returnType = Type(typespec, indirection);
	    symbol = defineFunction(name, Type(typespec, indirection, params));
	    match('{');
	    declarations();
	    stmts = statements();
	    decls = closeScope();
	    function = new Function(symbol, new Block(decls, stmts));
	    match('}');

	    if (numerrors == 0)
		function->generate();

	    return;
	}

	declareFunction(name, Type(typespec, indirection, params));
	closeScope(true);

    } else if (lookahead == '[') {
	match('[');
	declareVariable(name, Type(typespec, indirection, number()));
	match(']');

    } else
	declareVariable(name, Type(typespec, indirection));

    while (lookahead == ',') {
	match(',');
	globalDeclarator(typespec);
    }

    match(';');
}


/*
 * Function:	main
 *
 * Description:	Analyze the standard input stream.
 *
 *		translation-unit:
 *		  empty
 *		  function-or-global translation-unit
 */

int main()
{
    openScope();
    lookahead = yylex();
    lexbuf = yytext;

    while (lookahead != DONE)
	functionOrGlobal();

    generateGlobals(closeScope());
    exit(EXIT_SUCCESS);
}
