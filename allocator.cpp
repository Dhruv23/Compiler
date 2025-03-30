/*
 * File:	allocator.cpp
 *
 * Description:	This file contains the member function definitions for
 *		functions dealing with storage allocation.  The actual
 *		classes are declared elsewhere, mainly in Tree.h.
 *
 *		Extra functionality:
 *		- maintaining minimum offset in nested blocks
 *		- allocation within statements
 *		- computing alignment (necessary on some systems)
 */

# include <cassert>
# include <iostream>
# include "checker.h"
# include "machine.h"
# include "tokens.h"
# include "Tree.h"

using namespace std;


/*
 * Function:	Type::size
 *
 * Description:	Return the size of a type in bytes.
 */

unsigned long Type::size() const
{
    unsigned long count;


    assert(_kind != FUNCTION && _kind != ERROR);
    count = (_kind == ARRAY ? _length : 1);

    if (_indirection > 0)
	return count * SIZEOF_PTR;

    if (_specifier == CHAR)
	return count * SIZEOF_CHAR;

    if (_specifier == INT)
	return count * SIZEOF_INT;

    if (_specifier == LONG)
	return count * SIZEOF_LONG;

    return 0;
}


/*
 * Function:	Type::alignment
 *
 * Description:	Return the alignment of a type in bytes.
 */

unsigned Type::alignment() const
{
    assert(_kind != FUNCTION && _kind != ERROR);

    if (_indirection > 0)
	return ALIGNOF_PTR;

    if (_specifier == CHAR)
	return ALIGNOF_CHAR;

    if (_specifier == INT)
	return ALIGNOF_INT;

    if (_specifier == LONG)
	return ALIGNOF_LONG;

    return 0;
}


/*
 * Function:	Block::allocate
 *
 * Description:	Allocate storage for this block.  We assign decreasing
 *		offsets for all symbols declared within this block, and
 *		then for all symbols declared within any nested block.
 *		Only symbols that have not already been allocated an offset
 *		will be assigned one, since the parameters are already
 *		assigned special offsets.
 */

void Block::allocate(int &offset) const
{
    int temp, saved;
    const Symbols &symbols = _decls->symbols();


    for (auto symbol : symbols)
	if (symbol->offset == 0) {
	    offset -= symbol->type().size();
	    symbol->offset = offset;
	}

    saved = offset;

    for (auto stmt : _stmts) {
	temp = saved;
	stmt->allocate(temp);
	offset = min(offset, temp);
    }
}


/*
 * Function:	While::allocate
 *
 * Description:	Allocate storage for this while statement, which
 *		essentially means allocating storage for variables declared
 *		as part of its statement.
 */

void While::allocate(int &offset) const
{
    _stmt->allocate(offset);
}


/*
 * Function:	For::allocate
 *
 * Description:	Allocate storage for this for statement, which
 *		essentially means allocating storage for variables declared
 *		as part of its statement.
 */

void For::allocate(int &offset) const
{
    _stmt->allocate(offset);
}


/*
 * Function:	If::allocate
 *
 * Description:	Allocate storage for this if-then or if-then-else
 *		statement, which essentially means allocating storage for
 *		variables declared as part of its statements.
 */

void If::allocate(int &offset) const
{
    int saved, temp;


    saved = offset;
    _thenStmt->allocate(offset);

    if (_elseStmt != nullptr) {
	temp = saved;
	_elseStmt->allocate(temp);
	offset = min(offset, temp);
    }
}


/*
 * Function:	Function::allocate
 *
 * Description:	Allocate storage for this function and return the number of
 *		bytes required.  The parameters are allocated offsets as
 *		well, starting with the given offset.  This function is
 *		designed to work with both 32-bit and 64-bit Intel
 *		architectures, with or without callee-saved registers.
 *
 *		32-bit Intel/Linux:
 *		  PARAM_ALIGNMENT = 4 (each parameter is a multiple of 4 bytes)
 *		  NUM_PARAM_REGS = 0 (all parameters are on the stack)
 *
 *		64-bit Intel/Linux:
 *		  PARAM_ALIGNMENT = 8 (each parameter is a multiple of 8 bytes)
 *		  NUM_PARAM_REGS = 6 (first six parameters are in registers)
 *
 *		The initial value of the offset should be offset of the
 *		first parameter on the stack.  Normally, this value is the
 *		size of two registers (the instruction pointer and the base
 *		pointer), but would be larger if additional callee-saved
 *		registers were used.
 */

void Function::allocate(int &offset) const
{
    Types types = _id->type().parameters()->types;
    const Symbols &symbols = _body->declarations()->symbols();

    for (unsigned i = NUM_PARAM_REGS; i < types.size(); i ++) {
	symbols[i]->offset = offset;
	offset += types[i].size();

	while (offset % PARAM_ALIGNMENT != 0)
	    offset ++;
    }

    offset = 0;

    for (unsigned i = 0; i < NUM_PARAM_REGS; i ++)
	if (i < types.size()) {
	    offset -= types[i].size();
	    symbols[i]->offset = offset;
	} else
	    break;

    _body->allocate(offset);
}
