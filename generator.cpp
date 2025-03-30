/*
 * File:	generator.cpp
 *
 * Description:	This file contains the public and member function
 *		definitions for the code generator for Simple C.
 *
 *		Extra functionality:
 *		- putting all the global declarations at the end
 *		- prefix and suffix for globals (required on some systems)
 */

#include <vector>
#include <map>
#include <cassert>
#include <stack>
#include <iostream>
#include "generator.h"
#include "machine.h"
#include "Tree.h"
#include "string.h"
using namespace std;

static int offset;
static string funcname, tab = "\t";
static string suffix(Expression *expr);
static ostream &operator<<(ostream &ostr, Expression *expr);

static Register *rax = new Register("%rax", "%eax", "%al");
static Register *rbx = new Register("%rbx", "%ebx", "%bl");
static Register *rcx = new Register("%rcx", "%ecx", "%cl");
static Register *rdx = new Register("%rdx", "%edx", "%dl");
static Register *rsi = new Register("%rsi", "%esi", "%sil");
static Register *rdi = new Register("%rdi", "%edi", "%dil");
static Register *r8 = new Register("%r8", "%r8d", "%r8b");
static Register *r9 = new Register("%r9", "%r9d", "%r9b");
static Register *r10 = new Register("%r10", "%r10d", "%r10b");
static Register *r11 = new Register("%r11", "%r11d", "%r11b");
static Register *r12 = new Register("%r12", "%r12d", "%r12b");
static Register *r13 = new Register("%r13", "%r13d", "%r13b");
static Register *r14 = new Register("%r14", "%r14d", "%r14b");
static Register *r15 = new Register("%r15", "%r15d", "%r15b");

static vector<Register *> parameters = {rdi, rsi, rdx, rcx, r8, r9};
static vector<Register *> registers = {rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11};

static map<string, Label> strings;
static stack<Label> stack_label;

/* These will be replaced with functions in the next phase.  They are here
   as placeholders so that Call::generate() is finished. */

void assign(Expression *expr, Register *reg)
{
    if (expr != nullptr)
    {
        if (expr->reg != nullptr)
            expr->reg->node = nullptr;
        expr->reg = reg;
    }
    if (reg != nullptr)
    {
        if (reg->node != nullptr)
            reg->node->reg = nullptr;
        reg->node = expr;
    }
}

// void load(Expression *expr, Register *reg)
// {
//     if (expr != nullptr)
//     {
//         cout << tab << "mov" << suffix(expr) << expr << ", ";
//         cout << reg->name(expr->type().size()) << endl;
//     }
// }
void load(Expression *expr, Register *reg)
{
    if (reg->node != expr)
    {
        if (reg->node != nullptr)
        {
            offset -= reg->node->type().size();
            reg->node->offset = offset;
            cout << "\tmov" << suffix(reg->node) << reg << ", " << offset << "(%rbp)\n";
        }
        if (expr != nullptr)
        {
            unsigned size = expr->type().size();
            cout << "\tmov" << suffix(expr) << expr << ", " << reg->name(size) << endl; // Add "b, l, q" based on size of expr
        }
        assign(expr, reg);
    }
}
Register *getreg()
{
    for (auto reg : registers)
        if (reg->node == nullptr)
            return reg;
    load(nullptr, registers[0]);
    return registers[0];
}

/*
 * Function:	sign_extend_byte_arg (private)
 *
 * Description:	Sign extend a byte argument to 32 bits.  The Microsoft
 *		calling conventions explicitly state that parameters less
 *		than 64 bits long are not zero extended.  The System V
 *		conventions used for Unix-like systems do not specify what
 *		happens, but gcc and clang do sign extend, and clang
 *		apparently relies on it, but icc does not sign extend.
 *
 *		Writing to the 32 bit register will zero the upper 32-bits
 *		of the 64-bit register.  So in effect, an 8-bit value
 *		written to %al is sign extended into %eax but then zero
 *		extended into %rax.
 */

void sign_extend_byte_arg(Expression *arg)
{
    if (arg->type().size() == 1)
    {
        cout << tab << "movsbl" << tab << arg << ", ";
        cout << arg->reg->name(4) << endl;
    }
}

/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the given size.
 */

static string suffix(unsigned long size)
{
    return size == 1 ? "b\t" : (size == 4 ? "l\t" : "q\t");
}

/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the size of the
 *		given expression.
 */

static string suffix(Expression *expr)
{
    return suffix(expr->type().size());
}

/*
 * Function:	align (private)
 *
 * Description:	Return the number of bytes necessary to align the given
 *		offset on the stack.
 */

static int align(int offset)
{
    if (offset % STACK_ALIGNMENT == 0)
        return 0;

    return STACK_ALIGNMENT - (abs(offset) % STACK_ALIGNMENT);
}

/*
 * Function:	operator << (private)
 *
 * Description:	Convenience function for writing the operand of an
 *		expression using the output stream operator.
 */

static ostream &operator<<(ostream &ostr, Expression *expr)
{
    if (expr->reg != nullptr)
        return ostr << expr->reg;

    expr->operand(ostr);
    return ostr;
}

/*
 * Function:	Expression::operand
 *
 * Description:	Write an expression as an operand to the specified stream.
 */

void Expression::operand(ostream &ostr) const
{
    assert(offset != 0);
    ostr << offset << "(%rbp)";
}

/*
 * Function:	Identifier::operand
 *
 * Description:	Write an identifier as an operand to the specified stream.
 */

void Identifier::operand(ostream &ostr) const
{
    if (_symbol->offset == 0)
        ostr << global_prefix << _symbol->name() << global_suffix;
    else
        ostr << _symbol->offset << "(%rbp)";
}

/*
 * Function:	Number::operand
 *
 * Description:	Write a number as an operand to the specified stream.
 */

void Number::operand(ostream &ostr) const
{
    ostr << "$" << _value;
}

/*
 * Function:	Call::generate
 *
 * Description:	Generate code for a function call expression.
 *
 *		On a 64-bit platform, the stack needs to be aligned on a
 *		16-byte boundary.  So, if the stack will not be aligned
 *		after pushing any arguments, we first adjust the stack
 *		pointer.
 *
 *		Since all arguments are 8-bytes wide, we could simply do:
 *
 *		    if (args.size() > 6 && args.size() % 2 != 0)
 *			subq $8, %rsp
 */

void Call::generate()
{
    unsigned numBytes;

    /* Generate code for the arguments first. */

    numBytes = 0;

    for (int i = _args.size() - 1; i >= 0; i--)
        _args[i]->generate();

    /* Adjust the stack if necessary */

    if (_args.size() > NUM_PARAM_REGS)
    {
        numBytes = align((_args.size() - NUM_PARAM_REGS) * PARAM_ALIGNMENT);

        if (numBytes > 0)
            cout << tab << "subq" << tab << "$" << numBytes << ", %rsp" << endl;
    }

    /* Move the arguments into the correct registers or memory locations. */

    for (int i = _args.size() - 1; i >= 0; i--)
    {
        if (i >= NUM_PARAM_REGS)
        {
            numBytes += PARAM_ALIGNMENT;
            load(_args[i], rax);
            sign_extend_byte_arg(_args[i]);
            cout << tab << "pushq" << tab << "%rax" << endl;
        }
        else
        {
            load(_args[i], parameters[i]);
            sign_extend_byte_arg(_args[i]);
        }

        assign(_args[i], nullptr);
    }

    /* Call the function and then reclaim the stack space.  We only need to
       assign the number of floating point arguments passed in vector
       registers to %eax if the function being called takes a variable
       number of arguments. */

    for (auto reg : registers)
        load(nullptr, reg);

    if (_id->type().parameters()->variadic)
        cout << tab << "movl" << tab << "$0, %eax" << endl;

    cout << tab << "call" << tab << global_prefix << _id->name() << endl;

    if (numBytes > 0)
        cout << tab << "addq" << tab << "$" << numBytes << ", %rsp" << endl;

    assign(this, rax);
}

/*
 * Function:	Block::generate
 *
 * Description:	Generate code for this block, which simply means we
 *		generate code for each statement within the block.
 */

void Block::generate()
{
    for (auto stmt : _stmts)
    {
        stmt->generate();

        for (auto reg : registers)
            assert(reg->node == nullptr);
    }
}

/*
 * Function:	Simple::generate
 *
 * Description:	Generate code for a simple (expression) statement, which
 *		means simply generating code for the expression.
 */

void Simple::generate()
{
    _expr->generate();
    assign(_expr, nullptr);
}

/*
 * Function:	Function::generate
 *
 * Description:	Generate code for this function, which entails allocating
 *		space for local variables, then emitting our prologue, the
 *		body of the function, and the epilogue.
 */

void Function::generate()
{
    int param_offset;
    unsigned size;
    Symbols symbols;
    Types types;

    /* Assign offsets to the parameters and local variables. */

    param_offset = 2 * SIZEOF_REG;
    offset = param_offset;
    allocate(offset);

    /* Generate our prologue. */

    funcname = _id->name();
    cout << global_prefix << funcname << ":" << endl;
    cout << tab << "pushq" << tab << "%rbp" << endl;
    cout << tab << "movq" << tab << "%rsp, %rbp" << endl;
    cout << tab << "movl" << tab << "$" << funcname << ".size, %eax" << endl;
    cout << tab << "subq" << tab << "%rax, %rsp" << endl;

    /* Spill any parameters. */

    types = _id->type().parameters()->types;
    symbols = _body->declarations()->symbols();

    for (unsigned i = 0; i < NUM_PARAM_REGS; i++)
        if (i < types.size())
        {
            size = symbols[i]->type().size();
            cout << tab << "mov" << suffix(size) << parameters[i]->name(size);
            cout << ", " << symbols[i]->offset << "(%rbp)" << endl;
        }
        else
            break;

    /* Generate the body of this function. */

    _body->generate();

    /* Generate our epilogue. */

    cout << endl
         << global_prefix << funcname << ".exit:" << endl;
    cout << tab << "movq" << tab << "%rbp, %rsp" << endl;
    cout << tab << "popq" << tab << "%rbp" << endl;
    cout << tab << "ret" << endl
         << endl;

    offset -= align(offset - param_offset);
    cout << tab << ".set" << tab << funcname << ".size, " << -offset << endl;
    cout << tab << ".globl" << tab << global_prefix << funcname << endl
         << endl;
}

/*
 * Function:	generateGlobals
 *
 * Description:	Generate code for any global variable declarations.
 */

void generateGlobals(Scope *scope)
{
    const Symbols &symbols = scope->symbols();

    for (auto symbol : symbols)
        if (!symbol->type().isFunction())
        {
            cout << tab << ".comm" << tab << global_prefix << symbol->name();
            cout << ", " << symbol->type().size() << endl;
        }
    cout << "\t.data\n";
    for (auto &[stringName, label] : strings)
    {
        cout << label << ":\t" << ".asciz\t \"" << escapeString(stringName) << "\"\n";
    }
}

/*
 * Function:	Assignment::generate
 *
 * Description:	Generate code for an assignment statement.
 *
 *		NOT FINISHED: Only works if the right-hand side is an
 *		integer literal and the left-hand side is an integer
 *		scalar.
 */

void Assignment::generate()
{
    cout << "\n# Assignment generate \n";

    Expression *pointer;

    _right->generate();

    if (_left->isDereference(pointer))
    {
        cout << "\n# left deref \n";

        cout << "\n# pointer generate before \n";
        pointer->generate();
        cout << "\n# pointer generate after \n";

        if (pointer->reg == nullptr)
        {
            load(pointer, getreg());
        }
        if (_right->reg == nullptr)
        {
            load(_right, getreg());
        }
        cout << "\tmov" << suffix(_right) << _right << ", (" << pointer->reg << ")" << endl;
        assign(_right, nullptr);
        assign(pointer, nullptr);
    }
    else
    {
        cout << "\n# left not deref \n";
        if (_right->reg == nullptr)
        {
            load(_right, getreg());
        }
        cout << tab << "mov" << suffix(_right) << _right << ", " << _left << endl;
        assign(_right, nullptr);
    }
}

void Add::generate()
{
    // generate left and right
    _left->generate();
    _right->generate();
    // if left not in reg then load it
    if (_left->reg == nullptr)
        load(_left, getreg());
    // output assembly code
    cout << "\tadd" << suffix(_left);
    cout << _right << ", " << _left << endl;
    // deallocate right and store output in this
    assign(_right, nullptr);
    assign(this, _left->reg);
}

void Subtract::generate()
{
    // generate left and right
    _left->generate();
    _right->generate();
    // if left not in reg then load it
    if (_left->reg == nullptr)
        load(_left, getreg());
    // output assembly code
    cout << "\tsub" << suffix(_left);
    cout << _right << ", " << _left << endl;
    // deallocate right and store output in this
    assign(_right, nullptr);
    assign(this, _left->reg);
}

void Multiply::generate()
{
    // generate left and right
    _left->generate();
    _right->generate();
    // if left not in reg then load it
    if (_left->reg == nullptr)
        load(_left, getreg());
    // output assembly code
    cout << "\timul" << suffix(_left);
    cout << _right << ", " << _left << endl;
    // deallocate right and store output in this
    assign(_right, nullptr);
    assign(this, _left->reg);
}

void Divide::generate()
{
    // generate left and right
    _left->generate();
    _right->generate();
    // Load left into rax
    load(_left, rax);
    // Unload rdx
    load(nullptr, rdx);
    // Load right into rcx
    load(_right, rcx);
    // Sign extend rax into rdx
    if (suffix(_left) == "l\t")
    {
        cout << tab << "cltd" << endl;
    }
    else if (suffix(_left) == "q\t")
    {
        cout << tab << "cqto" << endl;
    }
    // idiv (with proper suffix) right; don’t forget to call Dr. Atkinson’s provided “suffix()”!
    cout << tab << "idiv" << suffix(_left) << _right << endl;
    assign(_right, nullptr);
    assign(_left, nullptr);
    assign(this, rax);
}

void Remainder::generate()
{
    // generate left and right
    _left->generate();
    _right->generate();
    // Load left into rax
    load(_left, rax);
    // Unload rdx
    load(nullptr, rdx);
    // Load right into rcx
    load(_right, rcx);
    // Sign extend rax into rdx
    if (suffix(_left) == "l\t")
    {
        cout << tab << "cltd" << endl;
    }
    else if (suffix(_left) == "q\t")
    {
        cout << tab << "cqto" << endl;
    }
    // idiv (with proper suffix) right; don’t forget to call Dr. Atkinson’s provided “suffix()”!
    cout << tab << "idiv" << suffix(_left) << _right << endl;
    assign(_right, nullptr);
    assign(_left, nullptr);
    assign(this, rdx);
}
void LogicalOr::generate()
{
    Label trueL, falseL;
    // Generate left and right
    _left->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << "$0" << ", " << _left << endl;
    cout << tab << "jne\t" << trueL << endl;
    assign(_left, nullptr);

    _right->generate();
    if (_right->reg == nullptr)
        load(_right, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_right) << "$0" << ", " << _right << endl;
    cout << tab << "jne\t" << trueL << endl;
    assign(_right, nullptr);

    assign(this, getreg());
    cout << tab << "movl " << "$0, " << this << endl;
    cout << tab << "jmp\t" << falseL << endl;
    cout << trueL << ": \n";
    cout << "movl " << "$1, " << this << endl;
    cout << falseL << ": \n";
}
void LogicalAnd::generate()
{
    Label trueL, falseL;
    // Generate left and right
    _left->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << "$0" << ", " << _left << endl;
    cout << tab << "je\t" << falseL << endl;
    assign(_left, nullptr);

    _right->generate();
    if (_right->reg == nullptr)
        load(_right, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_right) << "$0" << ", " << _right << endl;
    cout << tab << "je\t" << falseL << endl;
    assign(_right, nullptr);

    assign(this, getreg());
    cout << tab << "movl " << "$1, " << this << endl;
    cout << tab << "jmp\t" << trueL << endl;
    cout << falseL << ": \n";
    cout << "movl " << "$0, " << this << endl;
    cout << trueL << ": \n";
}
void LessThan::generate()
{
    // Generate left and right
    _left->generate();
    _right->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << _right << ", " << _left << endl;
    // Unassign left and right
    assign(_right, nullptr);
    assign(_left, nullptr);
    // Assign this to a register
    assign(this, getreg());
    // Store result of condition code in byte register
    cout << tab << "setl " << this->reg->byte() << endl;
    // Zero-extend byte (using “movzb” + suffix)
    cout << tab << "movzb" << suffix(this) << this->reg->byte() << ", " << this << endl;
}

void GreaterThan::generate()
{
    // Generate left and right
    _left->generate();
    _right->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << _right << ", " << _left << endl;
    // Unassign left and right
    assign(_right, nullptr);
    assign(_left, nullptr);
    // Assign this to a register
    assign(this, getreg());
    // Store result of condition code in byte register
    cout << tab << "setg " << this->reg->byte() << endl;
    // Zero-extend byte (using “movzb” + suffix)
    cout << tab << "movzb" << suffix(this) << this->reg->byte() << ", " << this << endl;
}

void LessOrEqual::generate()
{
    // Generate left and right
    _left->generate();
    _right->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << _right << ", " << _left << endl;
    // Unassign left and right
    assign(_right, nullptr);
    assign(_left, nullptr);
    // Assign this to a register
    assign(this, getreg());
    // Store result of condition code in byte register
    cout << tab << "setle " << this->reg->byte() << endl;
    // Zero-extend byte (using “movzb” + suffix)
    cout << tab << "movzb" << suffix(this) << this->reg->byte() << ", " << this << endl;
}

void GreaterOrEqual::generate()
{
    // Generate left and right
    _left->generate();
    _right->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << _right << ", " << _left << endl;
    // Unassign left and right
    assign(_right, nullptr);
    assign(_left, nullptr);
    // Assign this to a register
    assign(this, getreg());
    // Store result of condition code in byte register
    cout << tab << "setge " << this->reg->byte() << endl;
    // Zero-extend byte (using “movzb” + suffix)
    cout << tab << "movzb" << suffix(this) << this->reg->byte() << ", " << this << endl;
}

void Equal::generate()
{
    // Generate left and right
    _left->generate();
    _right->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << _right << ", " << _left << endl;
    // Unassign left and right
    assign(_right, nullptr);
    assign(_left, nullptr);
    // Assign this to a register
    assign(this, getreg());
    // Store result of condition code in byte register
    cout << tab << "sete " << this->reg->byte() << endl;
    // Zero-extend byte (using “movzb” + suffix)
    cout << tab << "movzb" << suffix(this) << this->reg->byte() << ", " << this << endl;
}
void NotEqual::generate()
{
    // Generate left and right
    _left->generate();
    _right->generate();
    // Load left (if not already in a register)
    if (_left->reg == nullptr)
        load(_left, getreg());
    // Compare left and right
    cout << tab << "cmp" << suffix(_left) << _right << ", " << _left << endl;
    // Unassign left and right
    assign(_right, nullptr);
    assign(_left, nullptr);
    // Assign this to a register
    assign(this, getreg());
    // Store result of condition code in byte register
    cout << tab << "setne " << this->reg->byte() << endl;
    // Zero-extend byte (using “movzb” + suffix)
    cout << tab << "movzb" << suffix(this) << this->reg->byte() << ", " << this << endl;
}

void Not::generate()
{
    // Start with generate and load expression
    _expr->generate();
    if (_expr->reg == nullptr)
    {
        load(_expr, getreg());
    }

    // not

    cout << tab << "cmp" << suffix(_expr) << "$0, " << _expr << endl;
    assign(_expr, nullptr);
    // End with assign result expression to register
    assign(this, getreg());

    cout << tab << "sete\t" << this->reg->byte() << endl;
    cout << tab << "movzbl\t" << this->reg->byte() << ", " << this << endl;
}
void Negate::generate()
{
    // Start with generate and load expression
    _expr->generate();
    if (_expr->reg == nullptr)
    {
        load(_expr, getreg());
    }

    // negate

    cout << tab << "neg" << suffix(_expr) << _expr << endl;

    assign(this, _expr->reg);
}

void Expression::test(const Label &label, bool ifTrue)
{
    generate();
    if (reg == nullptr)
        load(this, getreg());
    cout << "\tcmp" << suffix(this) << "$0, " << this << endl;
    cout << (ifTrue ? "\tjne\t" : "\tje\t") << label << endl;
    assign(this, nullptr);
}

void While::generate()
{
    cout << "# WHILE GENERATE" << endl;
    Label loop, exit;
    stack_label.push(exit);
    cout << loop << ":" << endl;
    _expr->test(exit, false);
    _stmt->generate();
    cout << "\tjmp\t" << loop << endl;
    cout << exit << ":" << endl;
    stack_label.pop();
}

void Address::generate()
{
    Expression *pointer;
    if (_expr->isDereference(pointer))
    {
        pointer->generate();
        if (pointer->reg == nullptr)
        {
            load(pointer, getreg());
        }
        assign(this, pointer->reg);
    }
    else
    {
        assign(this, getreg());
        cout << "\tleaq\t" << _expr << ", " << this << endl;
    }
}

void Dereference::generate()
{
    _expr->generate();
    if (_expr->reg == nullptr)
    {
        load(_expr, getreg());
    }

    cout << "\tmov" << suffix(this) << "(" << _expr->reg << "), " << _expr->reg->name(_type.size()) << endl;
    assign(this, _expr->reg);
}
void String::operand(ostream &ostr) const
{
    if (strings.find(this->value()) == strings.end())
    {
        strings[_value] = *(new Label());
    }
    ostr << strings[_value];
}

void Return::generate()
{
    _expr->generate();

    load(_expr, rax);
    // “<function-name>.exit”
    cout << "\tjmp" << "     " << funcname << ".exit" << endl;
    assign(_expr, nullptr);
}

void Cast::generate()
{
    cout << "# Cast generate \n";

    unsigned long source, target;
    source = _expr->type().size();
    target = _type.size();
    cout << "# expr generate \n";

    _expr->generate();
    cout << "# after expr generate \n";

    if (_expr->reg == nullptr)
    {
        load(_expr, getreg());
    }
    if (source < target)
    {
        if (source == 1 && target == 4)
        {
            cout << "# Cast generate 1 && 4\n";

            cout << "\tmovsbl\t" << _expr << ", " << _expr->reg->name(target) << endl;
        }
        else if (source == 1 && target == 8)
        {
            cout << "# Cast generate 1 && 8\n";

            cout << "\tmovsbq\t" << _expr << ", " << _expr->reg->name(target) << endl;
        }
        else if (source == 4 && target == 8)
        {
            cout << "# Cast generate 4 && 8\n";
            cout << "\tmovslq\t" << _expr << ", " << _expr->reg->name(target) << endl;
        }
    }

    assign(this, _expr->reg);
}

void Break::generate()
{
    cout << "\tjmp\t" << stack_label.top() << endl;
    // stack_label.pop()
}

void For::generate()
{
    cout << "# FOR STATEMENT \n";

    Label loop, exit;
    _init->generate();
    cout << loop << ": \n";
    _expr->generate();
    if (_expr->reg == nullptr)
    {
        load(_expr, getreg());
    }
    cout << tab << "cmp" << suffix(_expr) << "$0, " << _expr << endl;
    cout << "\n# BEFORE JE \n";

    cout << tab << "je\t" << exit << endl;

    assign(_expr, nullptr);

    _stmt->generate();
    _incr->generate();
    cout << tab << "jmp\t" << loop << endl;
    cout << exit << ": \n";
}
void If::generate()
{
    cout << "# IF STATEMENT \n";
    Label skip;
    _expr->generate();
    if (_expr->reg == nullptr)
    {
        load(_expr, getreg());
    }
    cout << tab << "cmp" << suffix(_expr) << "$0, " << _expr << endl;
    cout << tab << "je\t" << skip << endl;
    assign(_expr, nullptr);

    _thenStmt->generate();
    if (_elseStmt)
    {
        Label exit;
        cout << tab << "jmp\t" << exit << endl;
        cout << skip << ": \n";
        _elseStmt->generate();
        cout << exit << ": \n";
    }
    else
    {
        cout << skip << ": \n";
    }
}