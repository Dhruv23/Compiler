# scc – A Tiny C Compiler (x86, Linux)

This project is a teaching-oriented C compiler written in modern C++ (C++17). It accepts a small, C-like language (chars, ints, longs, pointers, arrays, functions, control flow, and basic expressions) and emits AT&T-style x86-64 assembly for Linux. The toolchain is intentionally simple: a Flex lexer feeds a hand-written recursive‑descent parser, followed by semantic checks, register allocation, and assembly emission.

## Key Features
- **Lexer (Flex-based):** `lexer.l` defines tokens for C keywords, operators, literals, and identifiers, including range checks for numeric literals and validation of string/char escape sequences (`checkInt`, `checkString`, `checkChar`).
- **Recursive‑Descent Parser:** `parser.cpp` implements the full grammar by hand (functions like `expression()`, `statement()`, `functionOrGlobal()`), performs a single pass over tokens, and builds typed AST nodes on the fly.
- **Semantic Analyzer:** `checker.cpp` drives scoped symbol management (`openScope/closeScope`, `Scope`/`Symbol`) and enforces type rules (promotion, pointer arithmetic scaling, cast validation, lvalue checks, return/argument compatibility). Undeclared identifiers are captured as error-typed symbols to avoid cascaded errors.
- **AST with Polymorphic Codegen:** `Tree.h/.cpp` model nodes (`Expression`, `Statement`, `Function`, etc.) with virtual `generate` / `allocate` / `write` hooks. Derived classes (e.g., `Add`, `While`, `Call`, `Return`) override behavior, enabling object-oriented code emission and pretty-printing (`writer.cpp`).
- **Backend & Register Allocator:** `generator.cpp` walks the AST, assigns stack offsets (`allocate()` in `allocator.cpp`), manages calling conventions (System V AMD64), and implements a simple register allocator (`getreg`, `assign`, `load`) that spills to the stack when all general registers are busy. Function prologues/epilogues, parameter moves, and string literal pooling are handled here.
- **Label and Control-Flow Management:** `Label` provides unique `.L*` symbols; a stack tracks break targets in loops. Conditional nodes emit compares/sets, short‑circuit logic, and structured jumps.
- **Type Representation:** `Type.h/.cpp` capture specifier, indirection, arrays, and function signatures with helper predicates (`isNumeric`, `isPointer`, `isCompatibleWith`) and size/alignment queries tied to `machine.h` target constants.

## Installation & Build
Dependencies: a C++17 compiler and Flex.

```bash
# Generate the lexer and build the compiler binary
make

# Clean outputs
make clean
```

`make` produces the `scc` executable in the project root. If `lexer.cpp` is missing or outdated, `make` regenerates it from `lexer.l` using `flex` (`$(LEX)` in the Makefile).

## Usage
The compiler reads C source from stdin and writes x86-64 assembly to stdout.

```bash
# Compile source.c to assembly
./scc < source.c > source.s

# Assemble and link with GCC
gcc -no-pie -o program source.s
```

## Project Structure
- `Makefile` – build rules, C++17 flags, flex rule for `lexer.cpp`.
- `lexer.l`, `lexer.h`, `lexer.cpp` – tokenization via Flex, error reporting helpers.
- `parser.cpp` – recursive‑descent parser and driver (`main`), invokes semantic checks and codegen.
- `checker.h/.cpp` – semantic analysis, scope and symbol management, type compatibility, implicit casts, pointer arithmetic scaling.
- `Tree.h/.cpp` – AST node definitions, constructors, lvalue tracking, virtual hooks for allocate/generate/write.
- `allocator.cpp` – stack-frame layout, parameter/local offsets, alignment handling across nested blocks.
- `generator.h/.cpp` – code generation, register allocator, call ABI handling, control-flow labels, string literal pool emission.
- `writer.cpp` – optional AST pretty-printer (LISP-like form) using virtual dispatch.
- `Type.h/.cpp`, `machine.h` – target sizes/alignments and rich type predicates/helpers.
- `Scope.h/.cpp`, `Symbol.h/.cpp` – nested scopes and symbol table entries with offsets.
- `Register.h/.cpp`, `Label.h/.cpp` – register abstractions and unique label generation.
- `string.h/.cpp` – escape-sequence parsing and emission for literals.
- `tokens.h` – token enumerations shared by lexer/parser.

## Running Notes
- Target: x86-64 System V ABI on Linux (see `machine.h` for sizes/alignments and calling-convention constants).
- Supported language: core C subset with simple types (char/int/long), pointers, arrays, functions (including variadic), and control-flow constructs (`if/else`, `while`, `for`, `break`, `return`).
- Output assembly uses AT&T syntax and expects `gcc`/`ld` defaults (`-no-pie` shown for predictable addresses).

Happy compiling!
