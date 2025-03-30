%{
/*
 * File:	lexer.l
 *
 * Description:	This file contains the flex description for the lexical
 *		analyzer for Simple C.
 *
 *		Extra functionality:
 *		- checking for out of range integer and real literals
 *		- checking for invalid string and character literals
 */

# include <cerrno>
# include <cstdlib>
# include <iostream>
# include "tokens.h"
# include "string.h"
# include "lexer.h"


/* This used to be a function, but students wouldn't remove the definition
   when they moved to the next phase, even though the function wasn't used
   anymore and they'd get a warning telling them that.  It annoyed me. */

# define printToken(s)  (cout << s << " " << yytext << endl)


using namespace std;

int numerrors = 0;
static void checkInt(), checkString(), checkChar();
static void ignoreComment();
%}

%option nounput noyywrap yylineno
%%

"/*"					{ignoreComment();}

"auto"					{return AUTO;}
"break"					{return BREAK;}
"case"					{return CASE;}
"char"					{return CHAR;}
"const"					{return CONST;}
"continue"				{return CONTINUE;}
"default"				{return DEFAULT;}
"do"					{return DO;}
"double"				{return DOUBLE;}
"else"					{return ELSE;}
"enum"					{return ENUM;}
"extern"				{return EXTERN;}
"float"					{return FLOAT;}
"for"					{return FOR;}
"goto"					{return GOTO;}
"if"					{return IF;}
"int"					{return INT;}
"long"					{return LONG;}
"register"				{return REGISTER;}
"return"				{return RETURN;}
"short"					{return SHORT;}
"signed"				{return SIGNED;}
"sizeof"				{return SIZEOF;}
"static"				{return STATIC;}
"struct"				{return STRUCT;}
"switch"				{return SWITCH;}
"typedef"				{return TYPEDEF;}
"union"					{return UNION;}
"unsigned"				{return UNSIGNED;}
"void"					{return VOID;}
"volatile"				{return VOLATILE;}
"while"					{return WHILE;}

"||"					{return OR;}
"&&"					{return AND;}
"=="					{return EQL;}
"!="					{return NEQ;}
"<="					{return LEQ;}
">="					{return GEQ;}
"++"					{return INC;}
"--"					{return DEC;}
"->"					{return ARROW;}
"..."					{return ELLIPSIS;}
[-|=<>+*/%&!()\[\]{};:.,]		{return yytext[0];}

[a-zA-Z_][a-zA-Z_0-9]*			{return ID;}

[0-9]+[lL]?				{checkInt(); return NUM;}
\"(\\.|[^\\\n"])*\"			{checkString(); return STRING;}
\'(\\.|[^\\\n'])+\'			{checkChar(); return CHARACTER;}

[ \f\n\r\t\v]+				{/* ignored */}
.					{/* ignored */}

%%

/*
 * Function:	ignoreComment
 *
 * Description:	Ignore a comment after recognizing its beginning.
 */

static void ignoreComment()
{
    int c1, c2;


    while ((c1 = yyinput()) != 0) {
	while (c1 == '*') {
	    if ((c2 = yyinput()) == '/' || c2 == 0)
		return;

	    c1 = c2;
	}
    }

    if (c1 == 0)
	report("unterminated comment");
}


/*
 * Function:	checkInt
 *
 * Description:	Check if an integer constant is valid.
 */

static void checkInt()
{
    errno = 0;
    strtol(yytext, NULL, 0);

    if (errno != 0)
	report("integer constant too large");
}


/*
 * Function:	checkString
 *
 * Description:	Check if a string literal is valid.
 */

static void checkString()
{
    bool invalid, overflow;
    string s(yytext + 1, yyleng - 2);


    s = parseString(s, invalid, overflow);

    if (invalid)
	report("unknown escape sequence in string constant");
    else if (overflow)
	report("escape sequence out of range in string constant");
}


/*
 * Function:	checkChar
 *
 * Description:	Check if a character literal is valid.
 */

static void checkChar()
{
    bool invalid, overflow;
    string s(yytext + 1, yyleng - 2);


    s = parseString(s, invalid, overflow);

    if (invalid)
	report("unknown escape sequence in character constant");
    else if (overflow)
	report("escape sequence out of range in character constant");
    else if (s.size() > 1)
	report("multi-character character constant");
}


/*
 * Function:	report
 *
 * Description:	Report an error to the standard error prefixed with the
 *		line number.  We'll be using this a lot later with an
 *		optional string argument, but C++'s stupid streams don't do
 *		positional arguments, so we actually resort to snprintf.
 *		You just can't beat C for doing things down and dirty.
 */

void report(const string &str, const string &arg)
{
    char buf[1000];


    snprintf(buf, sizeof(buf), str.c_str(), arg.c_str());
    cerr << "line " << yylineno << ": " << buf << endl;
    numerrors ++;
}
