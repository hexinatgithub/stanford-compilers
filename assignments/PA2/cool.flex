/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <math.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

#undef COOL_STR_NOT_EXCEED
#define COOL_STR_NOT_EXCEED (string_buf_ptr < string_buf + MAX_STR_CONST)

#undef COOL_STR_TOO_LONG
#define COOL_STR_TOO_LONG (!COOL_STR_NOT_EXCEED)

#undef COOL_STR_APPEND
#define COOL_STR_APPEND(char) {*string_buf_ptr++ = char;}

#undef COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID
#define COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID(char) {  \
  COOL_STR_APPEND(char);  \
  if (COOL_STR_TOO_LONG) { \
    BEGIN(strinvalid);  \
  }}

#undef COOL_CHECK_STR
#define COOL_CHECK_STR() {  \
    if (COOL_STR_TOO_LONG) {  \
      BEGIN(INITIAL); \
      cool_yylval.error_msg = "String constants too long"; \
      return (ERROR); \
    } \
    BEGIN(INITIAL); \
    cool_yylval.error_msg = "String contains null character"; \
    return (ERROR); \
  }

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

CLASS           (?i:class)
ELSE            (?i:else)
FI              (?i:fi)
IF              (?i:if)
IN              (?i:in)
INHERITS        (?i:inherits)
LET             (?i:let)
LOOP            (?i:loop)
POOL            (?i:pool)
THEN            (?i:then)
WHILE           (?i:while)
CASE            (?i:case)
ESAC            (?i:esac)
OF              (?i:of)
DARROW          =>
NEW             (?i:new)
ISVOID          (?i:isvoid)
INT_CONST       [[:digit:]]+
BOOL_CONST      t(?i:rue)|f(?i:alse)
TYPEID          [[:upper:]][[:alnum:]_]*
OBJECTID        [[:lower:]][[:alnum:]_]*
ASSIGN          <-
NOT             (?i:not)
LE              <=

    int comment_nested_deep = 0;
%x  comment
%x  str
%x  strinvalid

%%

 /*
  * One line comments
  */
--(.*)$

 /*
  *  Nested comments
  */
"(*"                    {
  BEGIN(comment);
  ++comment_nested_deep;
}

<comment>{
  "*)"                  {
    --comment_nested_deep;
    if (comment_nested_deep == 0) BEGIN(INITIAL);
  }
  
  <<EOF>>               {
    cool_yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    return (ERROR);
  }

  .                     /* just ignore */
}

"*)"                    {
  cool_yylval.error_msg = "Unmatched *)";
  BEGIN(INITIAL);
  return (ERROR);
}

 /*
  *  The multiple-character operators.
  */
{DARROW}	    	  return (DARROW);
{ASSIGN}		      return (ASSIGN);
{LE}		          return (LE);

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}           return (CLASS);
{ELSE}		        return (ELSE);
{FI}		          return (FI);
{IF}		          return (IF);
{IN}		          return (IN);
{INHERITS}        return (INHERITS);
{LET}	  	        return (LET);
{LOOP}		        return (LOOP);
{POOL}		        return (POOL);
{THEN}   		      return (THEN);
{WHILE}	  	      return (WHILE);
{CASE}   		      return (CASE);
{ESAC}   		      return (ESAC);
{OF}   		        return (OF);
{NEW}  		        return (NEW);
{ISVOID}		      return (ISVOID);
{NOT}		          return (NOT);
{BOOL_CONST}      {
  cool_yylval.boolean = *yytext == 't';
  return (BOOL_CONST);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  *  reference: https://westes.github.io/flex/manual/Start-Conditions.html#Start-Conditions
  */

\"                             string_buf_ptr = string_buf; BEGIN(str);

<str>{
  \"                           {/* saw closing quote - all done */
    BEGIN(INITIAL);
    *string_buf_ptr = '\0';
    cool_yylval.symbol = stringtable.add_string(string_buf);
    return (STR_CONST);
  }
  
  \n                           {
    /* error - unterminated string constant */
    /* generate error message */
    BEGIN(INITIAL);
    cool_yylval.error_msg = "Unterminated string constant";
    return (ERROR);
  }

  \\b                          COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID('\b');
  \\t                          COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID('\t');
  \\n                          COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID('\n');
  \\f                          COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID('\f');
  \\(.|\n)                     COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID(yytext[1]);

  [^\\\n\"\0]+                 {
    char *yptr = yytext;

    while (*yptr && YYSTATE == str)
      COOL_STR_APPEND_OR_EXCEED_BEGIN_INVALID(*yptr++);
  }

  \0                           BEGIN(strinvalid);
}

<strinvalid>{
  \"|\n                        COOL_CHECK_STR();
}

<str,strinvalid><<EOF>>        {
  cool_yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  return (ERROR);
}

 /*
  * new line
  */
<*>\n                          ++curr_lineno;

{INT_CONST}	                   {
  cool_yylval.symbol = inttable.add_string(yytext, yyleng);
  return (INT_CONST);
}

{TYPEID}		                   {
  cool_yylval.symbol = idtable.add_string(yytext, yyleng);
  return (TYPEID);
}

{OBJECTID}		                 {
  cool_yylval.symbol = idtable.add_string(yytext, yyleng);
  return (OBJECTID);
}

[ \f\r\t\v]+                                                           /* white space, just ignore */
["+""/"\-"*""=""<"".""~"","";"":""("")""@""{""}"]                     return yytext[0];
.                                                                      {
  cool_yylval.error_msg = yytext;
  return (ERROR);
}

%%
