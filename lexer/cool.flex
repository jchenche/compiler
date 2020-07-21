%option noyywrap
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
int  string_buf_idx;

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


A [Aa]
B [Bb]
C [Cc]
D [Dd]
E [Ee]
F [Ff]
G [Gg]
H [Hh]
I [Ii]
J [Jj]
K [Kk]
L [Ll]
M [Mm]
N [Nn]
O [Oo]
P [Pp]
Q [Qq]
R [Rr]
S [Ss]
T [Tt]
U [Uu]
V [Vv]
W [Ww]
X [Xx]
Y [Yy]
Z [Zz]



DARROW          =>
WHITESPACE      [ \n\f\r\t\v]

%Start COMMENT ONELINECOMMENT ENDOFFILE STRING

%%

 /*
  *  Comments
  */

  int num_open = 0;
<COMMENT,ONELINECOMMENT><<EOF>> {
  BEGIN (ENDOFFILE);
  cool_yylval.error_msg = "EOF in comment";
  return (ERROR);
}
<ENDOFFILE><<EOF>> { return 0; }
<INITIAL>"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}
<INITIAL,COMMENT>"(*"           { num_open++; BEGIN (COMMENT); }
<COMMENT>.|\n ;
<COMMENT>"*)"                   { num_open--; if(num_open == 0) BEGIN (0); }
<INITIAL>--                     { BEGIN (ONELINECOMMENT); }
<ONELINECOMMENT>[^\n] ;
<ONELINECOMMENT>\n              { BEGIN (0); }

 /*
  *  Strings
  */

<STRING><<EOF>> {
  BEGIN (ENDOFFILE);
  cool_yylval.error_msg = "EOF in string constant";
  return (ERROR);
}
<STRING>[\0][^\"]*\" {
  BEGIN (0);
  cool_yylval.error_msg = "String contains null character";
  return (ERROR);
}
<INITIAL>\" {
  BEGIN (STRING);
  string_buf_idx = 0;
}
<STRING>\n {
  BEGIN (0);
  cool_yylval.error_msg = "Unterminated string constant";
  return (ERROR);
}
<STRING>[^\"] {
  string_buf[string_buf_idx++] = yytext[0];
}
<STRING>\\. {
  switch (yytext[1]) {
    case 'b': string_buf[string_buf_idx++] = '\b';      break;
    case 't': string_buf[string_buf_idx++] = '\t';      break;
    case 'n': string_buf[string_buf_idx++] = '\n';      break;
    case 'f': string_buf[string_buf_idx++] = '\f';      break;
    default:  string_buf[string_buf_idx++] = yytext[1]; break;
  }
}
<STRING>\" {
  BEGIN (0);
  if (string_buf_idx <= MAX_STR_CONST) {
    string_buf[string_buf_idx] = '\0';
    cool_yylval.symbol = stringtable.add_string(string_buf);
    return (STR_CONST);
  } else {
    cool_yylval.error_msg = "String constant too long";
    return (ERROR);
  }
}

 /* char string_buf[MAX_STR_CONST]  */
 /* int  string_buf_idx */

<INITIAL>(?:class)                   { return CLASS; }
<INITIAL>{E}{L}{S}{E}                { return ELSE; }
<INITIAL>{F}{I}                      { return FI; }
<INITIAL>{I}{F}                      { return IF; }
<INITIAL>{I}{N}                      { return IN; }
<INITIAL>{I}{N}{H}{E}{R}{I}{T}{S}    { return INHERITS; }
<INITIAL>{I}{S}{V}{O}{I}{D}          { return ISVOID; }
<INITIAL>{L}{E}{T}                   { return LET; }
<INITIAL>{L}{O}{O}{P}                { return LOOP; }
<INITIAL>{P}{O}{O}{L}                { return POOL; }
<INITIAL>{T}{H}{E}{N}                { return THEN; }
<INITIAL>{W}{H}{I}{L}{E}             { return WHILE; }
<INITIAL>{C}{A}{S}{E}                { return CASE; }
<INITIAL>{E}{S}{A}{C}                { return ESAC; }
<INITIAL>{N}{E}{W}                   { return NEW; }
<INITIAL>{O}{F}                      { return OF; }
<INITIAL>{N}{O}{T}                   { return NOT; }
t{R}{U}{E} {
  cool_yylval.boolean = true;
  return (BOOL_CONST);
}
f{A}{L}{S}{E} {
  cool_yylval.boolean = false;
  return (BOOL_CONST);
}




<INITIAL>{DARROW}		         { return (DARROW); }
 /* <INITIAL>{WHITESPACE} ; */


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
