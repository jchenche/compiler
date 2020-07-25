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

int num_open = 0;
bool null_in_string = false;


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

%Start COMMENT ONELINECOMMENT STRING

%%

 /*
  *  Comments
  */

<COMMENT><<EOF>> {
  BEGIN (0);
  cool_yylval.error_msg = "EOF in comment";
  return (ERROR);
}
<INITIAL>"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}
<INITIAL,COMMENT>"(*"           { num_open++; BEGIN (COMMENT); }
<COMMENT>. ;
<COMMENT>\n                     { curr_lineno++; }
<COMMENT>"*)"                   { num_open--; if(num_open == 0) BEGIN (0); }
<INITIAL>--                     { BEGIN (ONELINECOMMENT); }
<ONELINECOMMENT>[^\n] ;
<ONELINECOMMENT>\n              { curr_lineno++; BEGIN (0); }

 /*
  *  Strings
  */

<STRING><<EOF>> {
  BEGIN (0);
  cool_yylval.error_msg = "EOF in string constant";
  return (ERROR);
}
<INITIAL>\" {
  BEGIN (STRING);
  null_in_string = false;
  string_buf_idx = 0;
}
<STRING>\n {        /* Unescaped newline breaks the string */
  curr_lineno++;
  BEGIN (0);
  cool_yylval.error_msg = "Unterminated string constant";
  return (ERROR);
}
<STRING>\\\n {      /* Escaped newline is ok */
  string_buf[string_buf_idx++] = '\n';
  curr_lineno++;
}
<STRING>\\\0|\0 {   /* Null poisons the string but \ followed by 0 is ok */
  null_in_string = true;
}
<STRING>[^\"] {
  string_buf[string_buf_idx++] = yytext[0];
}
<STRING>\\. {
  switch (yytext[1]) {
    case 'b':  string_buf[string_buf_idx++] = '\b';      break;
    case 't':  string_buf[string_buf_idx++] = '\t';      break;
    case 'n':  string_buf[string_buf_idx++] = '\n';      break;
    case 'f':  string_buf[string_buf_idx++] = '\f';      break;
    default:   string_buf[string_buf_idx++] = yytext[1]; break;
  }
}
<STRING>\" {
  BEGIN (0);
  if (null_in_string) {
    cool_yylval.error_msg = "String contains null character";
    return (ERROR);
  } else if (string_buf_idx >= MAX_STR_CONST) {
    cool_yylval.error_msg = "String constant too long";
    return (ERROR);
  } else {
    string_buf[string_buf_idx] = '\0';
    cool_yylval.symbol = stringtable.add_string(string_buf);
    return (STR_CONST);
  }
}

 /*
  *  Keywords
  */

<INITIAL>{C}{L}{A}{S}{S}             { return CLASS; }
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
<INITIAL>t{R}{U}{E} {
  cool_yylval.boolean = true;
  return (BOOL_CONST);
}
<INITIAL>f{A}{L}{S}{E} {
  cool_yylval.boolean = false;
  return (BOOL_CONST);
}

 /*
  *  Special symbols
  */

<INITIAL>\+   { return ('+'); }
<INITIAL>\/   { return ('/'); }
<INITIAL>-    { return ('-'); }
<INITIAL>\*   { return ('*'); }
<INITIAL>=    { return ('='); }
<INITIAL><    { return ('<'); }
<INITIAL>\.   { return ('.'); }
<INITIAL>~    { return ('~'); }
<INITIAL>,    { return (','); }
<INITIAL>;    { return (';'); }
<INITIAL>:    { return (':'); }
<INITIAL>\(   { return ('('); }
<INITIAL>\)   { return (')'); }
<INITIAL>@    { return ('@'); }
<INITIAL>\{   { return ('{'); }
<INITIAL>\}   { return ('}'); }
<INITIAL><-   { return (ASSIGN); }
<INITIAL><=   { return (LE); }
<INITIAL>=>   { return (DARROW); }

 /*
  *  Integers, identifiers, and whitespace
  */

<INITIAL>[0-9]+ {
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST);
}
<INITIAL>[a-z][0-9a-zA-Z_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}
<INITIAL>[A-Z][0-9a-zA-Z_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}
<INITIAL>\n                  { curr_lineno++; }
<INITIAL>[ \f\r\t\v] ;

<INITIAL>. {
  cool_yylval.error_msg = yytext;
  return (ERROR);
}

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
