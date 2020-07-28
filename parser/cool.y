/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    %type <features> feature_list feature_list_non_empty
    %type <feature> feature
    %type <formals> formal_list formal_list_non_empty
    %type <formal> formal
    %type <expressions> expr_list_formal expr_list_formal_non_empty expr_list_block
    %type <expression> expr let_expr_nested
    %type <cases> branch_list
    %type <case_> branch
    
    /* Precedence declarations go here. */
    %left NESTEDLET
    %right ASSIGN
    %left NOT
    %nonassoc LE '<' '='
    %left '+' '-'
    %left '*' '/'
    %left ISVOID
    %left '~'
    %left '@'
    %left '.'
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program : class_list
      { @$ = @1; SET_NODELOC(@1); ast_root = program($1); }
    ;
    
    class_list : class      /* single class */
      { $$ = single_Classes($1); parse_results = $$; }
    | class_list class      /* several classes */
      { $$ = append_Classes($1,single_Classes($2)); parse_results = $$; }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class : CLASS TYPEID '{' feature_list '}' ';'
      { SET_NODELOC(@6); $$ = class_($2,idtable.add_string("Object"),$4,
        stringtable.add_string(curr_filename)); }
    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
      { SET_NODELOC(@8); $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    | error ';'
      { ; }
    ;
    
    /* Feature list may be empty, but no empty features in list. */
    feature_list :    /* empty */
      { $$ = nil_Features(); }
    | feature_list_non_empty
      { $$ = $1; }
    ;

    feature_list_non_empty : feature
      { $$ = single_Features($1); }
    | feature_list_non_empty feature
      { $$ = append_Features($1,single_Features($2)); }
    ;
    
    feature : OBJECTID '(' formal_list ')' ':' TYPEID '{' expr '}' ';'
      { SET_NODELOC(@10); $$ = method($1,$3,$6,$8); }
    | OBJECTID ':' TYPEID ';'
      { SET_NODELOC(@4); $$ = attr($1,$3,no_expr()); }
    | OBJECTID ':' TYPEID ASSIGN expr ';'
      { SET_NODELOC(@6); $$ = attr($1,$3,$5); }
    | error ';'
      { ; }
    ;

    formal_list :    /* empty */
      { $$ = nil_Formals(); }
    | formal_list_non_empty
      { $$ = $1; }
    ;

    formal_list_non_empty : formal
      { $$ = single_Formals($1); }
    | formal_list_non_empty ',' formal
      { $$ = append_Formals($1,single_Formals($3)); }
    ;

    formal : OBJECTID ':' TYPEID
      { SET_NODELOC(@3); $$ = formal($1,$3); }
    ;

    expr_list_formal :       /* empty */
      { $$ = nil_Expressions(); }
    | expr_list_formal_non_empty
      { $$ = $1; }
    ;

    expr_list_formal_non_empty : expr
      { $$ = single_Expressions($1); }
    | expr_list_formal_non_empty ',' expr
      { $$ = append_Expressions($1,single_Expressions($3)); }
    ;

    expr_list_block : expr ';'
      { $$ = single_Expressions($1); }
    | expr_list_block expr ';'
      { $$ = append_Expressions($1,single_Expressions($2)); }
    | error ';'
      { ; }
    ;

    let_expr_nested : IN expr %prec NESTEDLET
      { $$ = $2; }
    | ',' OBJECTID ':' TYPEID ASSIGN expr let_expr_nested
      { SET_NODELOC(@7); $$ = let($2,$4,$6,$7); }
    | ',' OBJECTID ':' TYPEID let_expr_nested
      { SET_NODELOC(@5); $$ = let($2,$4,no_expr(),$5); }
    | error let_expr_nested
      { ; }
    ;

    branch_list : branch
      { $$ = single_Cases($1); }
    | branch_list branch
      { $$ = append_Cases($1,single_Cases($2)); }
    ;

    branch : OBJECTID ':' TYPEID DARROW expr ';'
      { SET_NODELOC(@6); $$ = branch($1,$3,$5); }
    ;

    expr : OBJECTID ASSIGN expr
      { SET_NODELOC(@3); $$ = assign($1,$3); }
    | expr '@' TYPEID '.' OBJECTID '(' expr_list_formal ')'
      { SET_NODELOC(@8); $$ = static_dispatch($1,$3,$5,$7); }
    | expr '.' OBJECTID '(' expr_list_formal ')'
      { SET_NODELOC(@6); $$ = dispatch($1,$3,$5); }
    | OBJECTID '(' expr_list_formal ')'
      { SET_NODELOC(@4); $$ = dispatch(object(idtable.add_string("self")),$1,$3); }
    | IF expr THEN expr ELSE expr FI
      { SET_NODELOC(@7); $$ = cond($2,$4,$6); }
    | WHILE expr LOOP expr POOL
      { SET_NODELOC(@5); $$ = loop($2,$4); }
    | '{' expr_list_block '}'
      { SET_NODELOC(@3); $$ = block($2); }
    | LET OBJECTID ':' TYPEID ASSIGN expr let_expr_nested
      { SET_NODELOC(@7); $$ = let($2,$4,$6,$7); }
    | LET OBJECTID ':' TYPEID let_expr_nested
      { SET_NODELOC(@5); $$ = let($2,$4,no_expr(),$5); }
    | CASE expr OF branch_list ESAC
      { SET_NODELOC(@5); $$ = typcase($2,$4); }
    | NEW TYPEID
      { SET_NODELOC(@2); $$ = new_($2); }
    | ISVOID expr
      { SET_NODELOC(@2); $$ = isvoid($2); }
    | expr '+' expr
      { SET_NODELOC(@3); $$ = plus($1,$3); }
    | expr '-' expr
      { SET_NODELOC(@3); $$ = sub($1,$3); }
    | expr '*' expr
      { SET_NODELOC(@3); $$ = mul($1,$3); }
    | expr '/' expr
      { SET_NODELOC(@3); $$ = divide($1,$3); }
    | '~' expr
      { SET_NODELOC(@2); $$ = neg($2); }
    | expr '<' expr
      { SET_NODELOC(@3); $$ = lt($1,$3); }
    | expr LE expr
      { SET_NODELOC(@3); $$ = leq($1,$3); }
    | expr '=' expr
      { SET_NODELOC(@3); $$ = eq($1,$3); }
    | NOT expr
      { SET_NODELOC(@2); $$ = comp($2); }
    | '(' expr ')'
      { SET_NODELOC(@3); $$ = $2; }
    | OBJECTID
      { SET_NODELOC(@1); $$ = object($1); }
    | INT_CONST
      { SET_NODELOC(@1); $$ = int_const($1); }
    | STR_CONST
      { SET_NODELOC(@1); $$ = string_const($1); }
    | BOOL_CONST
      { SET_NODELOC(@1); $$ = bool_const($1); }
    ;

    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    