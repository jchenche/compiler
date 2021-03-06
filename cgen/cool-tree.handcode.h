//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"

#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class CgenNode;
class Locator;
template<class SYM, class DAT>
class SymbolTable;
typedef SymbolTable<std::string, Locator>* Environment;

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void cgen(ostream&) = 0;		\
virtual void dump_with_types(ostream&, int) = 0; // Pure virtual function



#define program_EXTRAS                          \
void cgen(ostream&);     			\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_name() = 0;  	\
virtual Symbol get_parent() = 0;    	\
virtual Symbol get_filename() = 0;      \
virtual Features get_features() = 0; \
virtual void dump_with_types(ostream&,int) = 0; \


#define class__EXTRAS                                  \
Symbol get_name()   { return name; }		       \
Symbol get_parent() { return parent; }     	       \
Symbol get_filename() { return filename; }             \
Features get_features() { return features; } \
void dump_with_types(ostream&,int);  \


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \
virtual void gather_variable_names(std::string, CgenNode*) = 0; \
virtual void gather_local_slots(Class_) = 0; \
virtual void code_attr_init(CgenNode*, Environment, ostream&) = 0; \
virtual void code_method_def(CgenNode*, ostream&) = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    \
void gather_variable_names(std::string, CgenNode*); \
void gather_local_slots(Class_); \
void code_attr_init(CgenNode*, Environment, ostream&); \
void code_method_def(CgenNode*, ostream&);


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
Symbol get_name() { return name; }


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; \
virtual void code(CgenNode*, Environment, int, ostream&) = 0; \
virtual int num_locals() = 0; \
virtual Symbol get_type_decl() = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
void code(CgenNode*, Environment, int, ostream&); \
int num_locals(); \
Symbol get_type_decl() { return type_decl; }


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void code(CgenNode*, Environment, int, ostream&) = 0; \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual int num_locals() = 0;

#define Expression_SHARED_EXTRAS           \
void code(CgenNode*, Environment, int, ostream&); 			   \
void dump_with_types(ostream&,int); \
int num_locals();


#endif
