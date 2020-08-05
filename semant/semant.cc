

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;

static ClassTable* ct;
static unordered_map<std::string, unordered_map<std::string, vector<Symbol> > > signatures;
static unordered_map<std::string, unordered_map<std::string, Symbol> > attrs;

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    install_basic_classes();
    construct_class_hierarchy(ast_root->get_classes());
}

void ClassTable::check_for_cycles(Class_ class_, Node* class_node) {
    Node* temp = class_node;
    while(temp != NULL) {
        temp = temp->get_parent();
        if (temp == class_node) {
            semant_error(class_) << "Inheritance cycle detected" << endl;
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }
    }
}

void ClassTable::construct_class_hierarchy(Classes classes) {
    std::string class_name, parent_name;
    Symbol parent_symbol;

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class_name = classes->nth(i)->get_name()->get_string();
        if (hierarchy.find(class_name) == hierarchy.end()) {
            hierarchy[class_name] = new Node(class_name);
        } else {
            semant_error(classes->nth(i)) << "Redefined class " << class_name << endl;
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }
    }

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class_name = classes->nth(i)->get_name()->get_string();
        parent_symbol = classes->nth(i)->get_parent();
        parent_name = parent_symbol->get_string();

        if (parent_symbol == No_class) continue; // Object class has no parent

        if (parent_symbol == Int || parent_symbol == Str || parent_symbol == Bool) {
            semant_error(classes->nth(i)) << "Can't inherit from " << parent_name << endl;
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }

        if (hierarchy.find(parent_name) != hierarchy.end()) {
            hierarchy[class_name]->set_parent(hierarchy[parent_name]);
            check_for_cycles(classes->nth(i), hierarchy[class_name]);
        } else {
            semant_error(classes->nth(i)) << "Non-existing class " << parent_name << endl;
            cerr << "Compilation halted due to static semantic errors." << endl;
            exit(1);
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    // Add these classes to ast_root
    ast_root->add_classes(Object_class)->add_classes(IO_class)
            ->add_classes(Int_class)->add_classes(Bool_class)
            ->add_classes(Str_class);

}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
}

Classes program_class::get_classes() { return classes; }

Program program_class::add_classes(Class_ c) {
    classes = append_Classes(classes, single_Classes(c));
    return this;
}

static void gather_decls() {
    Classes classes = ast_root->get_classes();
    Features features;
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        features = classes->nth(i)->get_features();
        for(int j = features->first(); features->more(j); j = features->next(j)) {
            features->nth(j)->gather_decls(classes->nth(i));
        }
    }

    if (semant_debug) {
        for(auto it: attrs) {
            for(auto it2: it.second) {
                cout << it2.first << " -> " << it2.second << endl;
            }
            cout << endl;
        }
        cout << "--------------------------" << endl;
        for(auto it: signatures) {
            for(auto it2: it.second) {
                cout << it2.first << "(";
                for(auto v: it2.second) {
                    cout << v << ", ";
                }
                cout << ")" << endl;
            }
            cout << endl;
        }
    }
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();
    ct = new ClassTable(classes);

    gather_decls();

    if (ct->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

static vector<Symbol> find_signature() {
    vector<Symbol> v;
    return v;
}

static bool type_exist(Symbol type) {
    return type == SELF_TYPE || type == prim_slot
        || ct->get_hierarchy().find(type->get_string()) != ct->get_hierarchy().end();
}

void method_class::gather_decls(Class_ class_) {
    std::string class_name = class_->get_name()->get_string();
    std::string method_name = name->get_string();
    Symbol type;

    if (signatures[class_name].find(method_name) == signatures[class_name].end()) {

        for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
            type = formals->nth(i)->get_type();
            signatures[class_name][method_name].push_back(type);
            if (!type_exist(type)) {
                ct->semant_error(class_) << "Class "
                << type << " is not defined" << endl;
            }
        }

        signatures[class_name][method_name].push_back(return_type);
        if (!type_exist(return_type)) {
            ct->semant_error(class_) << "Class "
            << return_type << " is not defined" << endl;
        }
        
    } else {
        ct->semant_error(class_) << "Duplicated method "
        << method_name << " in " << class_name << endl;
    }
}

void attr_class::gather_decls(Class_ class_) {
    std::string class_name = class_->get_name()->get_string();
    std::string attr_name = name->get_string();

    if (attrs[class_name].find(attr_name) == attrs[class_name].end()) {
        attrs[class_name][attr_name] = type_decl;
        if (!type_exist(type_decl)) {
            ct->semant_error(class_) << "Class "
            << type_decl << " is not defined" << endl;
        }        
    } else {
        ct->semant_error(class_) << "Duplicated attr "
        << attr_name << " in " << class_name << endl;
    }
}

