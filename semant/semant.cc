

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
static SymbolTable<std::string, Entry>* env;
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

ClassTable::ClassTable() : semant_errors(0) , error_stream(cerr) {
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

    hierarchy[SELF_TYPE->get_string()] = new Node(SELF_TYPE->get_string());

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

        if (parent_symbol == Int || parent_symbol == Str ||
            parent_symbol == Bool || parent_symbol == SELF_TYPE) {
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
    classes = append_Classes(single_Classes(c), classes);
    return this;
}

static void gather_attr_and_signature_decls() {
    Classes classes = ast_root->get_classes();
    Features features;
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        features = classes->nth(i)->get_features();
        for(int j = features->first(); features->more(j); j = features->next(j)) {
            features->nth(j)->gather_decls(classes->nth(i));
        }
    }
}

static void check_main_in_Main() {
    std::string main_class_name = Main->get_string();
    if (ct->get_hierarchy().find(main_class_name) == ct->get_hierarchy().end()) {
        ct->semant_error() << "Class Main is not defined." << endl;
        return;
    }
    std::string main_meth_name = main_meth->get_string();
    if (signatures[main_class_name].find(main_meth_name) == signatures[main_class_name].end()) {
        ct->semant_error() << "Method main is not defined in Main." << endl;
        return;
    }
    if (signatures[main_class_name][main_meth_name].size() > 1) {
        ct->semant_error() << "Method main should have no arguments." << endl;
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

    Classes classes_without_basic = classes->copy_list();
    ct = new ClassTable();
    gather_attr_and_signature_decls();
    classes = classes_without_basic;

    check_main_in_Main();

    env = new SymbolTable<std::string, Entry>();
    typecheck();

    if (ct->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

static bool type_exist(Symbol type) {
    return type == prim_slot
        || ct->get_hierarchy().find(type->get_string()) != ct->get_hierarchy().end();
}

void method_class::gather_decls(Class_ class_) {
    std::string class_name = class_->get_name()->get_string();
    std::string method_name = name->get_string();
    Symbol type;

    if (signatures[class_name].find(method_name) == signatures[class_name].end()) {

        for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
            type = formals->nth(i)->get_type();
            if (type_exist(type)) {
                if (type != SELF_TYPE) {
                    signatures[class_name][method_name].push_back(type);
                } else {
                    signatures[class_name][method_name].push_back(Object);
                    ct->semant_error(class_) << "Formal parameters can't have type SELF_TYPE" << endl;
                }
            } else {
                signatures[class_name][method_name].push_back(Object);
                ct->semant_error(class_) << "Class " << type << " is not defined" << endl;
            }
        }

        if (type_exist(return_type)) {
            if (return_type == SELF_TYPE) {
                signatures[class_name][method_name].push_back(class_->get_name());
            } else {
                signatures[class_name][method_name].push_back(return_type);
            }
        } else {
            signatures[class_name][method_name].push_back(Object);
            ct->semant_error(class_) << "Class " << return_type << " is not defined" << endl;
        }

    } else {
        ct->semant_error(class_) << "Duplicated method " << method_name << " in " << class_name << endl;
    }
}

void attr_class::gather_decls(Class_ class_) {
    std::string class_name = class_->get_name()->get_string();
    std::string attr_name = name->get_string();

    if (attrs[class_name].find(attr_name) == attrs[class_name].end()) {
        if (type_exist(type_decl)) {
            if (type_decl == SELF_TYPE) {
                attrs[class_name][attr_name] = class_->get_name();
            } else {
                attrs[class_name][attr_name] = type_decl;
            }
        } else {
            attrs[class_name][attr_name] = Object;
            ct->semant_error(class_) << "Class " << type_decl << " is not defined" << endl;
        }
    } else {
        ct->semant_error(class_) << "Duplicated attr " << attr_name << " in " << class_name << endl;
    }
}

void add_attr_to_scope(Class_ class_) {
    std::string curr_class_name = class_->get_name()->get_string();
    Node* curr_node = ct->get_hierarchy()[curr_class_name];

    while(true) {
        for(auto it: attrs[curr_class_name]) {
            if (env->probe(it.first) == NULL) {
                env->addid(it.first, it.second);
            } else {
                ct->semant_error(class_) << "Duplicated attr "
                << it.first << " in " << curr_class_name << " and its descendants" << endl;
            }
        }

        curr_node = curr_node->get_parent();
        if (curr_node == NULL) break;
        curr_class_name = curr_node->get_name();
    }
}

static void check_overridden_method_sig(Class_ class_, std::string class_name, std::string method_name) {
    std::string curr_class_name = class_name;
    Node* curr_node = ct->get_hierarchy()[curr_class_name];
    bool found_method = false;
    vector<Symbol> old_signature;

    while(true) {
        if (signatures[curr_class_name].find(method_name) != signatures[curr_class_name].end()) {
            if (found_method) {
                if (old_signature != signatures[curr_class_name][method_name]) {
                    ct->semant_error(class_) << "Method " << method_name << " in " << curr_class_name
                    << " is overriden by " << method_name << " in " << class_name
                    << " with a different signature" << endl;
                }
                break;
            }
            found_method = true;
            old_signature = signatures[curr_class_name][method_name];
        }
        curr_node = curr_node->get_parent();
        if (curr_node == NULL) break;
        curr_class_name = curr_node->get_name();
    }
}

static vector<Symbol> get_signature(std::string class_name, std::string method_name) {
    std::string curr_class_name = class_name;
    Node* curr_node = ct->get_hierarchy()[curr_class_name];

    while(true) {
        if (signatures[curr_class_name].find(method_name) != signatures[curr_class_name].end()) {
            return signatures[curr_class_name][method_name];
        }
        curr_node = curr_node->get_parent();
        if (curr_node == NULL) break;
        curr_class_name = curr_node->get_name();
    }

    return {};
}

static bool is_subtype(Symbol t1, Symbol t2) {
    std::string curr_class_name = t1->get_string();
    std::string target_class_name = t2->get_string();
    Node* curr_node = ct->get_hierarchy()[curr_class_name];
    Node* target_node = ct->get_hierarchy()[target_class_name];
    while(curr_node != NULL) {
        if (curr_node == target_node) return true;
        curr_node = curr_node->get_parent();
    }
    return false;
}

static std::string least_common_ancestor(std::string class1, std::string class2) {
    vector<Node*> nodes1;
    vector<Node*> nodes2;
    
    Node* curr_node = ct->get_hierarchy()[class1];
    while(curr_node != NULL) {
        nodes1.push_back(curr_node);
        curr_node = curr_node->get_parent();
    }

    curr_node = ct->get_hierarchy()[class2];
    while(curr_node != NULL) {
        nodes2.push_back(curr_node);
        curr_node = curr_node->get_parent();
    }

    int i = nodes1.size() - 1;
    int j = nodes2.size() - 1;
    while(i >= 0 && j >= 0) {
        if (nodes1.at(i) != nodes2.at(j)) break;
        i--;
        j--;
    }

    return nodes1.at(i + 1)->get_name();
}

static char* to_compatible_string(std::string s) {
    char* new_s = new char[s.size() + 1];
    strncpy(new_s, s.c_str(), s.size());
    new_s[s.size()] = '\0';
    return new_s;
}

void program_class::typecheck() {
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        classes->nth(i)->typecheck();
    }
}

void class__class::typecheck() {
    env->enterscope();
    add_attr_to_scope(this);
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->typecheck(this);
    }
    env->exitscope();
}

void method_class::typecheck(Class_ class_) {
    std::string class_name = class_->get_name()->get_string();
    std::string method_name = name->get_string();

    check_overridden_method_sig(class_, class_name, method_name);
    vector<Symbol> method_sig = get_signature(class_name, method_name);
    int size = method_sig.size() - 1;
    assert(size == formals->len());

    std::string formal_name;
    Symbol formal_type;

    env->enterscope();
    env->addid(self->get_string(), class_->get_name());

    int i, j;
    for(i = formals->first(), j = 0; formals->more(i) && j < size; i = formals->next(i), ++j) {
        formal_name = formals->nth(i)->get_name()->get_string();
        formal_type = method_sig.at(j);

        if (formals->nth(i)->get_name() == self) {
            ct->semant_error(class_) << "self cannot be the name of a formal parameter" << endl;
        } else if (env->probe(formal_name) == NULL) {
            env->addid(formal_name, formal_type);
        } else {
            ct->semant_error(class_) << "Duplicated formal parameter "
            << formal_name << " in " << method_name << endl;
        }
    }

    Symbol t1 = expr->typecheck(class_);
    Symbol t2 = method_sig.at(size);
    // if (t2 == SELF_TYPE) t2 = class_->get_name();

    if (t1 != No_type && !is_subtype(t1, t2)) {
        ct->semant_error(class_) << t1 << " is not a subtype of " << t2 << endl;
    }

    env->exitscope();
}

void attr_class::typecheck(Class_ class_) {
    env->enterscope();

    if (name == self) {
        ct->semant_error(class_) << "self cannot be the name of an attribute" << endl;
    }

    Symbol t0 = type_decl;
    // if (t0 == SELF_TYPE) t0 = class_->get_name();

    env->addid(self->get_string(), class_->get_name());
    Symbol t1 = init->typecheck(class_);

    if (t1 != No_type && !is_subtype(t1, t0)) {
        ct->semant_error(class_) << t1 << " is not a subtype of " << t0 << endl;
    }

    env->exitscope();
}

Symbol no_expr_class::typecheck(Class_ class_) {
    return set_type(No_type)->get_type();
}

Symbol object_class::typecheck(Class_ class_) {
    Symbol t1 = env->lookup(name->get_string());
    if (t1 == NULL) {
        ct->semant_error(class_) << name << " is not defined in scope" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(t1)->get_type();
}

Symbol assign_class::typecheck(Class_ class_) {
    if (name == self) {
        ct->semant_error(class_) << "Cannot assign to self" << endl;
        return set_type(Object)->get_type();
    }

    Symbol t1 = env->lookup(name->get_string());
    if (t1 == NULL) {
        ct->semant_error(class_) << name << " is not defined in scope" << endl;
        return set_type(Object)->get_type();
    }

    Symbol t2 = expr->typecheck(class_);
    if (!is_subtype(t2, t1)) {
        ct->semant_error(class_) << t2 << " is not a subtype of " << t1 << endl;
        return set_type(Object)->get_type();
    }

    return set_type(t2)->get_type();
}

Symbol bool_const_class::typecheck(Class_ class_) {
    return set_type(Bool)->get_type();
}

Symbol int_const_class::typecheck(Class_ class_) {
    return set_type(Int)->get_type();
}

Symbol string_const_class::typecheck(Class_ class_) {
    return set_type(Str)->get_type();
}

Symbol new__class::typecheck(Class_ class_) {
    if (!type_exist(type_name)) {
        ct->semant_error(class_) << "Type " << type_name << " is not defined" << endl;
        return set_type(Object)->get_type();
    }
    Symbol t1 = type_name;
    if (t1 == SELF_TYPE) t1 = class_->get_name();
    return set_type(t1)->get_type();
}

Symbol dispatch_class::typecheck(Class_ class_) {
    std::string method_name = name->get_string();
    vector<Symbol> t;

    t.push_back(expr->typecheck(class_));
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        t.push_back(actual->nth(i)->typecheck(class_));
    }

    vector<Symbol> method_sig = get_signature(t.at(0)->get_string(), method_name);
    int param_size = method_sig.size();
    int arg_size = t.size();

    if (method_sig.empty()) {
        ct->semant_error(class_) << "Method " << method_name << " is not defined in "
        << class_->get_name() << " or any of its ancestors" << endl;
        return set_type(Object)->get_type();
    }

    if (arg_size != param_size) {
        ct->semant_error(class_) << "Method " << method_name <<
        " is called with wrong number of arguments" << endl;
        return set_type(Object)->get_type();
    }

    for(int i = 1; i < arg_size; ++i) {
        if (!is_subtype(t.at(i), method_sig.at(i - 1))) {
            ct->semant_error(class_) << t.at(i)
            << " is not a subtype of " << method_sig.at(i - 1) << endl;
            return set_type(Object)->get_type();
        }
    }

    return set_type(method_sig.at(param_size - 1))->get_type();
}

Symbol static_dispatch_class::typecheck(Class_ class_) {
    std::string method_name = name->get_string();
    vector<Symbol> t;

    t.push_back(expr->typecheck(class_));
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        t.push_back(actual->nth(i)->typecheck(class_));
    }

    if (!is_subtype(t.at(0), type_name)) {
        ct->semant_error(class_) << t.at(0) << " is not a subtype of " << type_name << endl;
        return set_type(Object)->get_type();
    }

    vector<Symbol> method_sig = get_signature(type_name->get_string(), method_name);
    int param_size = method_sig.size();
    int arg_size = t.size();

    if (method_sig.empty()) {
        ct->semant_error(class_) << "Method " << method_name <<
        " is called with wrong number of arguments" << endl;
        return set_type(Object)->get_type();
    }

    if (arg_size != param_size) {
        ct->semant_error(class_) << "Method argument and paramater number mismatch for "
        << method_name << endl;
        return set_type(Object)->get_type();
    }

    for(int i = 1; i < arg_size; ++i) {
        if (!is_subtype(t.at(i), method_sig.at(i - 1))) {
            ct->semant_error(class_) << t.at(i)
            << " is not a subtype of " << method_sig.at(i - 1) << endl;
            return set_type(Object)->get_type();
        }
    }

    return set_type(method_sig.at(param_size - 1))->get_type();
}

Symbol cond_class::typecheck(Class_ class_) {
    Symbol t1 = pred->typecheck(class_);
    if (t1 != Bool) {
        ct->semant_error(class_) << "The if condition doesn't have type Bool" << endl;
        return set_type(Object)->get_type();
    }
    Symbol t2 = then_exp->typecheck(class_);
    Symbol t3 = else_exp->typecheck(class_);
    std::string lca_class_name = least_common_ancestor(t2->get_string(), t3->get_string());
    return set_type(idtable.lookup_string(to_compatible_string(lca_class_name)))->get_type();
}

Symbol block_class::typecheck(Class_ class_) {
    Symbol t;
    for(int i = body->first(); body->more(i); i = body->next(i)) {
        t = body->nth(i)->typecheck(class_);
    }
    return set_type(t)->get_type();
}

Symbol let_class::typecheck(Class_ class_) {
    if (identifier == self) {
        ct->semant_error(class_) << "self cannot be bound in a let expression" << endl;
        return set_type(Object)->get_type();
    }

    if (!type_exist(type_decl)) {
        ct->semant_error(class_) << "Type " << type_decl << " is not defined" << endl;
        return set_type(Object)->get_type();
    }
    Symbol t0 = type_decl;
    if (t0 == SELF_TYPE) t0 = class_->get_name();

    Symbol t1 = init->typecheck(class_);
    if (t1 != No_type && !is_subtype(t1, t0)) {
        ct->semant_error(class_) << t1 << " is not a subtype of " << t0 << endl;
        return set_type(Object)->get_type();
    }

    env->enterscope();
    env->addid(identifier->get_string(), t0);
    Symbol t2 = body->typecheck(class_);
    env->exitscope();

    return set_type(t2)->get_type();
}

Symbol typcase_class::typecheck(Class_ class_) {
    Symbol t0 = expr->typecheck(class_);
    vector<Symbol> t;
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
        t.push_back(cases->nth(i)->typecheck(class_));
    }

    std::string lca_class_name = t.at(0)->get_string();
    int size = t.size();
    for(int i = 1; i < size; ++i) {
        lca_class_name = least_common_ancestor(lca_class_name, t.at(i)->get_string());
    }
    return set_type(idtable.lookup_string(to_compatible_string(lca_class_name)))->get_type();
}

Symbol branch_class::typecheck(Class_ class_) {
    if (name == self) {
        ct->semant_error(class_) << "self cannot be bound in a case branch" << endl;
        return Object;
    }

    if (!type_exist(type_decl)) {
        ct->semant_error(class_) << "Type " << type_decl << " is not defined" << endl;
        return Object;
    }
    if (type_decl == SELF_TYPE) {
        ct->semant_error(class_) << "SELF_TYPE can't be a declared type in case expressions" << endl;
        return Object;
    }
    env->enterscope();
    env->addid(name->get_string(), type_decl);
    Symbol t = expr->typecheck(class_);
    env->exitscope();
    return t;
}

Symbol loop_class::typecheck(Class_ class_) {
    Symbol t1 = pred->typecheck(class_);
    if (t1 != Bool) {
        ct->semant_error(class_) << "The while condition doesn't have type Bool" << endl;
        return set_type(Object)->get_type();
    }
    Symbol t2 = body->typecheck(class_);
    return set_type(Object)->get_type();
}

Symbol isvoid_class::typecheck(Class_ class_) {
    Symbol t = e1->typecheck(class_);
    return set_type(Bool)->get_type();
}

Symbol comp_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    if (t1 != Bool) {
        ct->semant_error(class_) << "The operand of NOT must have type Bool" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol lt_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 != Int || t2 != Int) {
        ct->semant_error(class_) << "The operands of < must of type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol leq_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 != Int || t2 != Int) {
        ct->semant_error(class_) << "The operands of <= must of type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Bool)->get_type();
}

Symbol neg_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    if (t1 != Int) {
        ct->semant_error(class_) << "The operand of ~ must have type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol plus_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 != Int || t2 != Int) {
        ct->semant_error(class_) << "The operands of + must of type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol sub_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 != Int || t2 != Int) {
        ct->semant_error(class_) << "The operands of - must of type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol mul_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 != Int || t2 != Int) {
        ct->semant_error(class_) << "The operands of * must of type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol divide_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 != Int || t2 != Int) {
        ct->semant_error(class_) << "The operands of / must of type Int" << endl;
        return set_type(Object)->get_type();
    }
    return set_type(Int)->get_type();
}

Symbol eq_class::typecheck(Class_ class_) {
    Symbol t1 = e1->typecheck(class_);
    Symbol t2 = e2->typecheck(class_);
    if (t1 == Int || t1 == Str || t1 == Bool || t2 == Int || t2 == Str || t2 == Bool) {
        if (t1 != t2) {
            ct->semant_error(class_) <<
            "If one of the operands has type Int, Str, or Bool, then they have to be the same" << endl;
            return set_type(Object)->get_type();
        }
    }
    return set_type(Bool)->get_type();
}
