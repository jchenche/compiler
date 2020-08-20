#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <functional>

using std::unordered_map;
using std::vector;
using std::max;

enum Variable_type {Attr, Param, Local};
enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
  List<CgenNode> *nds;
  ostream& str;
  int stringclasstag;
  int intclasstag;
  int boolclasstag;

  void build_class_tag_table(CgenNode*, int&);
  void build_class_tag_table();
  void fill_local_variable_slots();
  void gather_attr_and_full_method_names(CgenNode*);

  // The following methods emit code for
  // constants and global declarations.

  void code_global_data();
  void code_global_text();
  void code_bools(int);
  void code_select_gc();
  void code_constants();

  void code_class_nameTab(CgenNode*);
  void code_class_nameTab();
  void code_class_objTab(CgenNode*);
  void code_class_objTab();

  void code_proto_Obj();
  void code_dispatchTab();

  void code_object_init();
  void code_class_methods();

  // The following creates an inheritance graph from
  // a list of classes.  The graph is implemented as
  // a tree of `CgenNode', and class names are placed
  // in the base class symbol table.

  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);
public:
  CgenClassTable(Classes, ostream& str);
  void code();
  CgenNodeP root();
};


class CgenNode : public class__class {
private: 
  CgenNodeP parentnd;                        // Parent of class
  List<CgenNode> *children;                  // Children of class
  Basicness basic_status;                    // `Basic' if class is basic
                                             // `NotBasic' otherwise

public:
  CgenNode(Class_ c,
           Basicness bstatus,
           CgenClassTableP class_table);

  void add_child(CgenNodeP child);
  List<CgenNode> *get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd() { return parentnd; }
  int basic() { return (basic_status == Basic); }
  void gather_local_slots();
  void code_init(ostream&);
  void code_methods(ostream&);
};

class BoolConst {
private: 
  int val;
public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

class Locator {
private:
  Variable_type var_type;
  int offset;
public:
  Locator(Variable_type, int);
  Variable_type get_var_type() { return var_type; }
  int get_offset() { return offset; }
};
