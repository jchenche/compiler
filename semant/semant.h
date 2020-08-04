#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <unordered_map>

#define TRUE 1
#define FALSE 0

class Node {
private:
	Node* parent;
	std::string class_name;
public:
	Node(std::string name) { class_name = name; parent = NULL; }
	Node* get_parent() { return parent; }
	void set_parent(Node* p) { parent = p; }
	std::string get_name() { return class_name; }
};

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  void construct_class_hierarchy(Classes classes);
  std::unordered_map<std::string, Node*> hierarchy;
  void check_for_cycles(Class_ class_, Node* class_node);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  std::unordered_map<std::string, Node*> get_hierarchy() { return hierarchy; }
};


#endif

