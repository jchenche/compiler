
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }

static void emit_fetch_bool(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

static void emit_store_bool(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

// Emit code for a constant String.
void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << Str << DISPTAB_SUFFIX;
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

// Emit code for a constant Integer.
void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << Int << DISPTAB_SUFFIX;
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

// Emit code for a constant Bool.
void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD  << Bool << DISPTAB_SUFFIX;
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////


// Class tags are based on their left-to-right DFS ordering from CgenClassTable's root()
static unordered_map<std::string, int> class_tags;

static unordered_map<std::string, std::string> right_most_descendants;

// Reserve space in the stack for local variables
// object_init_local_slots is used for object init method calls (take the max out of all attrs)
// method_local_slots is used for normal method calls
static unordered_map<std::string, int> object_init_local_slots;
static unordered_map<std::string, unordered_map<std::string, int> > method_local_slots;

// These names are used to determine the offsets for env
static unordered_map<std::string, vector<std::pair<std::string, std::string> > > attr_names;
static unordered_map<std::string, vector<std::pair<std::string, std::string> > > full_method_names;

static int label_num = 0;


//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::fill_local_variable_slots() {
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    object_init_local_slots[l->hd()->get_name()->get_string()] = 0; // Some classes have no attrs
    if (!l->hd()->basic()) l->hd()->gather_local_slots();
  }
}

void CgenClassTable::build_class_tag_table(CgenNode* nd, int& class_tag) {
  class_tags[nd->get_name()->get_string()] = class_tag++;
  for(List<CgenNode> *l = nd->get_children(); l; l = l->tl())
    build_class_tag_table(l->hd(), class_tag);
}

void CgenClassTable::build_class_tag_table() {
  int class_tag = 0;
  build_class_tag_table(root(), class_tag);
}

void CgenClassTable::get_right_most_descendants(std::string ancestor_class, CgenNode* nd) {
  List<CgenNode> *l = nd->get_children();
  if (l == NULL) { // Leaf nodes have no children
    right_most_descendants[ancestor_class] = nd->get_name()->get_string();
    return;
  }
  for( ; l->tl(); l = l->tl()) {} // Get to the right most child
  get_right_most_descendants(ancestor_class, l->hd());
}

void CgenClassTable::build_right_most_descendants() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
    get_right_most_descendants(l->hd()->get_name()->get_string(), l->hd());
}

// Go through all nodes from root to descendants to gather all attr and full_method names
void CgenClassTable::gather_attr_and_full_method_names(CgenNode* nd) {
  Features features = nd->get_features();
  for(int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->gather_variable_names(nd->get_name()->get_string(), nd);

  for(List<CgenNode> *l = nd->get_children(); l; l = l->tl())
    gather_attr_and_full_method_names(l->hd());
}

//
// Features from class A (saved as first parameter) will fill in table (full_method/attr names) for A
// and propagate themselves to fill in tables for all descendants of A recursively
//
void attr_class::gather_variable_names(std::string enclosing_class, CgenNode* nd) {
  std::string class_name = nd->get_name()->get_string();
  std::string attr_name = name->get_string();
  std::string attr_type = type_decl->get_string();
  attr_names[class_name].push_back({attr_name, attr_type});

  for(List<CgenNode> *l = nd->get_children(); l; l = l->tl())
    gather_variable_names(enclosing_class, l->hd());
}

static int first_of_pair_in_vector(vector<std::pair<std::string, std::string> > v, std::string e) {
  int size = v.size();
  for (int i = 0; i < size; ++i)
    if (e == v.at(i).first) return i;
  return -1;
}

void method_class::gather_variable_names(std::string enclosing_class, CgenNode* nd) {
  std::string class_name = nd->get_name()->get_string();
  std::string method_name = name->get_string();
  int method_idx = first_of_pair_in_vector(full_method_names[class_name], method_name);
  if (method_idx == -1) { // Not found
    full_method_names[class_name].push_back({method_name, enclosing_class + METHOD_SEP + method_name});
  } else {
    full_method_names[class_name].at(method_idx).second = enclosing_class + METHOD_SEP + method_name;
  }

  for(List<CgenNode> *l = nd->get_children(); l; l = l->tl())
    gather_variable_names(enclosing_class, l->hd());
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  if (cgen_debug) cout << "Filling local variable slots" << endl;
  fill_local_variable_slots();

  if (cgen_debug) cout << "Building class tag table" << endl;
  build_class_tag_table();
  stringclasstag = class_tags[Str->get_string()];
  intclasstag    = class_tags[Int->get_string()];
  boolclasstag   = class_tags[Bool->get_string()];

  if (cgen_debug) cout << "Building right most descendant table" << endl;
  build_right_most_descendants();

  gather_attr_and_full_method_names(root());

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}


//
// code_class_nameTab() and code_class_objTab() assume that classes get their tags based on
// their left-to-right DFS ordering from CgenClassTable's root(). Refer to build_class_tag_table()
//
void CgenClassTable::code_class_nameTab(CgenNode* nd) {
  StringEntry* class_sym;
  class_sym = stringtable.lookup_string(nd->get_name()->get_string());
  str << WORD; class_sym->code_ref(str); str << endl;
  for(List<CgenNode> *l = nd->get_children(); l; l = l->tl())
    code_class_nameTab(l->hd());
}

void CgenClassTable::code_class_nameTab() {
  str << CLASSNAMETAB << LABEL;
  code_class_nameTab(root());
}

void CgenClassTable::code_class_objTab(CgenNode* nd) {
  StringEntry* class_sym;
  class_sym = stringtable.lookup_string(nd->get_name()->get_string());
  str << WORD << class_sym << PROTOBJ_SUFFIX << endl;
  str << WORD << class_sym << CLASSINIT_SUFFIX << endl;
  for(List<CgenNode> *l = nd->get_children(); l; l = l->tl())
    code_class_objTab(l->hd());
}

void CgenClassTable::code_class_objTab() {
  str << CLASSOBJTAB << LABEL;
  code_class_objTab(root());
}

void CgenClassTable::code_dispatchTab() {
  std::string class_name;
  std::string full_method_name;
  vector<std::pair<std::string, std::string> > methods;

  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    class_name = l->hd()->get_name()->get_string();
    str << class_name << DISPTAB_SUFFIX << LABEL;

    methods = full_method_names[class_name];
    for (auto method: methods) {
      full_method_name = method.second;
      str << WORD << full_method_name << endl;
    }
  }
}

void CgenClassTable::code_proto_Obj() {
  std::string class_name;
  vector<std::pair<std::string, std::string> > attrs;
  int num_attrs;
  std::string type_name;

  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    class_name = l->hd()->get_name()->get_string();
    attrs = attr_names[class_name];
    num_attrs = attrs.size();

    str << WORD << "-1" << endl; // For the garbage collector
    str << class_name << PROTOBJ_SUFFIX << LABEL;
    str << WORD << class_tags[class_name] << endl;
    str << WORD << DEFAULT_OBJFIELDS + num_attrs << endl;
    str << WORD << class_name << DISPTAB_SUFFIX << endl;

    for (auto attr: attrs) {
      type_name = attr.second;
      str << WORD;
      if      (type_name == STRINGNAME) stringtable.lookup_string("")->code_ref(str);
      else if (type_name == INTNAME)    inttable.lookup_string("0")->code_ref(str);
      else if (type_name == BOOLNAME)   falsebool.code_ref(str);
      else str << 0; // Default value for everything else is void (represented by 0)
      str << endl;
    }
  }
}

void CgenClassTable::code_object_init() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
    l->hd()->code_init(str);
}

void CgenClassTable::code_class_methods() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
    if (!l->hd()->basic()) l->hd()->code_methods(str);
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatchTab();

  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_proto_Obj();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding object initializer" << endl;
  code_object_init();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();
}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
  class__class((const class__class &) *nd),
  parentnd(NULL),
  children(NULL),
  basic_status(bstatus)
{ 
  stringtable.add_string(name->get_string());          // Add class name to string table
}

Locator::Locator(Variable_type type, int _offset) : var_type(type), offset(_offset) {}


void CgenNode::code_methods(ostream& s) {
  for(int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->code_method_def(this, s);
}

// Method def only involves methods
void attr_class::code_method_def(CgenNode* nd, ostream& s) {}

void method_class::code_method_def(CgenNode* nd, ostream& s) {
  std::string class_name = nd->get_name()->get_string();
  std::string method_name = name->get_string();
  std::string attr_name;
  std::string formal_name;
  int num_params = formals->len();
  int offset;

  // Memory location of var name is found by offset*MULTIPLIER(reg)
  // if var type is Attr,  then reg is $s0
  // if     "    is Param, then reg is $fp
  // if     "    is Local, then reg is $fp and offset is negated
  auto env = new SymbolTable<std::string, Locator>();

  env->enterscope();
  offset = DEFAULT_OBJFIELDS;
  for (auto attr: attr_names[class_name]) {
    attr_name = attr.first;
    env->addid(attr_name, new Locator(Attr, offset++));
  }

  env->enterscope();
  offset = num_params;
  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_name = formals->nth(i)->get_name()->get_string();
    env->addid(formal_name, new Locator(Param, offset--));
  }

  s << class_name + METHOD_SEP + method_name << LABEL;
  emit_push(FP, s);
  emit_addiu(FP, SP, WORD_SIZE, s); // Set current FP to point to the old FP
  emit_addiu(SP, SP, -method_local_slots[class_name][method_name]*WORD_SIZE, s); // Reserved for local vars
  emit_push(SELF, s);
  emit_push(RA, s);
  emit_move(SELF, ACC, s);

  expr->code(nd, env, 1, s);

  emit_load(RA, 1, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(FP, 0, FP, s);
  emit_addiu(SP, SP, (num_params + 3 + method_local_slots[class_name][method_name])*WORD_SIZE, s);
  emit_return(s);

  env->exitscope();
  env->exitscope();
  delete env;
}


void CgenNode::code_init(ostream& s) {
  std::string class_name = name->get_string();
  std::string attr_name;
  int offset;
  auto env = new SymbolTable<std::string, Locator>();

  env->enterscope();
  offset = DEFAULT_OBJFIELDS;
  for (auto attr: attr_names[class_name]) {
    attr_name = attr.first;
    env->addid(attr_name, new Locator(Attr, offset++));
  }

  s << class_name << CLASSINIT_SUFFIX << LABEL;
  emit_push(FP, s);
  emit_addiu(FP, SP, WORD_SIZE, s); // Set current FP to point to the old FP
  emit_addiu(SP, SP, -object_init_local_slots[class_name]*WORD_SIZE, s); // Reserved for local vars
  emit_push(SELF, s);
  emit_push(RA, s);
  emit_move(SELF, ACC, s);

  if (name != Object) {
    s << JAL; emit_init_ref(parentnd->get_name(), s); s << endl;
  }

  for(int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->code_attr_init(this, env, s);

  emit_move(ACC, SELF, s);
  emit_load(RA, 1, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(FP, 0, FP, s);
  emit_addiu(SP, SP, (3 + object_init_local_slots[class_name])*WORD_SIZE, s);
  emit_return(s);

  env->exitscope();
  delete env;
}

// Object init only involves attrs
void method_class::code_attr_init(CgenNode* nd, SymbolTable<std::string, Locator>* env, ostream& s) {}

void attr_class::code_attr_init(CgenNode* nd, SymbolTable<std::string, Locator>* env, ostream& s) {
  emit_partial_load_address(ACC, s);
  if      (type_decl == Str)  stringtable.lookup_string("")->code_ref(s);
  else if (type_decl == Int)  inttable.lookup_string("0")->code_ref(s);
  else if (type_decl == Bool) falsebool.code_ref(s);
  else s << 0; // Default value for everything else is void (represented by 0)
  s << endl;

  init->code(nd, env, 1, s); // Could be no_expr(), hence the above code

  Locator* locator = env->lookup(name->get_string());
  char* reg = FP;
  int offset = locator->get_offset();
  if (locator->get_var_type() == Attr) reg = SELF;
  if (locator->get_var_type() == Local) offset = -offset;

  emit_store(ACC, offset, reg, s);
}



void assign_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  expr->code(nd, env, local_slot, s);
  Locator* locator = env->lookup(name->get_string());
  char* reg = FP;
  int offset = locator->get_offset();
  if (locator->get_var_type() == Attr) reg = SELF;
  if (locator->get_var_type() == Local) offset = -offset;
  emit_store(ACC, offset, reg, s);
}

void static_dispatch_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(nd, env, local_slot, s);
    emit_push(ACC, s);
  }

  expr->code(nd, env, local_slot, s);
  emit_bne(ACC, ZERO, label_num, s); // Make sure we are not dispatching on void
  emit_load_string(ACC, stringtable.lookup_string(nd->get_filename()->get_string()), s);
  emit_load_imm(T1, 1, s);
  s << JAL << "_dispatch_abort" << endl;

  emit_label_def(label_num++, s);
  emit_partial_load_address(T1, s); s << type_name << DISPTAB_SUFFIX << endl;

  int method_idx = first_of_pair_in_vector(full_method_names[type_name->get_string()], name->get_string());
  assert(method_idx != -1); // static dispatch type must have the method we are dispatching (semantic analysis)

  emit_load(T1, method_idx, T1, s);
  emit_jalr(T1, s);
}

void dispatch_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(nd, env, local_slot, s);
    emit_push(ACC, s);
  }

  expr->code(nd, env, local_slot, s);
  emit_bne(ACC, ZERO, label_num, s); // Make sure we are not dispatching on void
  emit_load_string(ACC, stringtable.lookup_string(nd->get_filename()->get_string()), s);
  emit_load_imm(T1, 1, s);
  s << JAL << "_dispatch_abort" << endl;

  emit_label_def(label_num++, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);

  Symbol static_type = expr->get_type();
  if (static_type == SELF_TYPE) static_type = nd->get_name();
  int method_idx = first_of_pair_in_vector(full_method_names[static_type->get_string()], name->get_string());
  assert(method_idx != -1); // Static type of expr must have the method we are dispatching (semantic analysis)

  emit_load(T1, method_idx, T1, s);
  emit_jalr(T1, s);
}

void cond_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  int else_label = label_num++;
  int after_cond_label = label_num++;

  pred->code(nd, env, local_slot, s);
  emit_fetch_bool(T1, ACC, s);  
  emit_beqz(T1, else_label, s);

  then_exp->code(nd, env, local_slot, s);
  emit_branch(after_cond_label, s);

  emit_label_def(else_label, s);
  else_exp->code(nd, env, local_slot, s);

  emit_label_def(after_cond_label, s);
}

void loop_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  int loop_label = label_num++;
  int end_label = label_num++;

  emit_label_def(loop_label, s);

  pred->code(nd, env, local_slot, s);
  emit_fetch_bool(T1, ACC, s);
  emit_beqz(T1, end_label, s);

  body->code(nd, env, local_slot, s);

  emit_branch(loop_label, s);
  emit_label_def(end_label, s);
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  int done_label = label_num++;
  int case_label = label_num++;

  std::string branch_class_name;

  std::vector<Case> branches;
  for(int i = cases->first(); cases->more(i); i = cases->next(i))
    branches.push_back(cases->nth(i));

  // Sort the case branches in descending order based on their declared type's tag
  // because the spec wants the closest ancestor (the bigger the tag, the further away from the root)
  std::sort(branches.begin(), branches.end(),
            [](Case lhs, Case rhs) {
              return class_tags[lhs->get_type_decl()->get_string()] >
                     class_tags[rhs->get_type_decl()->get_string()]; });

  expr->code(nd, env, local_slot, s);
  emit_bne(ACC, ZERO, case_label, s);
  emit_load_string(ACC, stringtable.lookup_string(nd->get_filename()->get_string()), s);
  emit_load_imm(T1, 1, s);
  s << JAL << "_case_abort2" << endl;

  emit_label_def(case_label, s);
  emit_load(T1, 0, ACC, s); // Get the class tag of the value of expr

  // Based on how class tags are assigned (left-to-right DFS ordering),
  // Every tag (a number) within class A's tag and its right most descendant's tag is a descendant of A
  for(auto branch: branches) {
    case_label = label_num++;

    branch_class_name = branch->get_type_decl()->get_string();
    emit_blti(T1, class_tags[branch_class_name], case_label, s);
    emit_bgti(T1, class_tags[right_most_descendants[branch_class_name]], case_label, s);

    branch->code(nd, env, local_slot, s); // Matched with this branch
    emit_branch(done_label, s);

    emit_label_def(case_label, s);
  }

  s << JAL << "_case_abort" << endl; // If it gets here, it means there are no matching branches

  emit_label_def(done_label, s);
}

void branch_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  env->enterscope();
  env->addid(name->get_string(), new Locator(Local, local_slot));
  emit_store(ACC, -local_slot, FP, s);
  expr->code(nd, env, local_slot + 1, s);
  env->exitscope();
}

void block_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  for(int i = body->first(); body->more(i); i = body->next(i))
    body->nth(i)->code(nd, env, local_slot, s);
}

void let_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  emit_partial_load_address(ACC, s);
  if      (type_decl == Str)  stringtable.lookup_string("")->code_ref(s);
  else if (type_decl == Int)  inttable.lookup_string("0")->code_ref(s);
  else if (type_decl == Bool) falsebool.code_ref(s);
  else s << 0; // Default value for everything else is void (represented by 0)
  s << endl;

  init->code(nd, env, local_slot, s); // Could be no_expr(), hence the above code

  env->enterscope();
  env->addid(identifier->get_string(), new Locator(Local, local_slot));
  emit_store(ACC, -local_slot, FP, s);
  body->code(nd, env, local_slot + 1, s);
  env->exitscope();
}

static void code_arithmetic(Expression e1, Expression e2,
                            CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s,
                            std::function<void(char*, char*, char*, ostream&)> arith_emitter) {
  e1->code(nd, env, local_slot, s);
  emit_push(ACC, s);
  e2->code(nd, env, local_slot, s);
  s << JAL << Object << METHOD_SEP << "copy" << endl;
  emit_pop(T1, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  arith_emitter(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void plus_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  code_arithmetic(e1, e2, nd, env, local_slot, s, emit_add);
}

void sub_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  code_arithmetic(e1, e2, nd, env, local_slot, s, emit_sub);
}

void mul_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  code_arithmetic(e1, e2, nd, env, local_slot, s, emit_mul);
}

void divide_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  code_arithmetic(e1, e2, nd, env, local_slot, s, emit_div);
}

void neg_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  e1->code(nd, env, local_slot, s); // ACC now contains an Int object
  s << JAL << Object << METHOD_SEP << "copy" << endl;
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

static void code_comparison(Expression e1, Expression e2,
                            CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s,
                            std::function<void(char*, char*, int, ostream&)> comp_emitter) {
  e1->code(nd, env, local_slot, s);
  emit_push(ACC, s);  
  e2->code(nd, env, local_slot, s);
  emit_move(T2, ACC, s);
  emit_fetch_int(T2, T2, s);
  emit_pop(T1, s);
  emit_fetch_int(T1, T1, s);
  emit_load_bool(ACC, BoolConst(1), s);
  comp_emitter(T1, T2, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

void lt_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  code_comparison(e1, e2, nd, env, local_slot, s, emit_blt);
}

void leq_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  code_comparison(e1, e2, nd, env, local_slot, s, emit_bleq);
}

void eq_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  e1->code(nd, env, local_slot, s);
  emit_push(ACC, s);
  e2->code(nd, env, local_slot, s);
  emit_move(T2, ACC, s);
  emit_pop(T1, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beq(T1, T2, label_num, s);
  emit_load_bool(A1, BoolConst(0), s);
  s << JAL << "equality_test" << endl;
  emit_label_def(label_num++, s);
}

void comp_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  e1->code(nd, env, local_slot, s); // ACC now contains a Bool object
  emit_fetch_bool(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

void int_const_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream& s) {
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream& s) {
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream& s) {
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  // Get the class tag
  if (type_name == SELF_TYPE) emit_load(T2, 0, SELF, s);
  else                        emit_load_imm(T2, class_tags[type_name->get_string()], s);

  // tag * 8 of class_objTab has the protObj for the class associated with the tag
  emit_load_address(T1, CLASSOBJTAB, s);
  emit_sll(T2, T2, 3, s);
  emit_add(T1, T1, T2, s);
  emit_load(ACC, 0, T1, s);

  emit_push(T1, s);
  s << JAL << Object << METHOD_SEP << "copy" << endl;
  emit_pop(T1, s);

  // tag * 8 + 4 of class_objTab has the obj init for the class associated with the tag
  emit_load(T1, 1, T1, s);
  emit_jalr(T1, s);
}

void isvoid_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  e1->code(nd, env, local_slot, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

// Emit nothing for no_expr() because it simply doesn't have anything
void no_expr_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {}

void object_class::code(CgenNode* nd, SymbolTable<std::string, Locator>* env, int local_slot, ostream &s) {
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  }
  Locator* locator = env->lookup(name->get_string());
  char* reg = FP;
  int offset = locator->get_offset();
  if (locator->get_var_type() == Attr) reg = SELF;
  if (locator->get_var_type() == Local) offset = -offset;
  emit_load(ACC, offset, reg, s);
}



void CgenNode::gather_local_slots() {
  for(int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->gather_local_slots(this);
}

void method_class::gather_local_slots(Class_ class_) {
  std::string class_name = class_->get_name()->get_string();
  method_local_slots[class_name][name->get_string()] = expr->num_locals();
}

void attr_class::gather_local_slots(Class_ class_) {
  std::string class_name = class_->get_name()->get_string();
  object_init_local_slots[class_name] = max(object_init_local_slots[class_name], init->num_locals());
}

int assign_class::num_locals() { return expr->num_locals(); }

int static_dispatch_class::num_locals() {
  int num_locals = expr->num_locals();
  for(int i = actual->first(); actual->more(i); i = actual->next(i))
    num_locals = max(num_locals, actual->nth(i)->num_locals());
  return num_locals;
}

int dispatch_class::num_locals() {
  int num_locals = expr->num_locals();
  for(int i = actual->first(); actual->more(i); i = actual->next(i))
    num_locals = max(num_locals, actual->nth(i)->num_locals());
  return num_locals;
}

int cond_class::num_locals() {
  return max(pred->num_locals(), max(then_exp->num_locals(), else_exp->num_locals()));
}

int loop_class::num_locals() { return max(pred->num_locals(), body->num_locals()); }

int typcase_class::num_locals() {
  int num_locals = expr->num_locals();
  for(int i = cases->first(); cases->more(i); i = cases->next(i))
    num_locals = max(num_locals, cases->nth(i)->num_locals());
  return num_locals;
}

int branch_class::num_locals() { return 1 + expr->num_locals(); }

int block_class::num_locals() {
  int num_locals = 0;
  for(int i = body->first(); body->more(i); i = body->next(i))
    num_locals = max(num_locals, body->nth(i)->num_locals());
  return num_locals;
}

int let_class::num_locals() { return max(init->num_locals(), 1 + body->num_locals()); }
int plus_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int sub_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int mul_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int divide_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int neg_class::num_locals() { return e1->num_locals(); }
int lt_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int eq_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int leq_class::num_locals() { return max(e1->num_locals(), e2->num_locals()); }
int comp_class::num_locals() { return e1->num_locals(); }
int int_const_class::num_locals() { return 0; }
int string_const_class::num_locals() { return 0; }
int bool_const_class::num_locals() { return 0; }
int new__class::num_locals() { return 0; }
int isvoid_class::num_locals() { return e1->num_locals(); }
int no_expr_class::num_locals() { return 0; }
int object_class::num_locals() { return 0; }

