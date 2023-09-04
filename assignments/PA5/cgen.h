#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include "list"

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

   int next;
   int label;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following methods analyze
// layout for each class.

   void analyze_layout();
   void analyze_layout(CgenNodeP nd);

// The following methods emit code for
// - prototype objects
// - class_nameTab
// - dispatch tables

   void code_prototype_objects();
   void code_prototype_objects(CgenNodeP nd);
   void code_class_nameTab();
   void code_class_nameTab(CgenNodeP nd);
   void code_class_objTab();
   void code_class_objTab(CgenNodeP nd);
   void code_objects_initializer();
   void code_objects_initializer(CgenNodeP nd);
   void code_class_methods();
   void code_class_methods(CgenNodeP nd);

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   int next_class_tag();
public:
   CgenClassTable(Classes, ostream& str);
   int next_label();
   void code();
   CgenNodeP root();
};

class AttrEntry : public Entry {
public:
   int offset() { return index; };
   AttrEntry(char *s, int l, int i) : Entry(s, l, i) {};
};

class MethodEntry : public Entry {
public:
   int offset() { return index; };
   MethodEntry(char *s, int l, int i) : Entry(s, l, i) {};
};

typedef AttrEntry *AttrEntryP;
typedef MethodEntry *MethodEntryP;

class AttrTable : public StringTable<AttrEntry> {};

class MethodTable : public StringTable<MethodEntry> {};

class Location {
public:
   virtual void load(char *dest, ostream& s) = 0;
   virtual void store(char *src, ostream& s) = 0;
   virtual void gc_assign(ostream& s) = 0;
};

class AttrLocation : public Location {
private:
   int offset;
public:
   AttrLocation(int offset) : offset(offset) {};
   void load(char *dest, ostream& s);
   void store(char *src, ostream& s);
   void gc_assign(ostream& s);
};

class StackLocation : public Location {
private:
   int offset;
public:
   StackLocation(int offset) : offset(offset) {};
   void load(char *dest, ostream& s);
   void store(char *src, ostream& s);
   void gc_assign(ostream& s);
};

class RegisterLocation : public Location {
private:
   char *reg;
public:
   RegisterLocation(char *reg) : reg(reg) {};
   void load(char *dest, ostream& s);
   void store(char *src, ostream& s);
   void gc_assign(ostream& s);
};

class SelfLocation : public RegisterLocation {
public:
   SelfLocation() : RegisterLocation(SELF) {};
};

typedef Location *LocationP;

class Environment {
private:
   SymbolTable<Symbol, StackLocation> table;
   CgenClassTableP class_table;
   CgenNodeP nd;
   List<int> *tmpl;
   int args;
public:
   LocationP lookup(Symbol id);
   void add_argument(Symbol id);
   void push_tmp(char *reg, Symbol id, ostream& s);
   void push_tmp(char *reg, ostream& s);
   void pop_tmp();
   void pop_tmp(int i);
   void enterscope();
   void exitscope();
   void enterblock();
   void exitblock();

   int next_label();
   int get_class_tag();
   StringEntryP get_class_name();
   StringEntryP get_filename();
   int lookup_class_tag(Symbol class_name);
   int get_attr_offset(Symbol name);
   int get_attr_offset(Symbol class_name, Symbol name);
   int get_method_offset(Symbol name);
   int get_method_offset(Symbol class_name, Symbol name);
   Environment(CgenClassTableP class_table,  CgenNodeP nd);
};

typedef Environment *EnvironmentP;

class Declare {
private:
   CgenNodeP nd;
   Feature f;
   Symbol name;

public:
   void code_def(EnvironmentP env, ostream& s);
   void code_ref(ostream& s);
   Symbol get_name();
   Declare(CgenNodeP nd, Feature f, Symbol name);
};

typedef Declare *DeclareP;

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

   CgenClassTableP class_table;
   EnvironmentP env;

   int class_tag;                                   // classtag
   std::list<DeclareP> attr_slot_to_declare;
   AttrTable attrTable;
   std::list<DeclareP> method_slot_to_declare;
   MethodTable methodTable;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table,
            int class_tag);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   std::list<DeclareP> get_attr_slot_to_declare();
   int get_attrs_slots();
   void set_attr_slot_to_declare(std::list<DeclareP> parent);
   void add_attr_declare(Feature f, Symbol name);

   AttrTable get_attr_table();
   void set_attr_table(AttrTable parent);
   void add_attr(char *s);
   int get_attr_offset(Symbol name);

   std::list<DeclareP> get_method_slot_to_declare();
   int get_methods_slots();
   void set_method_slot_to_declare(std::list<DeclareP> parent);
   void add_method_declare(Feature f, Symbol name);

   MethodTable get_method_table();
   void set_method_table(MethodTable parent);
   void add_method(char *s);
   int get_method_offset(Symbol name);

   int get_class_tag();

   void code_def(ostream& str, int classtag);
   void code_def(ostream& str);
   void code_init(ostream& str);
   void code_methods(ostream& str);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

