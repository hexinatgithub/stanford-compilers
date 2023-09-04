
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

static void emit_comp(char *dest, char *src, ostream& s)
{ s << NOR << dest << " " << src << " " << ZERO << endl; }

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

static void emit_dispatch_abort(ostream& s)
{ s << JAL << DISPATCH_ABORT << endl; }

static void emit_equality_test(ostream& s)
{ s << JAL << EQUALITY_TEST << endl; }

static void emit_case_abort(ostream& s)
{ s << JAL << _CASE_ABORT << endl; }

static void emit_case_abort2(ostream& s)
{ s << JAL << _CASE_ABORT2 << endl; }

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

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/
      emit_disptable_ref(Str, s);
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

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      emit_disptable_ref(Int, s);
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
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      emit_disptable_ref(Bool, s);
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

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
  str << GLOBAL << DISPATCH_ABORT << endl;
  str << GLOBAL << _CASE_ABORT << endl;
  str << GLOBAL << _CASE_ABORT2 << endl;
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


CgenClassTable::CgenClassTable(Classes classes, ostream& s) :
  nds(NULL), str(s), next(0), label(0)
{
   stringclasstag = next_class_tag() /* Change to your String class tag here */;
   intclasstag =    next_class_tag() /* Change to your Int class tag here */;
   boolclasstag =   next_class_tag() /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

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
     Basic,this,intclasstag));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this,boolclasstag));

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
        Basic,this,stringclasstag));

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



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "analyze layout for each class" << endl;
  analyze_layout();

  if (cgen_debug) cout << "coding prototype objects" << endl; 
  code_prototype_objects();
  
  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  if (cgen_debug) cout << "coding object initializer" << endl;
  code_objects_initializer();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();
}

void CgenClassTable::analyze_layout() {
  analyze_layout(root());
}

void CgenClassTable::analyze_layout(CgenNodeP nd) {
  DeclareP method;
  for (int i = nd->features->first(); nd->features->more(i); i = nd->features->next(i)) {
    Feature feat = nd->features->nth(i);
    feat->layout(nd);
  }

  emit_disptable_ref(nd->get_name(), str); str << LABEL;
  for (DeclareP method: nd->get_method_slot_to_declare()) {
    str << WORD; method->code_ref(str); str << endl;
  }

  for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
    CgenNodeP child = l->hd();
    child->set_attr_slot_to_declare(nd->get_attr_slot_to_declare());
    child->set_attr_table(nd->get_attr_table());
    child->set_method_slot_to_declare(nd->get_method_slot_to_declare());
    child->set_method_table(nd->get_method_table());
    analyze_layout(l->hd());
  }
}

void CgenClassTable::code_prototype_objects() {
  code_prototype_objects(root());
}

void CgenClassTable::code_prototype_objects(CgenNodeP nd) {
  Symbol type = nd->get_name();
  if (type == Int || type == Bool || type == Str) {
    nd->code_def(str);
  } else {
    nd->code_def(str, next_class_tag());
  }
  for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
    code_prototype_objects(l->hd());
  }
}

void CgenClassTable::code_class_nameTab() {
  str << GLOBAL << CLASSNAMETAB << endl;
  str << CLASSNAMETAB << LABEL;
  code_class_nameTab(root());
}

void CgenClassTable::code_class_nameTab(CgenNodeP nd) {
  str << WORD;
  stringtable.lookup_string(nd->get_name()->get_string())->code_ref(str);
  str << endl;
  for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
    code_class_nameTab(l->hd());
  }
}

void CgenClassTable::code_class_objTab() {
  str << GLOBAL << CLASSOBJTAB << endl;
  str << CLASSOBJTAB << LABEL;
  code_class_objTab(root());
}

void CgenClassTable::code_class_objTab(CgenNodeP nd) {
  str << WORD; emit_protobj_ref(nd->get_name(), str); str << endl;
  str << WORD; emit_init_ref(nd->get_name(), str); str << endl;
  for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
    code_class_objTab(l->hd());
  }
}

void CgenClassTable::code_objects_initializer() {
  code_objects_initializer(root());
}

void CgenClassTable::code_objects_initializer(CgenNodeP nd) {
  nd->code_init(str);
  for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
    code_objects_initializer(l->hd());
  }
}

void CgenClassTable::code_class_methods() {
  code_class_methods(root());
}

void CgenClassTable::code_class_methods(CgenNodeP nd) {
  nd->code_methods(str);
  for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
    code_class_methods(l->hd());
  }
}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

int CgenClassTable::next_class_tag() {
  return next++;
}

int CgenClassTable::next_label() {
  return label++;
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   class_table(ct),
   env(new Environment(class_table, this))
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
   stringtable.add_string(nd->get_filename()->get_string());
}

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct, int tag) :
  CgenNode(nd, bstatus, ct)
{ 
  class_tag = tag;
}

void CgenNode::code_def(ostream& str) {
  code_def(str, class_tag);
}

void CgenNode::code_def(ostream& str, int tag) {
  class_tag = tag;

  // Add -1 eye catcher
  str << WORD << "-1" << endl;

  emit_protobj_ref(name, str); str << LABEL
      << WORD << class_tag << endl
      << WORD << (DEFAULT_OBJFIELDS + get_attrs_slots()) << endl
      << WORD ; emit_disptable_ref(name, str); str << endl;
  for (int i = 0; i < get_attrs_slots(); i++)
      str << WORD << 0 << endl;
}

void CgenNode::code_init(ostream& str) {
  env->enterscope();
  emit_init_ref(name, str); str << LABEL;
  emit_push(FP, str);
  emit_move(FP, SP, str);
  emit_push(RA, str);
  env->push_tmp(SELF, self, str);
  env->push_tmp(ACC, str);
  emit_move(SELF, ACC, str);

  // call parent initializations methods
  if (parent != No_class) {
    if (cgen_debug) str << "# call parent initializations methods" << endl;
    str << JAL; emit_init_ref(parentnd->get_name(), str); str << endl;
  }
  
  // initializations attributes
  int offset = DEFAULT_OBJFIELDS;
  for (DeclareP attr : attr_slot_to_declare) {
    if (cgen_debug) str << "# initializations attributes " << attr->get_name() << endl;
    attr->code_def(env, str);
    if (cgen_debug) str << "# store result to attributes" << endl;
    // after initialer,
    // self pointer store in $acc, result pointer store in $t1
    emit_move(T1, ACC, str);
    emit_load(ACC, 1, SP, str);
    // set result
    emit_store(T1, offset, ACC, str);
    // notify GC of attribute assignment
    env->lookup(attr->get_name())->gc_assign(str);
    offset += 1;
  }

  if (cgen_debug) str << "# restore stack and registers" << endl;
  emit_load(ACC, 1, SP, str);
  emit_load(SELF, 2, SP, str);
  emit_load(RA, 0, FP, str);
  emit_load(FP, 1, FP, str);
  emit_addiu(SP, SP, 4 * WORD_SIZE, str);
  emit_return(str);
  env->pop_tmp(2);
  env->exitscope();
}

void CgenNode::code_methods(ostream& str) {
  for (DeclareP method : method_slot_to_declare) {
    method->code_def(env, str);
  }
}

std::list<DeclareP> CgenNode::get_attr_slot_to_declare() {
  return attr_slot_to_declare;
};

int CgenNode::get_attrs_slots() {
  return attr_slot_to_declare.size();
};

void CgenNode::set_attr_slot_to_declare(std::list<DeclareP> parent) {
  for (DeclareP attr : parent) {
    attr_slot_to_declare.push_back(attr);
  }
};

void CgenNode::add_attr_declare(Feature f, Symbol name) {
  attr_slot_to_declare.push_back(new Declare(this, f, name));
};

AttrTable CgenNode::get_attr_table() {
  return attrTable;
};

void CgenNode::set_attr_table(AttrTable parent) {
  for (int i = parent.first(); parent.more(i); i = parent.next(i)) {
    attrTable.add_string(parent.lookup(i)->get_string());
  }
};

void CgenNode::add_attr(char *s) {
  attrTable.add_string(s);
};

int CgenNode::get_attr_offset(Symbol name) {
  return attrTable.lookup_string(name->get_string())->offset();
};

std::list<DeclareP> CgenNode::get_method_slot_to_declare() {
  return method_slot_to_declare;
};

int CgenNode::get_methods_slots() {
  return method_slot_to_declare.size();
};

void CgenNode::set_method_slot_to_declare(std::list<DeclareP> parent) {
  for (DeclareP method : parent) {
    method_slot_to_declare.push_back(method);
  }
};

void CgenNode::add_method_declare(Feature f, Symbol name) {
  method_slot_to_declare.push_back(new Declare(this, f, name));
};

MethodTable CgenNode::get_method_table() {
  return methodTable;
};

void CgenNode::set_method_table(MethodTable parent) {
  for (int i = parent.first(); parent.more(i); i = parent.next(i)) {
    methodTable.add_string(parent.lookup(i)->get_string());
  }
};

void CgenNode::add_method(char *s) {
  methodTable.add_string(s);
};

int CgenNode::get_method_offset(Symbol name) {
  return methodTable.lookup_string(name->get_string())->offset();
};

int CgenNode::get_class_tag() {
  return class_tag;
};

///////////////////////////////////////////////////////////////////////
//
// Environment methods
//
///////////////////////////////////////////////////////////////////////

LocationP Environment::lookup(Symbol id) {
  LocationP loc = table.lookup(id);
  if (loc == NULL) {
    if (id == self)
      return new SelfLocation();
    else
      return new AttrLocation(get_attr_offset(id));
  }
  return loc;
}

void Environment::add_argument(Symbol id) {
  table.addid(id, new StackLocation(args + 2));
  args++;
}

void Environment::push_tmp(char *reg, Symbol id, ostream& s) {
  int *tmp = tmpl->hd();
  table.addid(id, new StackLocation(-*tmp-1));
  emit_push(reg, s);
  *tmp += 1;
}

void Environment::push_tmp(char *reg, ostream& s) {
  int *tmp = tmpl->hd();
  emit_push(reg, s);
  *tmp += 1;
}

void Environment::pop_tmp() {
  pop_tmp(1);
}

void Environment::pop_tmp(int i) {
  int *tmp = tmpl->hd();
  *tmp -= i;
}

void Environment::enterscope() {
  int *tmp = new int(0);
  tmpl = new List<int>(tmp, tmpl);
  enterblock();
}

void Environment::exitscope() {
  delete tmpl->hd();
  tmpl = tmpl->tl();
  exitblock();
}

void Environment::enterblock() {
  table.enterscope();
}

void Environment::exitblock() {
  table.exitscope();
}

int Environment::next_label() {
  return class_table->next_label();
};

int Environment::get_class_tag() {
  return nd->get_class_tag();
};

StringEntryP Environment::get_class_name() {
  return stringtable.lookup_string(nd->get_name()->get_string());
};

StringEntryP Environment::get_filename() {
  return stringtable.lookup_string(nd->get_filename()->get_string());
};

int Environment::lookup_class_tag(Symbol class_name) {
  return class_table->lookup(class_name)->get_class_tag();
};

int Environment::get_attr_offset(Symbol name) {
  return nd->get_attr_offset(name);
};

int Environment::get_attr_offset(Symbol class_name, Symbol name) {
  if (class_name == SELF_TYPE) {
    return get_attr_offset(name);
  }
  return class_table->lookup(class_name)->get_attr_offset(name);
};

int Environment::get_method_offset(Symbol name) {
  return nd->get_method_offset(name);
};

int Environment::get_method_offset(Symbol class_name, Symbol name) {
  if (class_name == SELF_TYPE) {
    return get_method_offset(name);
  }
  return class_table->lookup(class_name)->get_method_offset(name);
}

Environment::Environment(CgenClassTableP class_table, CgenNodeP nd) :
  class_table(class_table), nd(nd), tmpl(NULL), args(0) {};

///////////////////////////////////////////////////////////////////////
//
// Location methods
//
///////////////////////////////////////////////////////////////////////

void AttrLocation::load(char *dest, ostream& s) {
  emit_load(dest, offset + DEFAULT_OBJFIELDS, SELF, s);
}

void AttrLocation::store(char *src, ostream& s) {
  emit_store(src, offset + DEFAULT_OBJFIELDS, SELF, s);
}

void AttrLocation::gc_assign(ostream& s) {
  if (cgen_Memmgr == GC_NOGC) return;
  s << "# gc assign" << endl;
  emit_addiu(A1, SELF, (offset + DEFAULT_OBJFIELDS) * WORD_SIZE, s);
  emit_jal("_GenGC_Assign", s);
}

void StackLocation::load(char *dest, ostream& s) {
  emit_load(dest, offset, FP, s);
}

void StackLocation::store(char *src, ostream& s) {
  emit_store(src, offset, FP, s);
}

void StackLocation::gc_assign(ostream& s) {
  // do nothing
}

void RegisterLocation::load(char *dest, ostream& s) {
  emit_move(dest, reg, s);
}

void RegisterLocation::store(char *src, ostream& s) {
  emit_move(reg, src, s);
}

void RegisterLocation::gc_assign(ostream& s) {
  // do nothing
}

///////////////////////////////////////////////////////////////////////
//
// Declare methods
//
///////////////////////////////////////////////////////////////////////

void Declare::code_def(EnvironmentP env, ostream& s) {
  f->code_def(env, s);
}

void Declare::code_ref(ostream& s) {
  emit_method_ref(nd->get_name(), name, s);
}

Symbol Declare::get_name() {
  return name;
}

Declare::Declare(CgenNodeP nd, Feature f, Symbol name) :
  nd(nd), f(f), name(name) {}

///////////////////////////////////////////////////////////////////////
//
// Feature methods
//
///////////////////////////////////////////////////////////////////////

void attr_class::layout(CgenNodeP nd) {
  nd->add_attr_declare(this, name);
  nd->add_attr(name->get_string());
}

void attr_class::code_def(EnvironmentP env, ostream& str) {
  if (init->get_type()) {
    init->code(env, str);
  } else if (type_decl == Int) {
    emit_load_int(ACC, inttable.lookup_string("0"), str);
  } else if (type_decl == Bool) {
    emit_load_bool(ACC, falsebool, str);
  } else if (type_decl == Str) {
    emit_load_string(ACC, stringtable.lookup_string(""), str);
  } else {
    emit_move(ACC, ZERO, str);
  }
}

void method_class::layout(CgenNodeP nd) {
  nd->add_method_declare(this, name);
  nd->add_method(name->get_string());
}

void method_class::code_def(EnvironmentP env, ostream& str) {
  LocationP loc;
  Symbol type = idtable.lookup_string(env->get_class_name()->get_string());

  if (type == Object || type == IO || type == Str)
    return;

  // set up environment
  env->enterscope();
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    env->add_argument(formals->nth(i)->get_name());
  }

  str << GLOBAL; emit_method_ref(env->get_class_name(), name, str); str << endl;
  emit_method_ref(env->get_class_name(), name, str); str << LABEL;
  // set up AR
  emit_push(FP, str);
  emit_move(FP, SP, str);
  emit_push(RA, str);
  // save SELF on stack
  // set SELF to ACC
  env->push_tmp(SELF, str);
  emit_move(SELF, ACC, str);

  if (cgen_debug) str << "# method body" << endl;
  expr->code(env, str);

  if (cgen_debug) str << "# method AR restore" << endl;
  // restore SELF
  emit_load(SELF, 1, SP, str);
  emit_load(RA, 0, FP, str);
  emit_load(FP, 1, FP, str);
  emit_addiu(SP, SP, (formals->len() + 3) * WORD_SIZE, str);
  emit_return(str);
  env->exitscope();
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(EnvironmentP env, ostream& s) {
  LocationP loc = env->lookup(name);
  expr->code(env, s);
  if (cgen_debug) s << "# assign to " << name << endl;
  loc->store(ACC, s);
  loc->gc_assign(s);
}

void static_dispatch_class::code(EnvironmentP env, ostream& s) {
  int static_dispatch_label = env->next_label();
  expr->code(env, s);
  if (cgen_debug) s << "# static dispatch to " << type_name << "." << name << endl;
  emit_bne(ACC, ZERO, static_dispatch_label, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_load_string(ACC, env->get_filename(), s);
  emit_dispatch_abort(s);

  emit_label_def(static_dispatch_label, s);
  env->push_tmp(ACC, s);
  if (cgen_debug) s << "# push actuals" << endl;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression a = actual->nth(actual->len() - 1 - i);
    a->code(env, s);
    env->push_tmp(ACC, s);
  }

  if (cgen_debug) s << "# call method " << type_name << "." << name << endl;
  emit_load(ACC, actual->len() + 1, SP, s);
  s << JAL; emit_method_ref(type_name, name, s); s << endl;
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp(actual->len() + 1);
}

void dispatch_class::code(EnvironmentP env, ostream& s) {
  int dispatch_label = env->next_label();
  expr->code(env, s);
  if (cgen_debug) s << "# dispatch to " << expr->get_type() << "." << name << endl;
  emit_bne(ACC, ZERO, dispatch_label, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_load_string(ACC, env->get_filename(), s);
  emit_dispatch_abort(s);

  emit_label_def(dispatch_label, s);
  env->push_tmp(ACC, s);
  if (cgen_debug) s << "# push actuals" << endl;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression a = actual->nth(actual->len() - 1 - i);
    a->code(env, s);
    env->push_tmp(ACC, s);
  }

  if (cgen_debug) s << "# call method " << expr->get_type() << "." << name << endl;
  emit_load(ACC, actual->len() + 1, SP, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, env->get_method_offset(expr->get_type(), name), T1, s);
  emit_jalr(T1, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp(actual->len() + 1);
}

void cond_class::code(EnvironmentP env, ostream& s) {
  int false_label = env->next_label(),
    end_if_label = env->next_label();
  if (cgen_debug) s << "# code cond" << endl;
  pred->code(env, s);
  emit_load_imm(A1, 0, s);
  emit_move(T1, ACC, s);
  emit_load_bool(T2, truebool, s);
  emit_equality_test(s);
  emit_beq(ACC, ZERO, false_label, s);
  then_exp->code(env, s);
  emit_branch(end_if_label, s);
  emit_label_def(false_label, s);
  else_exp->code(env, s);
  emit_label_def(end_if_label, s);
}

void loop_class::code(EnvironmentP env, ostream& s) {
  int loop_start_label = env->next_label(),
    loop_end_label = env->next_label();
  if (cgen_debug) s << "# code loop" << endl;
  emit_label_def(loop_start_label, s);
  pred->code(env, s);
  emit_load_imm(A1, 0, s);
  emit_move(T1, ACC, s);
  emit_load_bool(T2, truebool, s);
  emit_equality_test(s);
  emit_beq(ACC, ZERO, loop_end_label, s);
  body->code(env, s);
  emit_branch(loop_start_label, s);
  emit_label_def(loop_end_label, s);
}

void typcase_class::code(EnvironmentP env, ostream& s) {
  int typcase_label = env->next_label(), 
    not_matched_label = env->next_label(),
    matched_label = env->next_label(),
    next_case_label;
  if (cgen_debug) s << "# code typecase" << endl;
  expr->code(env, s);
  emit_bne(ACC, ZERO, typcase_label, s);
  emit_load_imm(T1, get_line_number(), s);
  emit_load_string(ACC, env->get_filename(), s);
  emit_case_abort2(s);

  emit_label_def(typcase_label, s);
  emit_load(T1, 0, ACC, s);
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case c = cases->nth(i);
    c->code_typecase(env, s);

    if (cases->len() == 1 || i == cases->len() - 1) {
      emit_bne(T1, T2, not_matched_label, s);
      c->code_expr(env, s);
      emit_branch(matched_label, s);
    } else {
      next_case_label = env->next_label();
      emit_bne(T1, T2, next_case_label, s);
      c->code_expr(env, s);
      emit_branch(matched_label, s);
      emit_label_def(next_case_label, s);
    }
  }
  emit_label_def(not_matched_label, s);
  emit_load_string(ACC, env->get_class_name(), s);
  emit_case_abort(s);
  emit_label_def(matched_label, s);
}

void branch_class::code_typecase(EnvironmentP env, ostream& s) {
  emit_load_imm(T2, env->lookup_class_tag(type_decl), s);
}

void branch_class::code_expr(EnvironmentP env, ostream& s) {
  env->enterblock();
  env->push_tmp(ACC, name, s);
  expr->code(env, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp();
  env->exitblock();
}

void block_class::code(EnvironmentP env, ostream& s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    expr->code(env, s);
  }
}

void let_class::code(EnvironmentP env, ostream& s) {
  if (cgen_debug) s << "# code let" << endl;
  env->enterblock();

  if (init->get_type()) {
    init->code(env, s);
  } else {
    if (type_decl == Int) {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, falsebool, s);
    } else if (type_decl == Str) {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    } else {
      emit_move(ACC, ZERO, s);
    }
  }

  if (cgen_debug) s << "# code let initializer" << endl;
  env->push_tmp(ACC, identifier, s);
  if (cgen_debug) s << "# let body" << endl;
  body->code(env, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp();
  env->exitblock();
}

void plus_class::code(EnvironmentP env, ostream& s) {
  int offset = 0;

  if (cgen_debug) s << "# code plus" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  env->push_tmp(ACC, s);

  if (cgen_debug) s << "# new Int" << endl;
  emit_load_int(ACC, inttable.lookup_string("0"), s);
  offset = env->get_method_offset(Bool, ::copy);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);

  if (cgen_debug) s << "# add" << endl;
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(T2, 2, SP, s);
  emit_load(T2, DEFAULT_OBJFIELDS, T2, s);
  emit_add(T1, T1, T2, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_addiu(SP, SP, 2 * WORD_SIZE, s);
}

void sub_class::code(EnvironmentP env, ostream& s) {
  int offset = 0;

  if (cgen_debug) s << "# code sub" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  env->push_tmp(ACC, s);

  if (cgen_debug) s << "# new Int" << endl;
  emit_load_int(ACC, inttable.lookup_string("0"), s);
  offset = env->get_method_offset(Bool, ::copy);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);

  if (cgen_debug) s << "# sub" << endl;
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(T2, 2, SP, s);
  emit_load(T2, DEFAULT_OBJFIELDS, T2, s);
  emit_sub(T1, T2, T1, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_addiu(SP, SP, 2 * WORD_SIZE, s);
}

void mul_class::code(EnvironmentP env, ostream& s) {
  int offset = 0;

  if (cgen_debug) s << "# code mul" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  env->push_tmp(ACC, s);

  if (cgen_debug) s << "# new Int" << endl;
  emit_load_int(ACC, inttable.lookup_string("0"), s);
  offset = env->get_method_offset(Bool, ::copy);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);

  if (cgen_debug) s << "# mul" << endl;
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(T2, 2, SP, s);
  emit_load(T2, DEFAULT_OBJFIELDS, T2, s);
  emit_mul(T1, T1, T2, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_addiu(SP, SP, 2 * WORD_SIZE, s);
}

void divide_class::code(EnvironmentP env, ostream& s) {
  int offset = 0;

  if (cgen_debug) s << "# code divide" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  env->push_tmp(ACC, s);

  if (cgen_debug) s << "# new Int" << endl;
  emit_load_int(ACC, inttable.lookup_string("0"), s);
  offset = env->get_method_offset(Bool, ::copy);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);

  if (cgen_debug) s << "# divide" << endl;
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(T2, 2, SP, s);
  emit_load(T2, DEFAULT_OBJFIELDS, T2, s);
  emit_div(T1, T2, T1, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_addiu(SP, SP, 2 * WORD_SIZE, s);
}

void neg_class::code(EnvironmentP env, ostream& s) {
  e1->code(env, s);
  if (cgen_debug) s << "# code neg" << endl;
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_neg(T1, T1, s);
  emit_push(T1, s);

  if (cgen_debug) s << "# new Int" << endl;
  emit_load_int(ACC, inttable.lookup_string("0"), s);
  int offset = env->get_method_offset(Bool, ::copy);
  emit_load(T2, DISPTABLE_OFFSET, ACC, s);
  emit_load(T2, offset, T2, s);
  emit_jalr(T2, s);

  if (cgen_debug) s << "# store result" << endl;
  emit_load(T1, 1, SP, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
}

void lt_class::code(EnvironmentP env, ostream& s) {
  int lt_label = env->next_label(),
    end_label = env->next_label();
  if (cgen_debug) s << "# code lt" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);

  emit_blt(T1, ACC, lt_label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);
  emit_label_def(lt_label, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_label, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp();
}

void eq_class::code(EnvironmentP env, ostream& s) {
  int eq_label = env->next_label(),
    end_label = env->next_label();
  if (cgen_debug) s << "# code eq" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);

  emit_beq(T1, ACC, eq_label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);
  emit_label_def(eq_label, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_label, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp();
}

void leq_class::code(EnvironmentP env, ostream& s) {
  int leq_label = env->next_label(),
    end_label = env->next_label();
  if (cgen_debug) s << "# code leq" << endl;
  e1->code(env, s);
  env->push_tmp(ACC, s);
  e2->code(env, s);
  emit_load(T1, 1, SP, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);

  emit_bleq(T1, ACC, leq_label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);
  emit_label_def(leq_label, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_label, s);
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp();
}

void comp_class::code(EnvironmentP env, ostream& s) {
  e1->code(env, s);
  if (cgen_debug) s << "# code comp" << endl;
  emit_move(T1, ACC, s);
  emit_load_bool(T2, truebool, s);
  emit_load_bool(ACC, falsebool, s);
  emit_load_bool(A1, truebool, s);
  emit_equality_test(s);
}

void int_const_class::code(EnvironmentP env, ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(EnvironmentP env, ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(EnvironmentP env, ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(EnvironmentP env, ostream& s) {
  int offset;
  if (cgen_debug) s << "# code new" << endl;
  // load classtag in ACC
  if (type_name == SELF_TYPE) {
    emit_load(T1, 0, SELF, s);
  } else {
    emit_load_imm(T1, env->lookup_class_tag(type_name), s);
  }
  // find prototype object
  emit_sll(T1, T1, 3, s);
  emit_load_address(T2, CLASSOBJTAB, s);
  emit_addu(T1, T1, T2, s);
  env->push_tmp(T1, s);
  emit_load(ACC, 0, T1, s);
  // load dispatch table
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  // load copy method address
  offset= env->get_method_offset(type_name, ::copy);
  emit_load(T1, offset, T1, s);
  // call copy method
  emit_jalr(T1, s);
  // load init method
  emit_load(T1, 1, SP, s);
  emit_load(T1, 1, T1, s);
  // call init method
  emit_jalr(T1, s);
  // restore stack
  emit_addiu(SP, SP, WORD_SIZE, s);
  env->pop_tmp();
}

void isvoid_class::code(EnvironmentP env, ostream& s) {
  int isvoid_label = env->next_label(),
    end_label = env->next_label();
  e1->code(env, s);
  if (cgen_debug) s << "# code isvoid" << endl;
  emit_beq(ACC, ZERO, isvoid_label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);
  emit_label_def(isvoid_label, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_label, s);
}

void no_expr_class::code(EnvironmentP env, ostream& s) {
  // no ops
  if (cgen_debug) s << "# code no_expr" << endl;
  emit_load_imm(ACC, 0, s);
}

void object_class::code(EnvironmentP env, ostream& s) {
  LocationP loc = env->lookup(name);
  if (cgen_debug) s << "# code object " << name << endl;
  loc->load(ACC, s);
}


