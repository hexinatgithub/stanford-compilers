

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
    Void,
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
    Void        = idtable.add_string("Void");
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

    /* Fill this in */
    install_basic_classes();

    // install user defined classes
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      Class_ c = classes->nth(i);
      Symbol name = c->get_name(), parent = c->get_parent();

      if (name == Object || name == Int || name == Bool || name == Str || name == IO) {
        semant_error(c) << "Class '" << name << "' can't be redefined" << endl;
        continue;
      }

      if (parent == Int || parent == Bool || parent == Str) {
        semant_error(c) << "'" << name << "'" << " can't inherite from '" << parent << "'" << endl;
        continue;
      } 
      
      if (table.lookup(name)) {
        semant_error(c) << "duplicate class '" << c->get_name() << "' declaration" << endl;
        continue;
      }

      table.addid(name, c);
    }

    /* check inheritance graph */
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      Symbol name = NULL;
      Class_ c = classes->nth(i), pre = c, next = get(pre->get_parent());
      table.enterscope();
      table.addid(c->get_name(), c);

      while(TRUE) {
        if (next == NULL) {
          semant_error(pre) << "'" << pre->get_parent() << "'" << " not defined." << endl;
          break;
        }

        name = next->get_name();
        if (name == Object)
          break;
        else if (table.probe(name)) {
          semant_error(c) << "'" << pre->get_name() << "'" << " form a inherite cycly with '" << name << "'" << endl;
          break;
        } else {
          table.addid(name, next); 
          pre = next; next = get(next->get_parent());
        }
      }
      table.exitscope();
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

    Class_ Dummy_class =
      class_(No_type,
             No_class,
             nil_Features(),
             filename);

    table.enterscope();
    table.addid(Object, Object_class);
    table.addid(IO, IO_class);
    table.addid(Int, Int_class);
    table.addid(Bool, Bool_class);
    table.addid(Str, Str_class);
    table.addid(No_type, Dummy_class);
    table.addid(prim_slot, Dummy_class);
    table.addid(Void, Dummy_class);
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

bool ClassTable::is_subclass(Symbol c1, Symbol c2, Symbol current) {
  assert(current != NULL && c1 != NULL && c2 != NULL);
  assert(current != SELF_TYPE);

  if (c1 == No_type || c1 == Void || c2 == Void) {
    return true;
  } else if (c2 == No_type) {
    return false;
  } else if (c1 == SELF_TYPE) {
    if (c2 == SELF_TYPE)
      return true;
    else
      return is_subclass(current, c2);
  } else if (c2 == SELF_TYPE) {
    return false;
  }
  return is_subclass(c1, c2);
}

bool ClassTable::is_subclass(Symbol c1, Symbol c2) {
  assert(c1 != NULL || c2 != NULL);
  while (c1 != No_class) {
    if (c1 == c2)
      return true;
    c1 = get(c1)->get_parent();
  }
  return false;
}

Symbol ClassTable::join(Symbol c1, Symbol c2, Symbol current) {
  assert(current != NULL);
  assert(c1 != NULL || c2 != NULL);
  assert(c1 != No_type && c2 != No_type && current != SELF_TYPE);

  if (c1 == NULL) {
    return c2;
  } else if (c2 == NULL) {
    return c1;
  } else if (c1 == c2) {
    return c1;
  } else if (c1 == SELF_TYPE) {
    return join(current, c2);
  } else if (c2 == SELF_TYPE) {
    return join(current, c1);
  }
  return join(c1, c2);
}

Symbol ClassTable::join(Symbol c1, Symbol c2) {
  ClassIndex ci;
  Symbol least_type = NULL;

  ci.enterscope();
  while (c1 != No_class) {
    ci.addid(c1, get(c1));
    c1 = get(c1)->get_parent();
  }

  while (c2 != No_class) {
    if (ci.probe(c2)) {
      least_type = c2;
      break;
    }
    c2 = get(c2)->get_parent();
  }
  ci.exitscope();
  return least_type;
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
void program_class::semant()
{
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  ClassTable *classtable = new ClassTable(classes);

  /* some semantic analysis code may go here */

  if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
  }

  ClassTableP &T = classtable;
  ObjectEnv O; MethodEnv M;
  Class_ C = NULL;

  // pass one: 
  // recursive construct type environment
  // detect name duplicate and type relate error.
  O.enterscope(); M.enterscope();
  C = T->get(Object); C->analysis(T, O, M, C);
  C = T->get(IO); C->analysis(T, O, M, C);
  C = T->get(Int); C->analysis(T, O, M, C);
  C = T->get(Bool); C->analysis(T, O, M, C);
  C = T->get(Str); C->analysis(T, O, M, C);

  for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
    C = classes->nth(i);
    C->analysis(T, O, M, C);
  }
  O.exitscope(); M.exitscope();

  // pass two:
  // semantic analysis and decorate AST.
  for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
    C = classes->nth(i);
    C->semant(T, O, M, C);
  }

  Class_ main_class = T->get(Main);
  if (main_class == NULL) {
    T->semant_error() << "program must have main Class" << endl;
  } else if (main_class->getM().lookup(main_meth) == NULL) {
    T->semant_error(main_class) << "main Class must have main method" << endl;
  }
}

/*
  recursive construct type environment
  detect name duplicate and type relate error.
*/

void class__class::analysis(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  if (!analysised) {
    ObjectEnv TO; MethodEnv TM;
    if (parent != No_class) {
      Class_ P = T->get(parent);
      P->analysis(T, O, M, P);
      TO = P->getO(); TM = P->getM();
    } else {
      TO = O; TM = M;
    } 

    TO.enterscope(); TM.enterscope();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      features->nth(i)->analysis(T, TO, TM, C);
    }
    CO = TO; CM = TM;
    TO.exitscope(); TM.exitscope();
    analysised = true;
  }
}

void method_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  List<Entry> *signature = NULL, *tsi = NULL, *tsj = NULL; 
  int semant_errors = 0;
  O.enterscope();
  O.addid(self, SELF_TYPE);

  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal f = formals->nth(i);
    f->analysis(T, O, M, C);
    Symbol type = O.probe(f->get_name());
    signature = new List<Entry>(type, signature);
    if (f->errors()) semant_errors++;
  }

  if (return_type != SELF_TYPE && T->get(return_type) == NULL) {
    T->semant_error(C->get_filename(), this) << "method '" << name << "' has unknown return type '" << return_type << "'" << endl;
    signature = new List<Entry>(Object, signature);
    semant_errors++;
  } else {
    signature = new List<Entry>(return_type, signature);
  }

  if (semant_errors == 0) {
    tsi = signature;
    tsj = M.lookup(name);

    if (tsj == NULL) {
      // do nothing, no parent method defined
    } else if (list_length(tsi) != list_length(tsj)) {
      T->semant_error(C->get_filename(), this) << "method '" << name << "' can't be redefined" << endl;
    } else {
      while(tsi != NULL) {
        if (tsi->hd() != tsj->hd()) {
          T->semant_error(C->get_filename(), this) << "method overload signature should be same, formal argument type should be '" << tsj->hd() << "\', " << "but be \'" << tsi->hd() << "\'" << endl;
        }
        tsi = tsi->tl(); tsj = tsj->tl();
      }
    }
  }

  M.addid(name, signature);
  expr->analysis(T, O, M, C);
  MO = O;
  O.exitscope();
}

void attr_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  if (O.lookup(name) == NULL) {
    Symbol type = type_decl;
    if (type != SELF_TYPE && T->get(type) == NULL) {
      T->semant_error(C->get_filename(), this) << "'" << name << "' has unknown type '" << type << "'" << endl;
      type = Object;
    }
    O.addid(name, type);
  } else {
    T->semant_error(C->get_filename(), this) << "dulplicate attribute declaration '" << name << "'" << endl;
  }

  O.enterscope();
  O.addid(self, SELF_TYPE);
  init->analysis(T, O, M, C);
  AO = O;
  O.exitscope();
}

void formal_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  if (O.probe(name) == NULL) {
    Symbol type = type_decl;
    if (type == SELF_TYPE) {
      T->semant_error(C->get_filename(), this) << "formal argument '" << name << "' type can't be '" << type << "'" << endl;
      type = Object;
      semant_errors++;
    } else if (T->get(type) == NULL) {
      T->semant_error(C->get_filename(), this) << "'" << name << "' has unknown type '" << type << "'" << endl;
      type = Object;
      semant_errors++;
    }
    O.addid(name, type);
  } else {
    T->semant_error(C->get_filename(), this) << "dulplicate formal argument declaration '" << name << "'" << endl;
    semant_errors++;
  }
}

void branch_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  Symbol type = type_decl;
  if (type == SELF_TYPE) {
    T->semant_error(C->get_filename(), this) << "attribute Type can't be '" << type << "'" << endl;
    type = Object;
  } else if (T->get(type) == NULL) {
    T->semant_error(C->get_filename(), this) << "unknown Type '" << type << "'" << endl;
    type = Object;
  }

  O.enterscope();
  O.addid(name, type);
  expr->analysis(T, O, M, C);
  BO = O;
  O.exitscope();
}

void assign_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  expr->analysis(T, O, M, C);
}

void static_dispatch_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  expr->analysis(T, O, M, C);
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression expr = actual->nth(i);
    expr->analysis(T, O, M, C);
  }
}

void dispatch_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  expr->analysis(T, O, M, C);
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression expr = actual->nth(i);
    expr->analysis(T, O, M, C);
  }
}

void cond_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  pred->analysis(T, O, M, C);
  then_exp->analysis(T, O, M, C);
  else_exp->analysis(T, O, M, C);
}

void loop_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  pred->analysis(T, O, M, C);
  body->analysis(T, O, M, C);
}

void typcase_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  // check cases type are all distinct
  ClassIndex dup_checker;
  dup_checker.enterscope();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case ca = cases->nth(i);
    Symbol type_decl = ca->get_type_decl();
    if (type_decl == SELF_TYPE || T->get(type_decl) == NULL) continue;

    if (dup_checker.probe(type_decl) == NULL) {
      dup_checker.addid(type_decl, NULL);
    } else {
      T->semant_error(C->get_filename(), ca) << "duplicate case Type '" << type_decl << "'" << endl;
    }
  }
  dup_checker.exitscope();

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case ca = cases->nth(i);
    ca->analysis(T, O, M, C);
  }
}

void block_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  O.enterscope();
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    expr->analysis(T, O, M, C);
  }
  O.exitscope();
}

void let_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  Symbol type = type_decl;
  if (type != SELF_TYPE && T->get(type) == NULL) {
    T->semant_error(C->get_filename(), this) << "unknown Type '" << type << "'" << endl;
    type = Object;
  }

  O.enterscope();
  O.addid(identifier, type);
  body->analysis(T, O, M, C);
  LO = O;
  O.exitscope();
}

void plus_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void sub_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void mul_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void divide_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void neg_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
}

void lt_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void eq_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void leq_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
  e2->analysis(T, O, M, C);
}

void comp_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
}

void isvoid_class::analysis(ClassTableP T, ObjectEnv &O, MethodEnv &M, Class_ C) {
  e1->analysis(T, O, M, C);
}

/* semant check */

void class__class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->semant(T, CO, CM, C);
  }
}

void method_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  expr->semant(T, MO, M, C);
  if (!T->is_subclass(expr->type, return_type, C->get_name())) {
    T->semant_error(C->get_filename(), this) \
      << "method return type is '" << return_type << "', but " \
      << "'" << expr->type << "'" << " is not subclass of " \
      << "'" << return_type << "'" << endl;
  }
}

void attr_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  Symbol tt = AO.lookup(name);
  init->semant(T, AO, M, C);

  if (!T->is_subclass(init->type, tt, C->get_name())) {
    T->semant_error(C->get_filename(), this) \
      << "attr '" << name << "' has type '" << type_decl << "', but" \
      << "'" << init->type << "'" << " is not subclass of " \
      << "'" << type_decl << "'" << endl;
  }
}

void branch_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  expr->semant(T, BO, M, C);
}

void assign_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  Symbol tt = O.lookup(name);
  expr->semant(T, O, M, C);
  if (!T->is_subclass(expr->type, tt, C->get_name())) {
    T->semant_error(C->get_filename(), this) \
      << "'" << name << "' has type '" << tt << "', but " \
      << "'" << expr->type << "'" << " is not subclass of " \
      << "'" << tt << "'" << endl;
  }
  type = tt;
}

void static_dispatch_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  Class_ static_type = NULL;
  List<Entry> *signature = NULL, *formal_types = NULL, *actual_types = NULL,
    *tfl = NULL, *tal = NULL;
  expr->semant(T, O, M, C);

  if ((static_type = T->get(type_name)) == NULL) {
    T->semant_error(C->get_filename(), this) << "static type '" << type_name << "' not found" << endl;
    type = Object;
  } else if ((signature = static_type->getM().lookup(name)) == NULL) {
    T->semant_error(C->get_filename(), this) << "Class '" << type_name << "' not have method '" \
      << name << "'" << endl;
    type = Object;
  } else if (!T->is_subclass(expr->type, type_name, C->get_name())) {
    T->semant_error(C->get_filename(), this) \
      << "expression has type '" << expr->type << "', but " \
      << "'" << expr->type << "'" << " is not subclass of " \
      << "'" << type_name << "'" << endl;
    type = signature->hd();
  } else {
    type = signature->hd() == SELF_TYPE ? expr->type : signature->hd();
  }

  if (signature) {
    formal_types = signature->tl();
    tfl = formal_types;

    if (actual->len() != list_length(formal_types)) {
      if (actual->len() < list_length(formal_types))
        T->semant_error(C->get_filename(), this) << "too few arguments" << endl;
      else
        T->semant_error(C->get_filename(), this) << "too mant arguments" << endl;
    }
  }

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression arg = actual->nth(i);
    arg->semant(T, O, M, C);
    actual_types = new List<Entry>(arg->type, actual_types);
    if (list_length(actual_types) <= list_length(formal_types)) {
      tal = actual_types;
    }
  }

  while (tal != NULL) {
    if (!T->is_subclass(tal->hd(), tfl->hd(), C->get_name())) {
      T->semant_error(C->get_filename(), this) \
        << "argument has type '" << tal->hd() << "', but " \
        << "'" << tal->hd() << "'" << " is not subclass of " \
        << "'" << tfl->hd() << "'" << endl;
    }
    tal = tal->tl(); tfl = tfl->tl();
  }
}

void dispatch_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  Class_ expr_class = NULL;
  List<Entry> *signature = NULL, *formal_types = NULL, *actual_types = NULL,
    *tfl = NULL, *tal = NULL;
  expr->semant(T, O, M, C);
  expr_class = expr->type == SELF_TYPE ? C : T->get(expr->type);

  if ((signature = expr_class->getM().lookup(name)) == NULL) {
    T->semant_error(C->get_filename(), this) << "Class '" << expr_class->get_name() << "' not have method '" \
      << name << "'" << endl;
    type = Object;
  } else {
    type = signature->hd() == SELF_TYPE ? expr->type : signature->hd();
  }

  if (signature) {
    formal_types = signature->tl();
    tfl = formal_types;

    if (actual->len() != list_length(formal_types)) {
      if (actual->len() < list_length(formal_types))
        T->semant_error(C->get_filename(), this) << "too few arguments" << endl;
      else
        T->semant_error(C->get_filename(), this) << "too mant arguments" << endl;
    }
  }

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression arg = actual->nth(i);
    arg->semant(T, O, M, C);
    actual_types = new List<Entry>(arg->type, actual_types);
    if (list_length(actual_types) <= list_length(formal_types)) {
      tal = actual_types;
    }
  }

  while (tal != NULL) {
    if (!T->is_subclass(tal->hd(), tfl->hd(), C->get_name())) {
      T->semant_error(C->get_filename(), this) \
        << "argument has type '" << tal->hd() << "', but " \
        << "'" << tal->hd() << "'" << " is not subclass of " \
        << "'" << tfl->hd() << "'" << endl;
    }
    tal = tal->tl(); tfl = tfl->tl();
  }
}

void cond_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  pred->semant(T, O, M, C);
  if (pred->type != Bool) {
    T->semant_error(C->get_filename(), this) << "predicate must have Bool value" << endl;
  }
  then_exp->semant(T, O, M, C);
  else_exp->semant(T, O, M, C);
  type = T->join(then_exp->type, else_exp->type, C->get_name());
}

void loop_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  pred->semant(T, O, M, C);
  if (pred->type != Bool) {
    T->semant_error(C->get_filename(), this) << "predicate must have Bool value" << endl;
  }
  body->semant(T, O, M, C);
  type = Object;
}

void typcase_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  Symbol tn = NULL;
  expr->semant(T, O, M, C);
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case c = cases->nth(i);
    c->semant(T, O, M, C);
    tn = T->join(c->get_expr_type(), tn, C->get_name());
  }
  type = tn;
}

void block_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    expr->semant(T, O, M, C);
    type = expr->type;
  }
}

void let_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  Symbol tt = LO.lookup(identifier);
  init->semant(T, O, M, C);
  if (!T->is_subclass(init->type, tt, C->get_name())) {
    T->semant_error(C->get_filename(), this) << "let initialization has type '" << type_decl << "' but " \
      << "declare type is '" << type_decl << "'" << endl;
  }
  body->semant(T, LO, M, C);
  type = body->type;
}

void plus_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int) {
    T->semant_error(C->get_filename(), e1) << "expression is not Int" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int) {
    T->semant_error(C->get_filename(), e2) << "expression is not Int" << endl;
  }
  type = Int;
}

void sub_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int) {
    T->semant_error(C->get_filename(), e1) << "expression is not Int" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int) {
    T->semant_error(C->get_filename(), e2) << "expression is not Int" << endl;
  }
  type = Int;
}

void mul_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int) {
    T->semant_error(C->get_filename(), e1) << "expression is not Int" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int) {
    T->semant_error(C->get_filename(), e2) << "expression is not Int" << endl;
  }
  type = Int;
}

void divide_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int) {
    T->semant_error(C->get_filename(), e1) << "expression is not Int" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int) {
    T->semant_error(C->get_filename(), e2) << "expression is not Int" << endl;
  }
  type = Int;
}

void neg_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Bool) {
    T->semant_error(C->get_filename(), e1) << "expression is not Bool" << endl;
  }
  type = Bool;
}

void lt_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int) {
    T->semant_error(C->get_filename(), e1) << "expression is not Int" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int) {
    T->semant_error(C->get_filename(), e2) << "expression is not Int" << endl;
  }
  type = Bool;
}

void eq_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int && e1->type != Str && e1->type != Bool) {
    T->semant_error(C->get_filename(), e1) << "expression is not {Int, Str, Bool} type, '" \
      << e1->type << "' can't be compared" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int && e2->type != Str && e2->type != Bool) {
    T->semant_error(C->get_filename(), e2) << "expression is not {Int, Str, Bool} type, '" \
      << e2->type << "' can't be compared" << endl;
  }
  if (e1->type != e2->type) {
    T->semant_error(C->get_filename(), e2) << "expression 1 and expression 2 type must be same. " \
      <<"expression 1 has type '" << e1->type << "', " \
      <<"expression 2 has type '" << e2->type << "'" << endl;
  }
  type = Bool;
}

void leq_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  if (e1->type != Int) {
    T->semant_error(C->get_filename(), e1) << "expression is not Int" << endl;
  }
  e2->semant(T, O, M, C);
  if (e2->type != Int) {
    T->semant_error(C->get_filename(), e2) << "expression is not Int" << endl;
  }
  type = Bool;
}

void comp_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  type = Int;
}

void int_const_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  type = Int;
}

void bool_const_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  type = Bool;
}

void string_const_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  type = Str;
}

void new__class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  type = type_name;
}

void isvoid_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  e1->semant(T, O, M, C);
  type = Bool;
}

void no_expr_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  type = No_type;
}

void object_class::semant(ClassTableP T, ObjectEnv& O, MethodEnv& M, Class_ C) {
  type = O.lookup(name);
}