(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class StackCommand {

   command : String;

   init(c : String) : SELF_TYPE {
      {
         command <- c;
         self;
      }
   };

   execute(sm : StackMachine) : Object { self };

   command() : String { command };

};

class IntCommand inherits StackCommand {};

class AddCommand inherits StackCommand {

   execute(sm : StackMachine) : Object {
      (let a2i : A2I <- (new A2I),
         a1 : StackCommand <- sm.pop(),
         a2 : StackCommand <- sm.pop(),
         i1 : Int <- a2i.a2i(a1.command()),
         i2 : Int <- a2i.a2i(a2.command()),
         r : Int <- i1 + i2 in
            sm.push(sm.command(a2i.i2a(r)))
      )
   };

};

class SwapCommand inherits StackCommand {

   execute(sm : StackMachine) : Object {
      (let a1 : StackCommand <- sm.pop(),
         a2 : StackCommand <- sm.pop() in {
            sm.push(a1);
            sm.push(a2);
         }
      )
   };

};

class EvaluateCommand inherits StackCommand {

   execute(sm : StackMachine) : Object {
      if sm.stack().empty() then self else 
         (let c : StackCommand <- sm.pop() in
            case c of
               cc : AddCommand => cc.execute(sm);
               cc : SwapCommand => cc.execute(sm);
               cc : IntCommand => sm.push(cc);
            esac
         )
      fi
   };

};

class DisplayCommand inherits StackCommand {

   execute(sm : StackMachine) : Object {
      (let cur : List <- sm.stack().list() in
         while not isvoid cur loop {
            sm.display(cur.command().command());
            cur <- cur.pre();
         } pool
      )
   };

};

class StopCommand inherits StackCommand {

   execute(sm : StackMachine) : Object {
      sm.stop()
   };

};

class List {

   command : StackCommand;

   pre : List;

   initNoPre(c : StackCommand) : List {
      {
         command <- c;
         self;
      }
   };

   init(c : StackCommand, p: List) : List {
      {
         command <- c;
         pre <- p;
         self;
      }
   };

   command() : StackCommand {
      command
   };

   pre() : List {
      pre
   };

};

class Stack {

   list : List;

   lenght : Int;

   push(c : StackCommand) : Int {
      {
         if isvoid list then 
            list <- (new List).initNoPre(c)
         else 
            list <- (new List).init(c, list)
         fi;
         lenght <- lenght + 1;
      }
   };

   pop() : StackCommand {
      {
         lenght <- lenght - 1;
         (let cur : List <- list in {
            list <- cur.pre();
            cur.command();
         }
         );
      }
   };

   list() : List { list };

   lenght() : Int { lenght };

   empty() : Bool { lenght = 0 };

};

class StackMachine inherits IO {

   stack : Stack;

   stop : Bool;

   init() : StackMachine {
      {
         stack <- new Stack;
         stop <- false;
         self;
      }
   };

   command(c : String) : StackCommand {
      if c = "+" then (new AddCommand).init("+") else
      if c = "s" then (new SwapCommand).init("s") else
      if c = "e" then (new EvaluateCommand).init("e") else
      if c = "d" then (new DisplayCommand).init("d") else
      if c = "x" then (new StopCommand).init("x") else
         (new IntCommand).init(c)
         fi fi fi fi fi 
   };

   input(c : String) : Object {
      case command(c) of
         c : IntCommand => stack.push(c);
         c : AddCommand => stack.push(c);
         c : SwapCommand => stack.push(c);
         c : EvaluateCommand => c.execute(self);
         c : DisplayCommand => c.execute(self);
         c : StopCommand => c.execute(self);
      esac
   };

   stop() : Object { stop <- true };

   stopped() : Bool { stop };

   push(c : StackCommand) : Int { stack.push(c) };

   pop() : StackCommand { stack.pop() };

   stack() : Stack { stack };

   display(s : String) : Object {
      out_string(s.concat("\n"))
   };

};

class Main inherits IO {

   main() : Object {
      (let c : String,
         sm : StackMachine <- (new StackMachine).init() in 
         while not sm.stopped() loop {
            out_string(">");
            c <- in_string();
            sm.input(c);
         } pool
      )
   };

};
