
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

-- Test 1 case
--    test arithmetic expressions
--    test defualt primitive values 
--    test attribute initialization
--    test static dispatch
--    test dispatch method
--    test inheritance
--    test cond
--    test loop
--    test block
--    test let
--    test neg
--    test lt, eq, leq
--    test comp
--    test new
--    test isvoid
--    test object

-- Test 2 case
--    test partition on type case
--      primitive type
--      class instance
--      void
--      self

-- Test 3 case
--    test primitive default value

class A {
  id1 : Int <- (2 * 3) + (3 - 2) + (4 / 2); -- 9
  id2 : String;
  id3 : Bool;

  get_id1() : Int { id1 };
  get_id2() : String { id2 };
  get_id3() : Bool { id3 };
};

class B inherits A {
  id4 : String <- "id4";
  get_id4() : String { id4 };

  count1 : Int <- 9;
  test_loop() : String {{
    while count1 < 9 loop 
      count1 <- count1 + 1
    pool;
    if count1 = 9 then "loop is right" else "loop is wrong" fi;
  }};

  test_lt() : String { if 10 < 11 then "lt is right" else "lt is wrong" fi };

  test_eq() : String { if 10 = 11 then "eq is wrong" else "eq is right" fi };

  test_neg() : String {
    let a : Int <- ~1 in if a < 0 then "neg is right" else "neg is wrong" fi
  };

  test_comp() : String { if not 10 < 11 then "comp is wrong" else "comp is right" fi };

  id5 : A;
  test_isvoid() : String { if isvoid id5 then "isvoid is right" else "isvoid is wrong" fi };
};

class C {
  id1 : Int;
  id2 : String;
  id3 : Bool;

  get_id1() : Int { id1 };
  get_id2() : String { id2 };
  get_id3() : Bool { id3 };
};

-- Test class methods

class Main inherits IO {
  main(): Object {{
    -- test 1 cases
    let a : A <- new B in {
      let t1 : String <- a@Object.type_name(), t2 : Int <- a.get_id1() in {
        if t1 = "B" then {
          out_string("static dispatch is right"); out_string("\n");
          if t2 = 9 then {
            out_string("int default initializer is right"); out_string("\n");
          } else {
            out_string("int default initializer is wrong"); out_string("\n");
          }
          fi;
        } else {
          out_string("static dispatch is wrong"); out_string("\n");
        }
        fi;
      };

      let t1 : String <- a.get_id2(), t2 : Bool <- a.get_id3() in {
        if t1 = "" then {
          out_string("string default initializer is right"); out_string("\n");
          if t2 = false then {
            out_string("bool default initializer is right"); out_string("\n");
          } else {
            out_string("bool default initializer is wrong"); out_string("\n");
          }
          fi;
        } else {
          out_string("string default is wrong"); out_string("\n");
        }
        fi;
      };
    };

    let a : B <- new B in {
      let t1 : Int <- a.get_id1(), t2 : String <- a.get_id4() in {
        if t1 = 9 then {
          if t2 = "id4" then {
            out_string("inheritance is right"); out_string("\n");
          } else {
            out_string("inheritance is wrong"); out_string("\n");
          }
          fi;
        } else {
          out_string("inheritance is wrong"); out_string("\n");
        }
        fi;
      };

      out_string(a.test_loop()); out_string("\n");
      out_string(a.test_lt()); out_string("\n");
      out_string(a.test_neg()); out_string("\n");
      out_string(a.test_eq()); out_string("\n");
      out_string(a.test_comp()); out_string("\n");
      out_string(a.test_isvoid()); out_string("\n");
    };

    -- test 2 cases
    let a : Int <- 1 in {
      case a of
        a : Int => { out_string("case on primitive type is right"); out_string("\n"); };
        a : String => { out_string("case on primitive type is wrong"); out_string("\n"); };
        a : Bool => { out_string("case on primitive type is wrong"); out_string("\n"); };
      esac;
    };

    let a : A <- new B in {
      case a of
        a : Int => { out_string("case on class instance is wrong"); out_string("\n"); };
        a : A => { out_string("case on class instance is wrong"); out_string("\n"); };
        a : B => { out_string("case on class instance is right"); out_string("\n"); };
      esac;
    };

    case self of
      a : IO => { out_string("case on self is wrong"); out_string("\n"); };
      a : Main => { out_string("case on self is right"); out_string("\n"); };
    esac;

    -- test 3 cases
    let a : C <- new C in {
      if a.get_id1() = 0 then {
        out_string("class int default value is right"); out_string("\n");
      } else {
        out_string("class int default value is wrong"); out_string("\n");
      }
      fi;

      if a.get_id2() = "" then {
        out_string("class string default value is right"); out_string("\n");
      } else {
        out_string("class string default value is wrong"); out_string("\n");
      }
      fi;

      if a.get_id3() = false then {
        out_string("class bool default value is right"); out_string("\n");
      } else {
        out_string("class bool default value is wrong"); out_string("\n");
      }
      fi;
    };

    let a : Int, b : String, c : Bool in {
      if a = 0 then {
        out_string("let expression int default value is right"); out_string("\n");
      } else {
        out_string("let expression int default value is wrong"); out_string("\n");
      }
      fi;

      if b = "" then {
        out_string("let expression string default value is right"); out_string("\n");
      } else {
        out_string("let expression string default value is wrong"); out_string("\n");
      }
      fi;

      if c = false then {
        out_string("let expression bool default value is right"); out_string("\n");
      } else {
        out_string("let expression bool default value is wrong"); out_string("\n");
      }
      fi;
    };

    self;
  }};
};

