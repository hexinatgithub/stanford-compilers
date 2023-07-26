class A {
    a : Int <- 100;
};

class B inherits A {};

class C {};

class D inherits C {
    a : A;
    b : Int;
    c : A;

    fun1(a1 : Int, a2 : String) : Int {
        {
            -- SELF_TYPEc is not subclass of A
            c <- new SELF_TYPE;
            -- Class A not subclass of Int
            a <- new B;
        }
    };
};

class E {
    -- return Int not String
    fun1(a1 : B, a2 : B) : String {{
        let o : D <- new D in {
            o.fun2(1, "error type");
            -- arguments wrong type, return Object
            o.fun1(new A, new A);
        };
    }};

    fun2(a1 : String, a2 : Int) : SELF_TYPE {
        -- too many arguments
        self.fun2("test", 1, 2, 3)
    };

    fun3() : String {
        -- too few arguments
        -- return SELF_TYPEe, not subclass of String
        self.fun2(1)
    };
};

class F inherits E {
    fun4() : Int {{
        -- static dispatch not found method
        self@E.funX();
        -- isvoid return Bool not Int
        self@E.fun2("error type", isvoid 1);
        -- not return Bool not Int
        self@E.fun2("error type", not true);
        -- not return Bool not Int
        self@E.fun2(1 < 2, 2 <= 4);
        -- neg return Int not String
        self@E.fun2(1, ~1024);
        -- add return Int not String
        self@E.fun2(1, 1 + 2);
        -- sub return Int not String
        self@E.fun2(1, 2 - 1);
        -- mul return Int not String
        self@E.fun2(1, 2 * 4);
        -- divide return Int not String
        self@E.fun2(1, 4 / 2);
        -- compare must be primitive type
        self@E.fun2(1 = 1, "error type" = "error type");
        self@E.fun2((isvoid new A) = false, new A = new SELF_TYPE);
        -- static dispatch wrong arguments type
        self@E.fun2(1, "error type");
    }};

    fun5() : String {
        -- too few arguments
        -- return SELF_TYPEe, not subclass of String
        self@E.fun2(1)
    };

    fun6() : String {{
        -- predicate must be Bool value
        if 1 then new E else new F fi;
        -- Class E is not subclass of String
        if true then new E else new F fi;
    }};

    fun7() : String {{
        let x : SELF_TYPE <- new E in x;
    }};
};

class G {};
class H inherits G {};
class I inherits H {};
class J inherits I {};

class K {
    a : G;

    fun1() : H {
        -- least type of case branch is G, not H
        case a of
        x1 : J => new J;
        x1 : I => new I;
        x1 : H => new H;
        x1 : G => new G;
        esac
    };

    fun2() : A {
        -- while expression type always Object
        while true loop new A pool
    };

};

class Main {
	main(): Void {
        true
    };
};