class A {
    -- Type not exist
    a : Nope;
    -- duplicate declare
    a : Int;

    -- formal arguments type can't be SELF_TYPE
    -- formal arguments must distinct
    -- type must exist
    fun1(a1 : SELF_TYPE, a1 : Int, a2 : String, a3 : Nope) : Nope {
        true
    };
};

-- redefine method signature must be same
class B {
    a : Int;
    b : String;

    fun2(a1 : Int, a2 : String) : Void {
        true
    };
};

class C inherits B {
    a : String;
    b : Int;

    fun2(a1 : String, a2 : String) : Void {
        true
    };
};

class Main {
	main(): Void { false };
};