-- Int, Bool and String can't be inherit
class A inherits Int {};
class B inherits Bool {};
class C inherits String {};

-- IO and Object can be inherit
class D inherits IO {};
class E inherits Object {};

-- Object, Int, Bool, String, IO can't redine
class Object {};
class Int {};
class Bool {};
class String {};
class IO {};

-- G not defined
Class F inherits G {};
-- duplicate F declare
Class F {};

-- inheris form a cycle
Class H inherits I {};
class I inherits J {};
class J inherits H {};

class Main {
	main(): D {
        (new D)
    };
};