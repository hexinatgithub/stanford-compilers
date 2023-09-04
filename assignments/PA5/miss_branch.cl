-- Test 5 case
--   test miss branch

class A {};

class Main inherits IO {
    main(): Object {{
        let x: Object <- new A in {
            case x of 
                a : Int => { out_string("test miss branch wrong"); };
                a : String => { out_string("test miss branch wrong"); };
            esac;
        };
    }};
};