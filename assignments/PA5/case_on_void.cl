-- Test 4 case
--   test case on void

class Main inherits IO {
    main(): Object {{
        let x: Object in {
            case x of 
                a : Int => { out_string("test case on void wrong"); };
                a : String => { out_string("test case on void wrong"); };
            esac;
        };
    }};
};