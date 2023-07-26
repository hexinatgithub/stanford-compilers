class A {
	fun1() : Void {
		(new IO).out_string("A")
	};
};

class B inherits A {
	fun1() : Void {
		(new IO).out_string("B")
	};
};

class C {
	a : Int;
	b : Bool;
	c : A;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		c <- new B;
		b <- (11 = 22);
		b <- false = true;
		self;
           }
	};

	print(str : String) : Void {{
		(new B)@A.fun1();
		(new IO).out_string(str);
	}};

	clone() : C {
		(new SELF_TYPE).init(a, b)
	};

	branch() : H {
		if isvoid c then
			if "test1" = "test2" then
				let x : J <- new J in x
			else
				new I
			fi
		else
			new H
		fi
	};

	case_type(a : J) : G {
		case a of
		x1 : G => x1;
		x1 : H => x1;
		x1 : I => x1;
		x1 : J => x1;
		esac
	};
};

class G {};
class H inherits G {};
class I inherits H {};
class J inherits I {
	a : G <- self;

	fun1() : SELF_TYPE {{
		a <- self;
		self;
	}};
};

Class Main {
	main():C {{
	  (new C).print("start");
	  (new C).clone().print("start");
	  (new C).init(1,true);
	}};
};
