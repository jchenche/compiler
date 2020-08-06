class Main {
    main() : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };

    x: Int;
    y: NONEXISTING;
    x: Int <- foo(3, "hi");

    foo(param: Int, param: String) : Int {
        param
    };

    main() : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };
};

class A inherits B {
    x: Int <- 33;
    foo() : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };

};

class B inherits C {};

class C {
    y: Int <- 33;
    foo(x: Int) : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };
};
