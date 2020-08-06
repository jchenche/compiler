class Main {
    main() : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };

    x: Int;
    y: NONEXISTING;
    x: B <- foo(3, "hi");
    i: Int;

    foo(param: String) : B {
        if true then new A else new B fi
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
    foo() : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };
};
