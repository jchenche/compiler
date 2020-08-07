class Silly {
    copy(): SELF_TYPE { self };
};

class Sally inherits Silly {};

class Main {
    main() : Object {
        let x: SELF_TYPE <- self, x: String <- "Hi" in x
    };

    x: String;
    y: NONEXISTING;
    x: B <- foo(3, "hi");
    z: Bool;

    foo() : B {
        if true then new A else new C fi
    };
};

class A inherits B {
    x: Int <- 33;
    foo() : SELF_TYPE {
        new SELF_TYPE
    };

};

class B inherits C {};

class C {
    y: Int <- 33;
    foo() : Object {
        let x: Int <- 3, x: String <- "Hi" in x
    };
};
