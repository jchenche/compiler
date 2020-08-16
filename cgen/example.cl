class Main {
    main():Int {
        0
    };
};

class A {
    a: Int <- 2;

    f(x1: Int, x2: String): Int { x1 + x1 };
    g(): String { "hello world" };
};

class B inherits A {
    b: String <- "hi";
};

class C inherits B {
    c1: Int;
    c2: String;

    f(y1: Int, y2: String): Int { y1 };
    h(): String { "from C" };
};

class D inherits A {
    d: Bool <- true;
};
