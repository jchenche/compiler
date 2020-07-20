(* top comments *)
class List {
   isNil() : Bool { true };
   head()  : Command { { abort(); new Command; } };
   tail()  : List { { abort(); self; } };
   cons(i : Command) : List { (new Cons).init(i, self) };
};

class Cons inherits List {
    car : Command;   -- The element in this list cell
    cdr : List;  -- The rest of the list
    isNil() : Bool { false };
    head()  : Command { car };
    tail()  : List { cdr };
    init(i : Command, rest : List) : List {
        {
            car <- i;
            cdr <- rest;
            self;
        }
    };
};

class Command inherits A2I {
    eval(stack: List): List { { abort(); new List; } };
    toString(): String { {abort(); ""; } };
};

class SwapCommand inherits Command {
    eval(stack: List): List {
        let stack: List <- stack.tail(),
            first: Command <- stack.head(),
            second: Command <- stack.tail().head()
        in stack.tail().tail().cons(first).cons(second)
    };
    toString(): String { "s" };
};

class AddCommand inherits Command {
    eval(stack: List): List {
        let stack: List <- stack.tail(),
            first: IntCommand <-
                case stack.head() of x: IntCommand => x; esac,
            second: IntCommand <-
                case stack.tail().head() of x: IntCommand => x; esac,
            sum: Int <- first.getNum() + second.getNum()
        in stack.tail().tail().cons(new IntCommand.init(i2a(sum)))
    };
    toString(): String { "+" };
};

Class IntCommand inherits Command {
    num: Int;
    init(input: String) : Command {
        {
            num <- a2i(input);
            self;
        }
    };
    eval(stack: List): List { stack };
    getNum(): Int { num };
    toString(): String { i2a(num) };
};

class Main inherits IO {
    stack: List <- new List;
    exec: Bool <- true;

    print_list(l: List) : Object {
        if l.isNil() then out_string("")
        else {
            out_string(l.head().toString());
            out_string("\n");
            print_list(l.tail());
        }
        fi
    };

    eval() : Object {
        if stack.isNil() then stack
        else stack <- stack.head().eval(stack)
        fi
    };

    main() : Object {
        while exec loop
        {
            out_string(">");
            let input: String <- in_string()
            in
                if      input = "x" then exec <- false
                else if input = "d" then print_list(stack)
                else if input = "e" then eval()
                else if input = "s" then stack <- stack.cons(new SwapCommand)
                else if input = "+" then stack <- stack.cons(new AddCommand)
                else stack <- stack.cons(new IntCommand.init(input))
                fi fi fi fi fi;
        }
        pool
    };
};
-- Just some comments
-- With (* and *)
(* while this and this *)
(*
 * multiline
 *)
(*  )***** nested (* if *) else *)
