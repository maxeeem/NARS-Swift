
import PlaygroundSupport

PlaygroundPage.current.needsIndefiniteExecution = true

// Create Narsy

var verbose = true // toggle to see intermediate steps

var time: UInt32 = 0
let timeProviderMs: () -> UInt32 = { time += 1 ; return time }

let narsy = NARS(timeProviderMs: timeProviderMs) { out in
    if out.hasPrefix(".") && !verbose {
        return
    }
    print(out)
}

func eval(_ terms: [Term]) -> Term {
    guard terms.count == 1, 
          case .operation(let op, let ts) = terms.first else {
        return .NULL
    }
    return narsy.operations[op]?(ts) ?? .NULL
}

narsy.register("__eval__", eval(_:))

func loop(_ terms: [Term]) -> Term {
    guard terms.count == 2,
          let count = Int(terms[0].description),
          case .operation = terms[1] else {
    return .NULL
    }
    for i in 0..<count {
        print("EVAL", // TODO: should be better
            eval([terms[1]])
        )
    }
    return .NULL
}

narsy.register("__loop__", loop(_:))

func _print(_ terms: [Term]) -> Term {
    print("PRINT", terms)
    return .NULL
}

narsy.register("__print__", _print(_:))

var x = Term.var("x")

narsy.perform(
    // when asked to evaluate something, performing __eval__ operation, will lead to the goal
    ((*["evaluate", x] >>|=> .operation("__eval__", [x])) >>|=> ("EVAL" && *["evaluate", x]))-*,
    // evaluate a sequence containing another operation should execute sub-operation
    ("EVAL" && *["evaluate", .operation("__print__", ["hello", "world"])])-!,
    .cycle(10)
)

/*
narsy.perform(
    ("calculate" && *[a, "times", b])-*,
//    (*[a, "times", b] >>|=>
//            (.operation("__mul__", [a, b]) >>|=>
//                ("calculate" && *[a, "times", b]))
//        )-*,
//    ("bird" --> "animal")-*
    .pause,
    (.operation("__mul__", [a, b]) >>|=>
        ("calculate" && *[a, "times", b])
    )-*,
    .pause,
    ("calculate" && *["2", "times", "4"])-*,
//    ("calculate" && *["2", "times", "4"])-!,
    .cycle(5)
)

//narsy.perform(
//    ("bird" && "animal")-*,
//    ("robin" && "bird")-*
//)
//sleep(10)

print("\n\n==========\n\n")
print(narsy.memory)
print("\n\n==========\n\n")
if let bag = narsy.memory as? Bag<Concept> {
    if let concept = bag.get("calculate") {
        print(concept)
        let q: Question = ("?" >>|=> ("calculate" && *["2", "times", "4"]))-?
        var derived = [Judgement]()
        derived.append(contentsOf: concept.accept(
            ("calculate" && *["2", "times", "4"])-*, derive: true)
        )
        print(derived)
    }
}

sleep(5)
*/

/*
 - compounds should create term links to their constituent terms in conjunction, disjunction etc
 - if goal `? >>|=> goal-term does not have direct match
   pose the goal to components of goal-term
 - so `"calculate" && *["2", "times", "4"]` will be posed to `calculate` concept
   inside that concept, a match rule can be applied to beliefs
   if any of them match and can lead to realizing the goal, perform that operation
 
 somehow the premise is simple, just start at the top level and if there is no match, recurse down into components
 */

let a: Term = .var("a")
let b: Term = .var("b")
 

let one: Term = *[a, "times", b] >>|=>
    (.operation("__mul__", [a, b]) >>|=>
                    ("calculate" && *[a, "times", b])
    )
let two: Term = "?" >>|=> ("calculate" && *["2", "times", "4"])

//solve(one: one, two: two)

