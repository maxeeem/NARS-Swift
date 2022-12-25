import PlaygroundSupport

PlaygroundPage.current.needsIndefiniteExecution = true

// Create Narsy

var verbose = false // toggle to see intermediate steps

let narsy = NARS(cycle: false) { out in
    if out.hasPrefix("•  ⏱") && !verbose {
        return
    }
    print(out)
}

// multiplication is a sequence of the form `a times b`
// sequences are represented as compounds with ⨯ connector
// *[a, b, c] is a shortcut for Term.compound(.x, [a, b, c])
// Note: * and ⨯ are symbols of compound product, not multiplication

let a: Term = .var("a")
let b: Term = .var("b")

/* condition */
/// given a statement `a times b` 
let mul: Term = *[a, "times", b] 

/* operation */ 
/// executing an operation __mul__ with `a` and `b`
let op: Term = .operation("__mul__", [a, b])

// register __mul__ operation and map it to swift's multiply
//narsy.register("__mul__", *)
// TODO: make Term IntegerRepresentable

// full syntax for more complex operations
func multiply(_ terms: [Term]) -> Term {
    guard terms.count == 2,
          let a = Int(terms[0].description),
          let b = Int(terms[1].description) else {
        return .NULL
    }
    let result = a * b
    return .symbol("\(result)")
}
narsy.register("__mul__", multiply(_:))
 
/* result */
/// leads to achieving the goal `multiply a times b`
/// && is a shortcut for conjunction Term.compound(.c, [a, b, c])
let res: Term = ("multiply" && *[a, "times", b])

/// we define the rule in reverse
/// (condition, operation) |- result
/// -or-
/// condition >> (operation >> result)

let multiplication_rule: Term = (mul >>|=> (op >>|=> res))

/// narsy uses sentences as input 
/// -* is a judgement
/// -? is a question
/// -! is a goal
narsy.perform( multiplication_rule-* )
    
let two_times_four: Term = *["2", "times", "4"]

/// events are represented by a || prefix 
//let input_two_times_four: Sentence = ||(two_times_four)-*

//narsy.perform(input_two_times_four)

/// goals are something narsy should strive to achieve
let goal_two_times_four: Sentence = ("multiply" && *["2", "times", "4"])-!

narsy.perform(goal_two_times_four)
/// cycle or pause at the end is needed in playgrounds
/// to allow the system to process before returning
/// when working from the command line or in a library
/// and if cycling is enabled, it can be omitted
narsy.perform(.pause)



/*
 ALTERNATIVE SYNTAX
 */

narsy.reset()
print("\nALTERNATIVE SOLUTION\n")

/// you can skip the intermediate variables
/// and just express what you need directly

narsy.perform(
    // input the rule
    multiplication_rule-*,
    // give narsy some input
//    ||(*["2", "times", "4"])-*,
    // ask it to do `multiply`
    ("multiply" && *["2", "times", "4"])-!,
    // give it different input
//    ||(*["5", "times", "8"])-*,
    // ask it to multiply again
    ("multiply" && *["5", "times", "8"])-!,
    // ask it to multiply again
    ("multiply" && *["2", "times", "4"])-!
)



//print(narsy.memory)

