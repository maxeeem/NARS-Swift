
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
    (*["evaluate", x] >>|=> (.operation("__eval__", [x]) >>|=> ("EVAL" && *["evaluate", x])))-*,
    ("EVAL" && *["evaluate", .operation("__print__", ["hello world"])])-!,
    .pause
)







