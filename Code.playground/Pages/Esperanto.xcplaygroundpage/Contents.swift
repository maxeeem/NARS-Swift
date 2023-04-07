import PlaygroundSupport

PlaygroundPage.current.needsIndefiniteExecution = true

var verbose = true
var history = [String]()

var time: UInt32 = 0
let timeProviderMs: () -> UInt32 = { time += 1 ; return time }

let narsy = NARS(timeProviderMs: timeProviderMs) { s in
    if !verbose && (s.hasPrefix(".") || s.contains("💤")) { return }
    history.append(s); print(s)
}

var __ : NARS { narsy }

func rep(_ t: Term) -> Term {
   ç.e_("represent", .º, t)
}
func rep2(_ t: Term) -> Term {
   ç.e_("represent", t, .º)
}
//        __.perform(*[.var("x"), "kaj", .var("y")] --> rep(.var("x") & .var("y")))
//        __.perform(*[.var("x"), "kaj", .var("y")] --> rep(.var("x") && .var("y")))
//        __.perform(*[.var("x"), "estas", .var("y")] --> rep(.var("x") --> .var("y")))
//
//        __.perform((*["Adamo", "kaj", "Sofia"] --> rep("?"))-?)
//        __.perform((*["Adamo", "estas", "viro"] --> rep("?"))-?)
//        __.perform(.cycle(20))
       
//        __.register("esperanto") { terms in
//            // recursively convert `represent` terms
//
//            return .NULL
//        }
       
__.register("ask") { ts in
   if let question = ts.first {
       narsy.perform(question-?)
   }
   return .NULL
}

// `as` indicates the present in verbs
// TODO:
// needs to be handled at the level of judgement to carry tense information
// alrernatively can bring in tense into the Term, need to investigate further
let `as`: Term = *["$x", "as"] --> rep(||("[$x]"))
let `os`: Term = *["$x", "os"] --> rep(>>("[$x]"))
let `is`: Term = *["$x", "is"] --> rep(<<("[$x]"))

__.perform((`as`)-*)
__.perform((`os`)-*)
__.perform((`is`)-*)

//        __.perform((*[.var("x"), *[.var("y"), "as"]] --> rep(.var("x") --> .property(.var("y")))))

__.perform((*["$x", "$y"] --> rep("$x" --> "$y")))

__.perform((*["Sandy", *["dorm", "as"]]))

__.perform(.cycle(40))

__.perform((*["Adamo", *["log", "as"]]))

__.perform(.cycle(40))
