//
//  Experimental.swift
//  
//
//  Created by Maxim VT on 3/6/23.
//

import XCTest

@testable import NARS


class Experimental: XCTestCase {
    
    var output: [String] = []
    
    var narsy: NARS!
    
    var __ : NARS { narsy } // alias
    
    var verbose = true
    
    override func setUpWithError() throws {
        //        Sentence.defaultPause = 1000 // in milliseconds
        //        let timeProviderMs: () -> UInt32 = { DispatchWallTime.now().rawValue }
        var time: UInt32 = 0
        let timeProviderMs: () -> UInt32 = { time += 1 ; return time }
        
        narsy = NARS(timeProviderMs: timeProviderMs) { self.output.append($0) ; if self.verbose { print($0) } }
    }
    
    override func tearDownWithError() throws {
        narsy.reset()
        output.removeAll()
        //        usleep(1000000)
    }
    
    private func outputMustContain(_ expectation: String, timeout: TimeInterval = 1) {
        func condition() -> Bool { output.contains(where: { $0.contains(expectation) }) }
        let start = Date().timeIntervalSince1970
        while ((Date().timeIntervalSince1970 - start) < timeout) && !condition() {
            usleep(1000) // wait a bit
        }
        XCTAssertTrue(condition())
    }
    
    func testTheory() {
        narsy.perform(//("{Sandy}" --> "dog")-*,
            //                     ("{Sandy}" --> "dog")-*,
            ("{Sandy}" --> "dog")-*(0.1, 0.9),
            ("{Sandy}" --> "dog")-*(0.1, 0.9),
            ("{Sandy}" --> "dog")-*(0.1, 0.9),
            ("{Sandy}" --> "dog")-*,
            ("{Sandy}" --> "dog")-?
        )
        outputMustContain("💡 <{Sandy} -> dog>. %0.55;0.95%")
    }
    
    func testSample() {
        narsy.perform(
            ("{sky}" --> "[blue]")-*,
            ("{tom}" --> "cat")-*,
            ("{tom}" --> ç.e_("likes", .º, "{sky}"))-*,
            ("[blue]" --> ç.e_("likes", "cat", .º))-?,
            .cycle(200)
        )
        outputMustContain("💡 <[blue] -> (/ likes cat º)>.") // c should be 0.37%
    }
    
    func testLang() {
        narsy.perform((*["cat", "animal"] --> "is")-*)
        narsy.perform(("cat" --> "animal")-*)
        narsy.perform((*["dog", "animal"] --> "is")-*)
        narsy.perform(("dog" --> "animal")-?)
        
        outputMustContain("<dog -> animal>.")
        
        //        nars.perform(("dog" --> "animal")-*(1.0, 0.9, 0))
        //        nars.perform(("dog" --> "animal")-?)
    }
    /*
    func testPattern() {
        narsy.perform((*[*["1","0","0","0","0","0","0","0","0","0"], "left"] --> "is")-*)
        narsy.perform((*["1","0","0","0","0","0","0","0","0","0"] --> "left")-*)
        narsy.perform((*[*["1","1","0","0","0","0","0","0","0","0"], "left"] --> "is")-*)

//        narsy.perform(.cycle(500))
        
//        narsy.perform((*[*["0","0","0","0","0","0","0","0","0","1"], "right"] --> "is")-*)
//        narsy.perform((*["0","0","0","0","0","0","0","0","0","1"] --> "right")-*)
//        narsy.perform((*[*["0","0","0","0","0","0","1","0","1","1"], "right"] --> "is")-*)
                
        narsy.perform((*["1","1","1","0","0","0","0","0","0","0"] --> "left")-?)
//        narsy.perform((*["0","0","0","0","0","0","1","0","1","0"] --> "right")-?)
        narsy.perform(.cycle(200))
        //        nars.perform((*["0","0","0","0","0","0","0","1","1","1"] --> "right")-?)
        //        nars.perform(.cycle(200))
        //        nars.perform((*["0","0","0","0","0","0","0","1","1","1"] --> "right")-?)
        
        outputMustContain("💡 <(⨯ 1 1 1 0 0 0 0 0 0 0) -> left>.")
//        outputMustContain("💡 <(⨯ 0 0 0 0 0 0 1 0 1 0) -> right>.")
    }
    */
    func testCompare() {
        //        let compare = Term.operation("compare", ["$a", "$b"])
        let compare = Term.operation("compare", [])
        let less = Term.operation("compare", ["<"])
        _ = *["{x}", "{1}"] --> compare
        _ = *["{y}", "{2}"] --> compare
        _ = *["{1}", "{2}"] --> less
        // =>
        _ = *["{x}", "{y}"] --> less
    }
    
    func testSymbolic() {
//        let relation = *["C", "subset"] --> "represent"
//        let image = "C" --> ç.e_("represent", .º, "subset")

        let knowledge = *[.var("x"), "C", .var("y")] --> ç.e_("represent", .º, (*[.var("x"), .var("y")] --> "subset"))
        
        narsy.perform(
//            image-*,
            knowledge-*,
            .cycle(20),
            (*["dog", "C", "animal"] --> ç.e_("represent", .º, "?"))-?,
            .cycle(200)
        )
        outputMustContain("💡 <((dog ⨯ C) ⨯ animal) -> (/ represent º (dog ⨯ animal) -> subset)>.")
    }
    
    func testEsperanto2() {
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
                self.narsy.perform(question-?)
            }
            return .NULL
        }
/*
        __.perform((*[.var("x"), "dormas"] --> rep(.var("x") --> "[dormas]")))
        
        __.perform(*["Sandy", "dormas"])

        //        __.perform("kiu" --> rep("?"))
        __.perform((("kiu" --> .var("x")) --> rep(.operation("ask", ["?kiu" --> .var("x")]))))
                

        __.perform(((*["kiu", "dormas"]) --> rep("?"))-?)
        
//        __.perform(*["kiu", "estas", .var("x")] --> rep(.operation("ask", [.var("x") --> "?"])))
//        __.perform(*["kiu", .var("x"), .var("y")] --> rep(.operation("ask", [.variable(.query("kiu")), .var("x"), .var("y")])))
//        __.perform(.cycle(20))
//        __.perform((*["kiu", "estas", "viro"] --> rep("?"))-?)
//        __.perform(.cycle(40))
//        __.perform((*["?kiu", "estas", "viro"] --> rep("?"))-?)
//        __.perform(.cycle(80))
//        __.perform(("?kiu" --> "[dormas]")-?)
        
        __.perform(.cycle(20))
        
        outputMustContain("💡 <Sandy -> [dormas]>.")
        
        __.perform((("Sandy" --> "[dormas]") --> rep2("?"))-?)

        outputMustContain("💡 <(Sandy -> [dormas]) -> (/ represent (Sandy ⨯ dormas) º)>.")
*/
        // `as` indicated the present in verbs
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
        
        __.perform((*["Adamo", *["log", "as"]]))

        __.perform(.cycle(100))

        outputMustContain("<Sandy -> (|=> [dorm])>.")
        outputMustContain("<Adamo -> (|=> [log])>.")

    }
    
    func testLogicMatch() {
        let res = Term.logic_match(t1: *["dog", "C", "animal"], t2: *["$x", "C", "$y"])
        XCTAssertTrue(res)
    }
    
    func testLookup() {
        narsy.perform(
            (("dog" --> "$x") => ("$x" --> "[live]"))-*,
            ("dog" --> "animal")-*,
            .cycle(60)
        )
        outputMustContain("<animal -> [live]>.")
    }
    
    func testSimpleQuestion() {
        narsy.perform(
            ("dog" --> "animal")-*,
            ("dog" --> "?")-?
        )
        outputMustContain("💡 <dog -> animal>.")
    }
    
    
    /*
     func testik() {
     let relation = ç.x_("water", "salt") --> "dissolve"
     let knowledge = "rain" --> "water"
     
     //        nars.perform(
     //            relation-*,
     //            knowledge-*,
     //            .cycle
     //        )
     
     let image = "water" --> ç.e_("dissolve", "º", "salt")
     nars.perform(
     image-*,
     knowledge-*,
     .cycle
     )
     }
     */
    // KK <P |=> S>. %1.00;0.45%.ind ["S+(1.0, 0.9, 16781238065391810616)", "P+(1.0, 0.9, 16781238065392859616)"]
    // KK <P |=> S>. %1.00;0.45%.ind ["S+(1.0, 0.9, 16781238065391810616)", "P+(1.0, 0.9, 16781238065378744616)"]
    
    // KK <S |=> P>. %1.00;0.45%.ind ["P+(1.0, 0.9, 16781238065392859616)", "S+(1.0, 0.9, 16781238065391810616)"]
    // KK <S |=> P>. %1.00;0.45%.ind ["P+(1.0, 0.9, 16781238065378744616)", "S+(1.0, 0.9, 16781238065391810616)"]
    /*
     func test4() {
     //        nars.cycle = true
     nars.perform(
     ||("_P_")-*,
     ("_P_")-*(1.0,0.9,0),
     //            .pause,
     ||("_S_")-*
     //            .cycle
     )
     //        print(nars.memory)
     nars.perform(
     ||("_P_")-*,
     //            .pause,
     ||("_S_")-*
     //            .cycle
     )
     nars.perform(
     ||("_P_")-*,//(1.0,0.9,0),
     //            .pause,
     ||("_S_")-*,
     //              , .pause
     .cycle
     )
     //        nars.cycle = false
     print(nars.memory)
     }
     */
    
    func testEsperanto() { // TODO: how to add diacritics etc?
        
        __.register("nth") { terms in
            if terms.count > 1,
               let pos = terms.first?.description, let i = Int(pos),
               terms[1].terms.count > i-1 {
                return terms[1].terms[i-1]
            }
            return .operation("nth", terms)
            
        }
        __.register("4th") { terms in
            self.__.dynamicallyCall(withArguments: ["nth", "4"] + terms)
        }
        
        __.register("days-of-week") { terms in
            if let locale = terms.first {
                switch locale.description {
                case "en":
                    return *["Monday", "Tuesday", "Wednesday", "Thursday"]
                case "fr":
                    return *["Lundi", "Mardi", "Mercredi", "Jeudi"]
                case "eo":
                    break // return below
                default:
                    return .operation("days-of-week", [locale])
                }
            }
            return *["M", "T", "W", "Jaud"]
        }
        
        //        __.register("translate") { terms in
        //            if terms.count > 1 {
        //                let term = terms[0]
        //                let locale = terms[1]
        //                return term --> self.__("get-fourth", self.__("days-of-week", locale))
        //            }
        //            return .NULL
        //        }
        
        __.register("a") { terms in
            if let word = terms.first {
                return .property(word)
            }
            return .operation("a", terms)
        }
        
        __.perform(
            ("a" --> __("a"))-*,
            
            (*["$x", "a"] --> __("a", "$x"))-*,
            
            ("kvar" --> "4")-*,
            
            (*["kvar", "a"])-*, // should derive kvar,a --> [kvar] -or- kvar,a --> __(a, kvar)
            
            ("[4]" --> __("nth", "4"))-*,
            
            ("jaud" --> __("4th", __("days-of-week")))-*,
            
            
            
            //            ("hom" --> "")
            
            
            ("Thursday" --> "jaud")-*,
            
            //            ("jaud" --> "t001")-*,
            //            ("t001" --> __("get-fourth", __("days-of-week")))-*,
            
            //            ("t001" --> *["t001", "$lang"])-*,
            //            (*["t001", "$lang"] --> __("get-fourth", __("days-of-week", "$lang")))-*,
            
            //            (*["t001", "$lang"] --> __("get-fourth", __("days-of-week", "$lang")))-*,
            
            //            (*["t001", "eo"] --> "Jaud")-*,
            
            //            (__("translate", "t001", "en"))-!,
            //            .cycle(10),
            //            (__("translate", "t001", "fr"))-*,
                .cycle(100)
        )
        
        //        __.perform(
        //            ("jaud" --> *["t001", "eo"])-*,
        ////            ("t001" --> __("get-fourth", __("days-of-week", "en")))-*,
        ////            ("t001" --> __("get-fourth", __("days-of-week", "fr")))-*,
        //            (*["t01", "$lang"] --> __("get-fourth", __("days-of-week", "$lang")))-*,
        ////            (*["t001", "en"] --> "?")-?,
        //            .cycle(100)
        //        )
        
        //        outputMustContain("⏱ <jaud -> ^get-fourth ^days-of-week >. %1.00;0.81%")
        outputMustContain("<(kvar ⨯ a) -> ^a kvar>.")
        outputMustContain("⏱ <Thursday -> ^4th ^days-of-week >. %1.00;0.81%")
        //        outputMustContain("⏱ <jaud -> Thursday>.")// %1.00;0.81%")
        //        outputMustContain("⏱ <jaud -> Jeudi>.")// %1.00;0.81%")
    }
    
    
    func testOp() {
        narsy.perform(
            ("G")-!,
            ((("ball" --> "[left]") >>|=> .operation("move", [.SELF, "[left]"])) >>|=> "G")-*,
            ||("ball" --> "[left]")-*,
            .cycle(100)
        )
        outputMustContain("🤖 ^move SELF [left]")
//        print(nars.memory)
    }

    func testOp2() {
        narsy.perform(
            ("G")-!,
            ((("ball" --> "[left]") >>|=> (.operation("move", [.SELF, "[left]"]))) >>|=> ("ball" --> "[center]"))-*,
            (("ball" --> "[center]") >>|=> "G")-*,
            ||("ball" --> "[left]")-*,
            .cycle(200)
        )
        outputMustContain("🤖 ^move SELF [left]")
//        print(narsy.memory)
    }
    /*
    func testMove() {
        __.register("move") { ts in
            .NULL
        }
        
        narsy.perform(
            ("G")-!,
//            ||(("ball" --> "[left]"))-*,
//            ||("ball" --> "[left]")-*,
//            ||("ball" --> "[left]")-*,
//            ||("ball" --> "[center]")-*,
//            ||("G")-*,
////            ("G")-!,
//            .cycle(100),
            ||("ball" --> "[left]")-*,
            ||(.operation("move", [.SELF, "[left]"]))-*,
//              .cycle(10),
            ||("ball" --> "[center]")-*,
//              .cycle(10),
            ||("G")-*,
            ("G")-!,
            .cycle(10),
            ||("ball" --> "[left]")-*,
//            .cycle(100),
            ||("ball" --> "[center]")-*,
//            .cycle(10),
            ||("G")-*,
            ("G")-!,
            .cycle(20),
            ||("ball" --> "[center]")-*,
//            ||(("ball" --> "[left]") >>|=> (.operation("move", ["left"])))-*,
//               ||("G")-*,
//              .cycle(10),
            ||("ball" --> "[right]")-*,
            ||("ball" --> "[right]")-*,
            ||("ball" --> "[right]")-*,
            .cycle(10),
            ||("ball" --> "[right]")-*,
            ||("ball" --> "[right]")-*,
            .cycle(40)
        )
        
        outputMustContain("🤖 ^move SELF [left]")
        outputMustContain("🤖 ^move SELF [right]")
    }
    */
    
    func testMultiply() {
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
         
        /* result */
        /// leads to achieving the goal `multiply a times b`
        /// && is a shortcut for conjunction Term.compound(.c, [a, b, c])
        let res: Term = ("multiply" && *[a, "times", b])

        /// we define the rule in reverse
        /// (condition, operation) |- result
        /// -or-
        /// condition >> (operation >> result)

        let multiplication_rule: Term = ((mul >>|=> op) >>|=> res)

        /// narsy uses sentences as input
        /// -* is a judgement
        /// -? is a question
        /// -! is a goal
        narsy.perform( multiplication_rule-* )

        /// goals are something narsy should strive to achieve
        let goal_two_times_four: Sentence = ("multiply" && *["2", "times", "4"])-!

        narsy.perform(goal_two_times_four)
        
        narsy.perform(.cycle(10))

        narsy.reset()

        /*
         ALTERNATIVE SYNTAX
         */

        print("\nALTERNATIVE SOLUTION\n")
         
        /// you can skip the intermediate variables
        /// and just express what you need directly

        narsy.perform(
            // input the rule
            ((*[a, "times", b] >>|=>
                .operation("__mul__", [a, b])) >>|=>
                    ("multiply" && *[a, "times", b])
            )-*,
            // ask it to do `multiply`
            ("multiply" && *["2", "times", "4"])-!,
            // ask it to multiply again
            ("multiply" && *["5", "times", "8"])-!,
            // ask it to multiply again
            ("multiply" && *["2", "times", "4"])-!,
            .cycle
        )

    }
    
    func testEval() {
        
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
            
            ("EVAL" && *["evaluate", .operation("__print__", ["hello", "world"])])-!,
            .cycle(10)
        )

        // TODO: what is the expected behavior? need additional triggers to execure operation?
        let y: Term = *["evaluate", .operation("__print__", ["hello", "world"])]
        print(y)
    }
}
