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
            ("[blue]" --> ç.e_("likes", "cat", .º))-?
//            .cycle(1000)
        )
        for _ in 0..<1000 {
            narsy.perform(.cycle)
        }
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
            .cycle(2000)
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
//        • <G>! %1.00;0.90%.
//        • <<<ball -> [left]> /=> (^move SELF [left])> /=> <ball -> [center]>>. %1.00;0.90%.
//        • <<ball -> [center]> /=> G>. %1.00;0.90%.
//        • <ball -> [left]>. %1.00;0.90%.
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
    
    func testTurn() {
        let inputA: Term = *["•", .NULL]
        let inputB: Term = *[.NULL, "•"]
        
//        let inputs = [inputA, inputB]
        
//        let random = inputs.randomElement()!
//        let choice: Term = (random == inputA) ? "A" : "B"
        let rule1 = (inputA >>|=> .operation("take", ["A"])) >>|=> "G"
        let rule2 = (inputB >>|=> .operation("take", ["B"])) >>|=> "G"

        let game = ("$x" >>|=> .operation("take", [.operation("index", ["$x"])])) >>|=> "G"
        
        narsy.register("take") { args in
            print(args[0])
            return .NULL
        }
        narsy.register("index") { args in
            
            return "idx"
        }
//        print(game, " | ", inputA)
        narsy.perform("G"-!)
//        narsy.perform(rule1-*, rule2-*)
        narsy.perform(game)
        narsy.perform(inputB)
        narsy.perform(.cycle(100))
    }
    
    func testConditional1() {
        let rule = Rules.deduction.conditional[2]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B", "C", "D"] => "P")-*
        let j2: Judgement = ("B")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, (&&["A", "C", "D"] => "P")-*(1.0, 0.81))
    }

    func testConditional2() {
        let rule = Rules.deduction.conditional[2]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B", "C", "D"] => "P")-*
        let j2: Judgement = (&&["B", "C"])-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, (&&["A", "D"] => "P")-*(1.0, 0.81))
    }

    func testConditional3() {
        let rule = Rules.deduction.conditional[3]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B"] => "P")-*
        let j2: Judgement = ("C" => "B")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, (&&["A", "C"] => "P")-*(1.0, 0.81))
    }
    
    func testConditional4() {
        let rule = Rules.deduction.conditional[3]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B", "C", "D"] => "P")-*
        let j2: Judgement = ("X" => "B")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, (&&["A", "C", "D", "X"] => "P")-*(1.0, 0.81))
    }
    
    func testConditionalAbduction1() {
        let rule = Rules.abduction.conditional[2]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B"] => "P")-*
        let j2: Judgement = ("A" => "P")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, ("B")-*(1.0, 0.44751381215469616))
    }
    
    func testConditionalAbduction2() {
        let rule = Rules.abduction.conditional[2]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B", "C"] => "P")-*
        let j2: Judgement = ("A" => "P")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, (&&["B", "C"])-*(1.0, 0.44751381215469616))
    }
    
    func testConditionalAbduction3() {
        let rule = Rules.abduction.conditional[3]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B"] => "P")-*
        let j2: Judgement = (&&["A", "C"] => "P")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, ("C" => "B")-*(1.0, 0.44751381215469616))
    }
    
    func testConditionalAbduction4() {
        let rule = Rules.abduction.conditional[3]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B", "D"] => "P")-*
        let j2: Judgement = (&&["A", "C"] => "P")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, ("C" => &&["B", "D"])-*(1.0, 0.44751381215469616))
    }

    func testConditionalAbduction5() {
        let rule = Rules.abduction.conditional[3]

        print(rule.0)
        print(rule.1)
        print("----")
        print(rule.2)
        
        let j1: Judgement = (&&["A", "B", "D"] => "P")-*
        let j2: Judgement = (&&["A", "C", "E"] => "P")-*

        print("\n")
        print(j1)
        print(j2)
        print("----")
        
        let res = rule_applicator(rule)((j1, j2))!
        print(res)

        XCTAssertEqual(res, (&&["C", "E"] => &&["B", "D"])-*(1.0, 0.44751381215469616))
    }
    
    func testNavigate() {
        
        typealias Line = [Term]
        
        let empty: Line = [.NULL, .NULL, .NULL]

        let lines: [Line] = [
            [.NULL, .NULL, .NULL],
            [.NULL, .NULL, .NULL],
            [.NULL, .NULL, .NULL],
            [" • ", .NULL, .NULL],
            [.NULL, " • ", .NULL],
            [.NULL, .NULL, " • "],
        ]
        
        var screen: [Line] = [
            [" • ", .NULL, .NULL],
            [.NULL, " • ", .NULL],
            [.NULL, .NULL, " • "],
            [.NULL, .NULL, .NULL],
            [.NULL, .NULL, .NULL],
            [.NULL, .SELF, .NULL]
        ]
                
        let actions: [Term] = [" \\ ", .SELF, " / "]
        
        var lastAction: Term?
        
        narsy.register("move") {
            var args = $0
            let a1 = args.removeFirst()
            if args.isEmpty {
                lastAction = a1
                return .NULL
            }
            let a2 = args.removeFirst()
            if args.isEmpty {
                if a2 == "[forward]" {
                    lastAction = .SELF
                }
                if a2 == "[left]" {
                    lastAction = " \\ "
                }
                if a2 == "[right]" {
                    lastAction = " / "
                }
            }
            return .NULL
        }
        
        narsy.register("stay") { _ in
            lastAction = nil
            return .NULL
        }
        
        var lastLine: Term = .compound(.x, screen.last!)
        
        var home = 1
        var away = 1 {
            didSet {
//                usleep(110000)
//                narsy.perform("G")
//                if let op = lastAction {
//                    if op == .SELF {
//                        narsy.perform(.operation("move", [.SELF, "[forward]"]) >>|=> "G")
//                        narsy.perform((lastLine >>|=> .operation("move", [.SELF, "[forward]"])) >>|=> "G")
//                    }
//                    if op == " \\ " {
//                        narsy.perform(.operation("move", [.SELF, "[left]"]) >>|=> "G")
//                        narsy.perform((lastLine >>|=> .operation("move", [.SELF, "[left]"])) >>|=> "G")
//                    }
//                    if op == " / " {
//                        narsy.perform(.operation("move", [.SELF, "[right]"]) >>|=> "G")
//                        narsy.perform((lastLine >>|=> .operation("move", [.SELF, "[right]"])) >>|=> "G")
//                    }
//
//                }
                narsy.perform(("G")-!)

//                narsy.perform(.cycle(50))

            }
        }
        
        narsy.perform(("G")-!)
        
        let pos: Term = .var("x")
        narsy.perform(((" • " --> "[forward]") >>|=> .operation("move", [.SELF, "[forward]"])) >>|=> "G")
        narsy.perform(((" • " --> "[left]") >>|=> .operation("move", [.SELF, "[left]"])) >>|=> "G")
        narsy.perform(((" • " --> "[right]") >>|=> .operation("move", [.SELF, "[right]"])) >>|=> "G")

        
        while true {
            
//            let snapshot: Term = .compound(.x, screen.map({.compound(.x, $0)}))
            
            let snapshot: Term = .compound(.x, screen.last!)

            let ratio = "\(Double(away)/Double(home))".prefix(4)
            
            print("\(screen[0].line)\n\(screen[1].line)\n\(screen[2].line)\n\(screen[3].line)\n\(screen[4].line)\n\(screen[5].line) SCORE: \(away)  \(ratio) -- \(output.filter({ $0.contains("🤖") }).count)")
            
            
            let last = screen[5]
            
            if last[0] == " • " { home += 1 }
            if last[1] == " • " { home += 1 }
            if last[2] == " • " { home += 1 }
            
            if last[0] == " % " { away += 1 }
            if last[1] == " % " { away += 1 }
            if last[2] == " % " { away += 1 }
            
//            narsy.perform(||(.instance(snapshot) --> "ENV")-*)
//                        narsy.perform(.cycle(10))
            let pos: Term = {
                let idx = screen[5].firstIndex(where: { $0 == .SELF || $0 == " % " })!
                if idx == 0 {
                    if screen[4][0] == " • " {
                        return "[forward]"
                    }
                    return "[right]"
                }
                if idx == 1 {
                    if screen[4][0] == " • " {
                        return "[left]"
                    }
                    if screen[4][1] == " • " {
                        return "[forward]"
                    }
                    if screen[4][2] == " • " {
                        return "[right]"
                    }
                }
                if idx == 2 {
                    if screen[4][2] == " • " {
                        return "[forward]"
                    }
                    return "[left]"
                }
                return "[forward]"
            }()
            narsy.perform(" • " --> pos)
//            narsy.perform("G"-!)
            narsy.perform(.cycle(20))
            let action: Term
            if lastAction != nil {
                action = lastAction!
                lastAction = nil
            } else {
                action = actions.randomElement()!
                
                if action == .SELF {
//                    narsy.perform((snapshot >>|=> .operation("move", [.SELF, "[forward]"])))
                    _ = narsy.operations["move"]?([.SELF, "[forward]"])
//                    narsy.perform((.operation("move", [.SELF])))
                }
                if action == " \\ " {
//                    narsy.perform((snapshot >>|=> .operation("move", [.SELF, "[left]"])))
                    _ = narsy.operations["move"]?([.SELF, "[left]"])
//                    narsy.perform((.operation("move", [.SELF, "[left]"])))
                }
                if action == " / " {
//                    narsy.perform((snapshot >>|=> .operation("move", [.SELF, "[right]"])))
                    _ = narsy.operations["move"]?([.SELF, "[right]"])
//                    narsy.perform((.operation("move", [.SELF, "[right]"])))
                }
                
//                away += 1 // temp
                
//                usleep(150000)
//                narsy.perform(.cycle(10))
            }
            
            if screen[5][0] != .NULL && screen[5][0]  != " • " {
                screen[5][0] = action
            }
            
            if screen[5][1] != .NULL && screen[5][1]  != " • " {
                screen[5][1] = action
            }
            
            if screen[5][2] != .NULL && screen[5][2]  != " • " {
                screen[5][2] = action
            }
            
        
//            print("\(screen[0].line)\n\(screen[1].line)\n\(screen[2].line)\n\(screen[3].line)\n\(screen[4].line)\n\(screen[5].line) SCORE: \(away)  \(ratio)")
            
            
            
            if screen[5][0] == " / " {
                if screen[4][1] != .NULL {
                    screen[4][1] = " % "
                } else {
                    screen[4][1] = .SELF
                }
            }
            
            
            if screen[5][1] == " \\ " {
                if screen[4][0] != .NULL {
                    screen[4][0] = " % "
                } else {
                    screen[4][0] = .SELF
                }
            }
            
            if screen[5][1] == " / " {
                if screen[4][2] != .NULL {
                    screen[4][2] = " % "
                } else {
                    screen[4][2] = .SELF
                }
            }
            
            
            if screen[5][2] == " \\ " {
                if screen[4][1] != .NULL {
                    screen[4][1] = " % "
                } else {
                    screen[4][1] = .SELF
                }
            }
            
            
            
            if screen[5][0] == .SELF || screen[5][0] == " \\ " || screen[5][0] == " % " {
                if screen[4][0] != .NULL {
                    screen[4][0] = " % "
                } else {
                    screen[4][0] = .SELF
                }
            }
            
            if screen[5][1] == .SELF || screen[5][1] == " % " {
                if screen[4][1] != .NULL {
                    screen[4][1] = " % "
                } else {
                    screen[4][1] = .SELF
                }
            }
            if screen[5][2] == .SELF || screen[5][2] == " / " || screen[5][2] == " % " {
                if screen[4][2] != .NULL {
                    screen[4][2] = " % "
                } else {
                    screen[4][2] = .SELF
                }
            }
            
            
            
            lastLine = .compound(.x, screen.removeLast())
            
            if screen[0] != empty {
                screen.insert(empty, at: 0)
            } else {
                screen.insert(lines.randomElement()!, at: 0)
            }
            
//            if away > 2 {
//                print(narsy.memory)
//                print(narsy.buffer)
//                break
//            }
            
//            usleep(200000)
        }
    }
    
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
            .cycle(40)
//            ||("ball" --> "[left]")-*,
////            .cycle(100),
//            ||("ball" --> "[center]")-*,
////            .cycle(10),
//            ||("G")-*,
//            ("G")-!,
//            .cycle(20),
//            ||("ball" --> "[center]")-*,
////            ||(("ball" --> "[left]") >>|=> (.operation("move", ["left"])))-*,
////               ||("G")-*,
////              .cycle(10),
//            ||("ball" --> "[right]")-*,
//            ||("ball" --> "[right]")-*,
//            ||("ball" --> "[right]")-*,
//            .cycle(10),
//            ||("ball" --> "[right]")-*,
//            ||("ball" --> "[right]")-*,
//            .cycle(40)
        )
        
        outputMustContain("🤖 ^move SELF [left]")
        outputMustContain("🤖 ^move SELF [right]")
    }
    
    
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

extension Array<Term> {
    var line: String {
//        String(
            "💱\(self[0])\(self[1])\(self[2])💱"
//            .prefix(10))
    }
}
