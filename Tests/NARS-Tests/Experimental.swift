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
            ("{Sandy}" --> "dog")-*
        )
    }
    
    func testSample() {
        narsy.perform(
            ("{tom}" --> "cat")-*,
            ("{tom}" --> ç.e_("likes", .º, "{sky}"))-*,
            ("{sky}" --> "[blue]")-*,
            ("[blue]" --> ç.e_("likes", "cat", .º))-?,
            .cycle(10000)
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
    
    func testPattern() {
        narsy.perform((*[*["1","0","0","0","0","0","0","0","0","0"], "left"] --> "is")-*)
        narsy.perform((*["1","0","0","0","0","0","0","0","0","0"] --> "left")-*)
        narsy.perform((*[*["1","1","0","0","0","0","0","0","0","0"], "left"] --> "is")-*)
        
        
        narsy.perform((*[*["0","0","0","0","0","0","0","0","0","1"], "right"] --> "is")-*)
        narsy.perform((*["0","0","0","0","0","0","0","0","0","1"] --> "right")-*)
        narsy.perform((*[*["0","0","0","0","0","0","1","0","1","1"], "right"] --> "is")-*)
        
        
        narsy.perform((*["1","1","1","0","0","0","0","0","0","0"] --> "left")-?)
        narsy.perform(.cycle(500))
        narsy.perform((*["0","0","0","0","0","0","1","0","1","0"] --> "right")-?)
        narsy.perform(.cycle(500))
        //        nars.perform((*["0","0","0","0","0","0","0","1","1","1"] --> "right")-?)
        //        nars.perform(.cycle(200))
        //        nars.perform((*["0","0","0","0","0","0","0","1","1","1"] --> "right")-?)
        
        outputMustContain("💡 <(⨯ 1 1 1 0 0 0 0 0 0 0) -> left>.")
        outputMustContain("💡 <(⨯ 0 0 0 0 0 0 1 0 1 0) -> right>.")
    }
    
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
        let image = "C" --> ç.e_("represent", .º, "subset")

        let knowledge = *[.var("x"), "C", .var("y")] --> ç.e_("represent", .º, (*[.var("x"), .var("y")] --> "subset"))
        
        narsy.perform(
            image-*,
            knowledge-*,
            
            .cycle(50),
            (*["dog", "C", "animal"] --> ç.e_("represent", .º, "?"))-?,
//                        .cycle(50),
//            (*["dog", "C", "animal"] --> ç.e_("represent", .º, *["dog", "animal"] --> "subset"))-?,
            .cycle(100)
        )
//                print(narsy.memory)
//        print(
//            Term.match(t: *["dog", "C", "animal"] --> ç.e_("represent", .º, "?"), s: *["dog", "C", "animal"] --> ç.e_("represent", .º, "?"))
//        )
        outputMustContain("💡 <(⨯ dog C animal) -> (/ represent º (dog ⨯ animal) -> subset)>.")
    }
    
    func testLogicMatch() {
        let res = Term.logic_match(t1: *["dog", "C", "animal"], t2: *["$x", "C", "$y"])
        XCTAssertTrue(res)
    }
    
    func testLookup() {
        narsy.perform(
            (("dog" --> "$x") => ("$x" --> "[live]"))-*,
            ("dog" --> "animal")-*,
            .cycle(10)
        )
    }
    
    func testSimpleQuestion() {
        narsy.perform(
            ("dog" --> "animal")-*,
            ("dog" --> "?")-?
        )
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
            
            (*["kvar", "a"])-*,
            
//            (*["kvar", "a"] --> "?")-?, // should derive kvar,a --> [kvar] -or- kvar,a --> __(a, kvar)
            
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
                .cycle(20)
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
            ("G")-!, // TODO: make goals sticky so they're recurring
            (("ball" --> "[left]") >>|=> (.operation("move", ["[left]"]) >>|=> "G"))-*,
            ||("ball" --> "[left]")-*,
            .cycle(20)
        )
        outputMustContain("🤖 ^move [left]")
//        print(nars.memory)
    }
}
